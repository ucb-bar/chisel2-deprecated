/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

package Chisel

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.{Queue => ScalaQueue}
import scala.math.pow

object addPin {
  def apply(m: Module, pin: Data, name: String) {
    for ((n, io) <- pin.flatten) {
      // assign component
      io.component = m
      io.isIo = true
    }
    // set name
    pin nameIt (name, true)
    // included in io
    (m.io) match {
      case io: Bundle => io += pin
    }
  }
}

object wire {
  def apply(pair: (Node, Data)) {
    val input = pair._1
    val consumer = pair._2
    if (consumer.inputs.isEmpty) consumer.inputs += input
    else consumer.inputs(0) = input
  }
}

// DaisyTransform inserts the step counter and
// the snapshot and evenet counter pins
object DaisyTransform {
  var top: Module = null

  val stepsIn = Decoupled(UInt(width = 32)).flip
  val snapOut = Decoupled(UInt(width = 32))
  val snapCtrl = UInt(INPUT, 1)
  val cntrOut = Decoupled(UInt(width = 32))
  val cntrCtrl = UInt(INPUT, 1)

  val fires = new HashMap[Module, Bool]
  val snapIns = new HashMap[Module, UInt]
  val snapOuts = new HashMap[Module, DecoupledIO[UInt]]
  val snapCtrls = new HashMap[Module, Bits]
  val snapCopies = new HashMap[Module, Bool]
  val snapReads = new HashMap[Module, Bool]
  val cntrIns = new HashMap[Module, UInt]
  val cntrOuts = new HashMap[Module, DecoupledIO[UInt]]
  val cntrCtrls = new HashMap[Module, Bits]
  val cntrCopies = new HashMap[Module, Bool]
  val cntrReads = new HashMap[Module, Bool]

  val states = new ArrayBuffer[Node]

  def apply[T <: Module](c: => T) = {
    top = Module(c)

    // clock counters
    val steps = Reg(init = UInt(0, 32))
    val fired = steps.orR
    val notFired = !fired

    addReg(top, steps, "steps",
      (notFired && stepsIn.valid) -> stepsIn.bits,
      fired                       -> (steps - UInt(1))
    )

    ChiselError.info("[DaisyTransform] add daisy pins")

    addPin(top, stepsIn, "steps_in")
    addPin(top, snapOut, "snap_out")
    addPin(top, snapCtrl, "snap_ctrl")
    addPin(top, cntrOut, "cntr_out")
    addPin(top, cntrCtrl, "cntr_ctrl")
    
    wire(notFired -> stepsIn.ready)
    fires(top)     = fired
    snapIns(top)   = UInt(0)
    snapOuts(top)  = snapOut
    snapCtrls(top) = snapCtrl
    cntrIns(top)   = UInt(0)
    cntrOuts(top)  = cntrOut
    cntrCtrls(top) = cntrCtrl

    val snapFire = addTypeNode(top, snapOut.ready && notFired, "snap_fire")
    val snapCopy = addTypeNode(top, snapFire && snapCtrl === Bits(0), "snap_copy")
    val snapRead = addTypeNode(top, snapFire && snapCtrl === Bits(1), "snap_read")
    val cntrFire = addTypeNode(top, cntrOut.ready && notFired, "cntr_fire")
    val cntrCopy = addTypeNode(top, cntrFire && cntrCtrl === Bits(0), "cntr_copy")
    val cntrRead = addTypeNode(top, cntrFire && cntrCtrl === Bits(1), "cntr_read")
    snapCopies(top) = snapCopy
    snapReads(top)  = snapRead
    cntrCopies(top) = cntrCopy
    cntrReads(top)  = cntrRead
    wire((snapFire && notFired) -> snapOut.valid)
    wire((cntrFire && notFired) -> cntrOut.valid)
    
    top.children foreach (addDaisyPins(_))
    top.asInstanceOf[T]
  }


  def addDaisyPins (c: Module) = {
    /* insert pins */
    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      val fire = Bool(INPUT)
      val snapIn = UInt(INPUT, 32)
      val snapOut = Decoupled(UInt(width = 32))
      val snapCtrl = UInt(INPUT, 1)
      val cntrIn = UInt(INPUT, 32)
      val cntrOut = Decoupled(UInt(width = 32))
      val cntrCtrl = UInt(INPUT, 1)
      addPin(m, fire,   "fire")
      addPin(m, snapIn, "snap_in")
      addPin(m, cntrIn, "cntr_in")
      addPin(m, snapOut,  "snap_out")
      addPin(m, snapCtrl, "snap_ctrl")
      addPin(m, cntrOut,  "cntr_out")
      addPin(m, cntrCtrl, "cntr_ctrl")
      fires(m)     = fire
      snapIns(m)   = snapIn
      snapOuts(m)  = snapOut
      snapCtrls(m) = snapCtrl
      cntrIns(m)   = cntrIn
      cntrOuts(m)  = cntrOut
      cntrCtrls(m) = cntrCtrl
      wire(snapCtrls(m.parent) -> snapCtrl)
      wire(snapOuts(m.parent).ready -> snapOut.ready)

      val snapFire = addTypeNode(m, snapOut.ready && !fire, "snap_fire")
      val snapCopy = addTypeNode(m, snapFire && snapCtrl === Bits(0), "snap_copy")
      val snapRead = addTypeNode(m, snapFire && snapCtrl === Bits(1), "snap_read")
      val cntrFire = addTypeNode(m, cntrOut.ready && !fire, "cntr_fire")
      val cntrCopy = addTypeNode(m, cntrFire && cntrCtrl === Bits(0), "cntr_copy")
      val cntrRead = addTypeNode(m, cntrFire && cntrCtrl === Bits(1), "cntr_read")
      snapCopies(m) = snapCopy
      snapReads(m)  = snapRead
      cntrCopies(m) = cntrCopy
      cntrReads(m)  = cntrRead
      wire((snapFire && !fire) -> snapOut.valid)
      wire((cntrFire && !fire) -> cntrOut.valid)

      // visit children
      m.children foreach (queue enqueue _)
    }
  }

  def addReg(m: Module, outType: Bits, name: String, updates: (Bool, Node)*) {
    val reg = outType.comp match {
      case r: Reg => r
    }
    // assign component
    reg.component = m
    // assign clock
    reg.clock = m.clock
    // assign name
    if (name != "") reg setName name
    // set enable signal
    reg.isEnable = !updates.isEmpty
    for ((cond, value) <- updates) {
      reg.enable = reg.enable || cond
    }
    // add updates
    reg.updates ++= updates
  }

  def addTypeNode[T <: Data](m: Module, typeNode: T, name: String) = {
    typeNode.getNode.component = m
    typeNode.getNode.inputs foreach (_.getNode.component = m)
    typeNode.getNode setName name
    typeNode
  }
}

trait DaisyChain extends Backend {
  override def backannotationTransforms {
    super.backannotationTransforms
    transforms += ((c: Module) => c bfs (_.addConsumers))
    transforms ++= daisyTransforms
    transforms += ((c: Module) => c.addClockAndReset)
    transforms += ((c: Module) => gatherClocksAndResets)
    transforms += ((c: Module) => connectResets)
    transforms += ((c: Module) => c.inferAll)
    transforms += ((c: Module) => c.forceMatchingWidths)
    transforms += ((c: Module) => c.removeTypeNodes)
    transforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  }
  val daisyTransforms = ArrayBuffer(
    {(c: Module) => decoupleTarget(DaisyTransform.top)},
    {(c: Module) => appendFires(DaisyTransform.top)}
  )

  val ioBuffers = new HashMap[Node, Bits]
  val daisyNames = HashSet("steps", "fire",
    "snap_in", "snap_ctrl", "cntr_in", "cntr_ctrl",
    "steps_in_ready", "steps_in_valid", "steps_in_bits",
    "snap_out_ready", "snap_out_valid", "snap_out_bits",
    "cntr_out_ready", "cntr_out_valid", "cntr_out_bits",
    "snap_fire", "snap_copy", "snap_read",
    "cntr_fire", "cntr_copy", "cntr_read")

  def addReg(m: Module, outType: Bits, name: String, updates: (Bool, Node)*) {
    val reg = outType.comp match {
      case r: Reg => r
    }
    // assign component
    reg.component = m
    // assign clock
    reg.clock = m.clock
    // assign name
    if (name != "") reg setName name
    // set enable signal
    reg.isEnable = !updates.isEmpty
    for ((cond, value) <- updates) {
      reg.enable = reg.enable || cond
    }
    // add updates
    reg.updates ++= updates
    // genreate muxes
    reg genMuxes reg
    // assign reset
    if (reg.isReset) 
      reg.inputs += m.reset
  }

  def appendFires(c: Module) {
    ChiselError.info("[DaisyChain] append fire signals to Reg and Mem")

    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val top = queue.dequeue

      // Make all delay nodes be enabled by the fire signal
      for (node <- top.nodes ; if !(daisyNames contains node.name)) {
        node match {
          // For Reg, different muxes are generated by different backend
          case reg: Reg if this.isInstanceOf[VerilogBackend] && 
                           !Driver.isBackannotating => {
            val enable = DaisyTransform.fires(top) && reg.enable
            reg.inputs(reg.enableIndex) = enable.getNode
          }
          case reg: Reg => {
            reg.inputs(0) = Multiplex(DaisyTransform.fires(top), reg.next, reg)
          }
          case mem: Mem[_] => {
            for (write <- mem.writeAccesses) {
              val en = Bool()
              val newEn = DaisyTransform.fires(top) && en
              wire(write.inputs(1) -> en)
              write.inputs(1) = newEn
            }
          }
          case _ =>
        }
      }
    
      top.children foreach (queue enqueue _)
    }  
  }

  /* target decoupling */
  def decoupleTarget(c: Module) = {
    ChiselError.info("[DaisyChain] target decoupling")

    for ((name, io) <- c.io.asInstanceOf[Bundle].elements) {
      io nameIt (name, true)
    }
    // For the input and output pins of the c component
    // insert buffers so that their values are avaiable
    // after the target is stalled
    for ((name, targetPin) <- c.io.flatten; if !(daisyNames contains name)) {
      val bufName = name + "_buf"
      daisyNames += bufName
      ioBuffers(targetPin) = {
        val buf = Reg(UInt())
        if (targetPin.dir == INPUT) {
          // Input buffers work when the clock counter value is set
          addReg(c, buf, bufName, 
            DaisyTransform.stepsIn.valid -> targetPin)
          for (consumer <- targetPin.consumers) {
            val idx = consumer.inputs indexOf targetPin
            consumer.inputs(idx) = buf
          }
        } else if (targetPin.dir == OUTPUT) {
          addReg(c, buf, bufName, 
            DaisyTransform.fires(c) -> targetPin.inputs.head)
          wire(buf -> targetPin)
        }
        buf
      }
    }
  }
}

trait SnapshotChain extends DaisyChain {
  daisyTransforms += { c => findStates(DaisyTransform.top) }
  daisyTransforms += { c => genSnapshotChain(DaisyTransform.top) }

  var snapIdx = -1
  def emitSnapIdx = {
    snapIdx = snapIdx + 1
    snapIdx
  }

  def findStates(c: Module) { 
    ChiselError.info("[SnapshotChain] find state elements")
    /*** collect state elements for snapshotting ***/
    // First, collect the top component's inputs
    for ((name, targetPin) <- c.io.flatten ; 
      if !(daisyNames contains name) && targetPin.dir == INPUT) {
        c.states += targetPin
      }

    // Second, collect all the state elements (Reg & Mem)
    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val top = queue.dequeue
      // collect registers and memory access
      top.nodes foreach { 
        _ match {
          case reg: Reg if !(daisyNames contains reg.name) =>
            top.states += reg
          case mem: Mem[_] =>
            for (i <- 0 until mem.size)
              top.states += (new MemRead(mem, UInt(i)))
          case _ =>
        }
      }
      // visit children
      top.children foreach (queue enqueue _)
    }
  }

  def genSnapshotChain(c: Module) {
    ChiselError.info("[SnapshotChain] generate snapshot chains")
 
    val queue = ScalaQueue(c)
    // Daisy chaining
    while (!queue.isEmpty) {
      val m = queue.dequeue
      val copy = DaisyTransform.snapCopies(m)
      val read = DaisyTransform.snapReads(m)
 
      m.states foreach { node =>
        node.snapShadow = Reg(UInt(width=32))
        node.snapIdx = emitSnapIdx
      }

      (m.children.isEmpty, m.states.isEmpty) match {
        // no children & no singals 
        case (true, true) => {
          // snap output <- snap input
          wire(DaisyTransform.snapIns(m) -> 
               DaisyTransform.snapOuts(m).bits)
        }
        // children but no signals
        case (false, true) => {
          // head child's snap output -> snap output
          wire(DaisyTransform.snapOuts(m.children.head).bits -> 
               DaisyTransform.snapOuts(m).bits)
          // snap input -> last child's snap input 
          wire(DaisyTransform.snapIns(m) -> 
               DaisyTransform.snapIns(m.children.last))
        }
        // no children but signals
        case (true, false) => {
          // snap output -> head shadow
          wire(m.states.head.snapShadow -> 
               DaisyTransform.snapOuts(m).bits)
          val state = ioBuffers getOrElse (m.states.last, m.states.last)
          // snap input -> last shadow 
          addReg(m, m.states.last.snapShadow, "snap_shadow_" + m.states.last.snapIdx,
            copy -> state,
            read -> DaisyTransform.snapIns(m))
        }
        // children & signals
        case (false, false) => {
          // snap output <- head shadow
          wire(m.states.head.snapShadow ->
               DaisyTransform.snapOuts(m).bits)
          val state = ioBuffers getOrElse (m.states.last, m.states.last)
          // last shadow <- snap input
          addReg(m, m.states.last.snapShadow, "snap_shadow_" + m.states.last.snapIdx,
            copy -> state,
            read -> DaisyTransform.snapOuts(m.children.head).bits)
          // last child's snap input <- snap input
          wire(DaisyTransform.snapIns(m) ->
               DaisyTransform.snapOuts(m.children.last).bits)
        }
      }
 
      for (s <- m.children sliding 2 ; if s.size == 2) {
        val cur = s.head
        val next = s.last
        // next child's daisy output -> cur child's daisy input
        wire(DaisyTransform.snapOuts(next).bits -> 
             DaisyTransform.snapIns(cur))
      }

      for (s <- m.states sliding 2 ; if s.size == 2) {
        val cur = s.head
        val next = s.last
        val state = ioBuffers getOrElse (cur, cur)
        /****** Shaodw Counter *****/
        // daisy_ctrl == 'copy' -> current state
        // daisy_ctrl == 'read' -> next shadow 
        addReg(m, cur.snapShadow, "snap_shadow_" + cur.snapIdx,
          copy -> state,
          read -> next.snapShadow
        )
      }
      // collect signal lists
      DaisyTransform.states ++= m.states
      // visit children
      m.children foreach (queue enqueue _)
    }
  }  
}

class SnapshotVerilog extends VerilogBackend with SnapshotChain
class SnapshotCpp     extends CppBackend     with SnapshotChain

abstract class DaisyTester[+T <: Module](c: T, isTrace: Boolean = true) extends Tester(c, isTrace) {
  val peeks = new ArrayBuffer[BigInt]
  val stepsIn  = DaisyTransform.stepsIn
  val snapOut  = DaisyTransform.snapOut
  val snapCtrl = DaisyTransform.snapCtrl
  val states   = DaisyTransform.states

  // proceed 'n' clocks
  def takeSteps (n: Int) {
    val clk = emulatorCmd("step %d".format(n))
    if (isTrace) println("  STEP %d".format(n))
    delta += clk.toInt
  }

  def pokeSteps (n: Int) {
    // Wait until the clock counter is ready
    // (the target is stalled)
    while(peek(stepsIn.ready) == 0) {
      takeSteps(1)
    }
    // Set the clock counter
    poke(stepsIn.bits, n)
    poke(stepsIn.valid, 1)
    takeSteps(1)
    poke(stepsIn.valid, 0)
  }

  // Show me the current status of the daisy chain
  def showCurrentSnapChain() = {
    if (isTrace && !states.isEmpty) {
      println("--- CURRENT CHAIN ---")
      states foreach (x => peek(x.snapShadow))
      println("---------------------")
    }
  }

  def snapCopy() {
    do {
      poke(snapCtrl, 0)
      poke(snapOut.ready, 1)
      takeSteps(1)
    } while (peek(snapOut.valid) == 0)
    poke(snapOut.ready, 0)
    showCurrentSnapChain()
  }

  def snapRead() = {
    do {
      poke(snapCtrl, 1)
      poke(snapOut.ready, 1)
      takeSteps(1)
    } while (peek(snapOut.valid) == 0)
    poke(snapOut.ready, 0)
    showCurrentSnapChain()
    peek(snapOut.bits)
  }

  def snapCheck(expected: BigInt) {
    expect(snapOut.bits, expected)
  }

  override def dump(): Snapshot = {
    val snap = new Snapshot(t)

    if (isTrace) println("*** Snapshot Chain Copy ***")
    // Copy activity counter values to shadow counters
    snapCopy()

    for ((state, i) <- states.zipWithIndex) {
      if (isTrace) println("*** Snapshot Chain Read ***")
      // Read out the snap chain
      val snapValue = snapRead()
      // Check the snap output
      snapCheck(peeks(i))
      state match {
        case read: MemRead =>
          snap.pokes += Poke(read.mem, read.addr.litValue(0).toInt, snapValue)
        case _ =>
          snap.pokes += Poke(state, 0, snapValue)
      }
    }
    snap
  }

  override def step (n: Int = 1) { 
    if (isTrace) {
      println("-------------------------")
      println("|  Snapshot Chain Step  |")
      println("-------------------------")
    }

    /*** Snapshotting! ***/
    if (t != 0) snapshot()
    val target = t + n

    // set clock register
    pokeSteps(n)

    // run the target until it is stalled
    if (isTrace) println("*** CYCLE THE TARGET ***")
    takeSteps(n)

    // read out signal values
    if (isTrace) println("*** READ STATE VALUES ***")
    peeks.clear
    peeks ++= states map {
      case read: MemRead =>
        peekBits(read.mem, read.addr.litValue(0).toInt)
      case signal =>
        peekBits(signal)
    }
    takeSteps(1)

    t += n
  }

  override def finish(): Boolean = {
    dumpSnapshots("%s.snapshots".format(c.name), snapshots)
    super.finish()
  }
}

abstract class AXISlave(val aw: Int = 5, val dw: Int = 32, val n: Int = 32 /* 2^aw */) extends Module {
  val io = new Bundle {
    val in = Decoupled(Bits(width = dw)).flip
    val out = Decoupled(Bits(width = dw))
    val addr = Bits(INPUT, aw)
  }

  def wen(i: Int) = io.in.valid && io.addr(log2Up(n)-1, 0) === UInt(i)
  def ren(i: Int) = io.out.ready && io.addr(log2Up(n)-1, 0) === UInt(i)
  val rdata = Vec.fill(n){Bits(width = dw)}
  val rvalid = Vec.fill(n){Bool()}
  val wready = Vec.fill(n){Bool()}

  io.in.ready  := wready(io.addr)
  io.out.valid := rvalid(io.addr)
  io.out.bits  := rdata(io.addr)
}

class DaisyFPGAWrapper[+T <: Module](c: => T) extends AXISlave(n = 16 /* 2^(aw - 1) */){
  val top      = DaisyTransform(c)
  val stepsIn  = DaisyTransform.stepsIn
  val snapOut  = DaisyTransform.snapOut
  val snapCtrl = DaisyTransform.snapCtrl
  val cntrOut  = DaisyTransform.cntrOut
  val cntrCtrl = DaisyTransform.cntrCtrl
  // write 4 => steps
  stepsIn.bits := io.in.bits
  stepsIn.valid := wen(4)
  wready(4) := stepsIn.ready
  // read 4 => snapchain
  rdata(4) := snapOut.bits
  rvalid(4) := snapOut.valid
  snapOut.ready := ren(4)
  // read 5 => cntrchain
  rdata(5) := cntrOut.bits
  rvalid(5) := cntrOut.valid
  cntrOut.ready := ren(5)
  // snap & cntr control bit <- MSB of addr
  snapCtrl := io.addr(aw-1) 
  cntrCtrl := io.addr(aw-1) 
}

abstract class DaisyWrapperTester[+T <: DaisyFPGAWrapper[_]](c: T, isTrace: Boolean = true) extends DaisyTester(c, isTrace) {
  // poke 'bits' to the address 'addr'
  def pokeAddr(addr: BigInt, bits: BigInt) {
    do {
      poke(c.io.addr, addr)
      takeSteps(1)
    } while (peek(c.io.in.ready) == 0)
    poke(c.io.in.bits, bits)
    poke(c.io.in.valid, 1)
    takeSteps(1)
    poke(c.io.in.valid, 0)
  }

  // peek the signal from the address 'addr'
  def peekAddr(addr: BigInt) = {
    do {
      poke(c.io.addr, addr)
      poke(c.io.out.ready, 1)
      takeSteps(1)
    } while (peek(c.io.out.valid) == 0)

    peek(c.io.out.bits)
  }

  // compare the signal value from the address 'addr' with 'expected'
  def expectAddr(addr: BigInt, expected: BigInt) = {
    do {
      poke(c.io.addr, addr)
      poke(c.io.out.ready, 1)
      takeSteps(1)
    } while (peek(c.io.out.valid) == 0)
   
    expect(c.io.out.bits, expected)
  }

  // poke 'n' clocks to the clock counter
  // whose address is 4
  override def pokeSteps (n: Int) {
    do {
      poke(c.io.addr, 4)
      takeSteps(1)
    } while (peek(c.io.in.ready) == 0)
 
    pokeBits(c.io.in.bits, n)
    pokeBits(c.io.in.valid, 1)
    takeSteps(1)
    pokeBits(c.io.in.valid, 0)
  }

  // read at 4 | 0 << 4 -> daisy copy
  override def snapCopy() {
    peekAddr(4)
    showCurrentSnapChain()
  }

  // read at 4 | 1 << 4 -> snap read
  override def snapRead() = {
    val snapValue = peekAddr(4 | 1 << 4)
    showCurrentSnapChain()
    snapValue
  }

  override def snapCheck(expected: BigInt) {
    expect(c.io.out.bits, expected)
  }
}

/*
object CounterTransform {
  val counterCopy = new HashMap[Module, Bool]
  val counterRead = new HashMap[Module, Bool]
}

trait CounterTransform extends Backend {
  val decoupledPins = new HashMap[Node, Bits]

  var counterIdx = -1

  override def backannotationTransforms {
    super.backannotationTransforms

    transforms += ((c: Module) => c bfs (_.addConsumers))

    transforms += ((c: Module) => appendFires(c))
    transforms += ((c: Module) => connectDaisyPins(c))
    transforms += ((c: Module) => generateCounters(c))
    transforms += ((c: Module) => generateDaisyChains(c))

    transforms += ((c: Module) => c.addClockAndReset)
    transforms += ((c: Module) => gatherClocksAndResets)
    transforms += ((c: Module) => connectResets)
    transforms += ((c: Module) => c.inferAll)
    transforms += ((c: Module) => c.forceMatchingWidths)
    transforms += ((c: Module) => c.removeTypeNodes)
    transforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  }


  // Connect daisy pins of hierarchical modules
  def connectDaisyPins(c: Module) {
    ChiselError.info("[CounterBackend] connect daisy pins")

    val queue = new scala.collection.mutable.Queue[Module]
    queue enqueue c

    while (!queue.isEmpty) {
      val m = queue.dequeue

      // add daisy pins and wire them to its parent's daisy pins
      val daisyIn = UInt(INPUT, 32)
      val daisyOut = Decoupled(UInt(width = 32))
      val daisyCtrl = UInt(INPUT, 1)
      val daisyFire = daisyOut.ready && !firedPins(m)
      val copy = daisyFire && daisyCtrl === Bits(0)
      val read = daisyFire && daisyCtrl === Bits(1)
      copy.getNode setName "copy"
      read.getNode setName "read"
  
      addPin(m, daisyOut,  "daisy_out")
      addPin(m, daisyCtrl, "daisy_ctrl")
      if (m != c) {
        addPin(m, daisyIn, "daisy_in")
        wire(daisyOut.ready, daisyOuts(m.parent).ready)
      }
      wire(daisyOut.valid,   daisyFire)

      // The top component has no daisy input
      daisyIns(m)    = if (m ==c) UInt(0) else daisyIn
      daisyOuts(m)   = daisyOut
      daisyCtrls(m)  = daisyCtrl
      counterCopy(m) = copy
      counterRead(m) = read

      // visit children
      m.children map (queue enqueue _)
   }

    queue enqueue c

    while (!queue.isEmpty) {
      val m = queue.dequeue

      if (!m.children.isEmpty) {
        val head = m.children.head
        val last = m.children.last

        // If the component has its children and no signals for counters,
        // its first child's daisy output is connected to its daisy output
        if (m.signals.isEmpty) {
          if (m == c) {
            // For the top component, the shaodw buffer is inserted
            // between the outpins 
            // so that the first counter value would not be missed
            // when shadow counter values are shifted
            val buf = Reg(next = daisyOuts(head).bits)
            addReg(m, buf, "shadow_buf")
            wire(daisyOuts(m).bits, buf)            
          } else {
            // Otherwise, just connect them
            wire(daisyOuts(m).bits, daisyOuts(head).bits)
          }
        }

        for (i <- 0 until m.children.size - 1) {
          val cur = m.children(i)
          val next = m.children(i+1)
          // the current child's daisy input <- the next child's daisy output
          wire(daisyIns(cur), daisyOuts(next).bits)
        }

        // the last child's daisy input <- the module's diasy input
        wire(daisyIns(last), daisyIns(m))
      } else if (m.signals.isEmpty) {
        // No children & no singals for counters
        // the daisy output <- the daisy input
        wire(daisyOuts(m).bits, daisyIns(m))
      }

      m.children map (queue enqueue _)
    }
  }

  def generateCounters (c: Module) {
    ChiselError.info("[CounterBackend] generate counters")

    val queue = new scala.collection.mutable.Queue[Module]
    queue enqueue c
 
    while (!queue.isEmpty) {
      val m = queue.dequeue

      for (signal <- m.signals) {
        val signalWidth = signal.width
        val signalValue = 
          if (decoupledPins contains signal) decoupledPins(signal) 
          else UInt(signal)
        // Todo: configurable counter width
        val counter = Reg(Bits(width = 32))
        val shadow  = Reg(Bits(width = 32))
        signal.counter = counter
        signal.shadow = shadow
        signal.cntrIdx = emitCounterIdx

        val counterValue = {
          // Signal
          if (signalWidth == 1) {
            counter + signalValue
          // Bus -> hamming distance
          } else {
            val buffer = Reg(UInt(width = signalWidth))
            val xor = signalValue ^ buffer
            xor.inferWidth = (x: Node) => signalWidth
            val hd = PopCount(xor)
            addReg(m, buffer, "buffer_%d".format(signal.cntrIdx), 
              Map(firedPins(m) -> signalValue)
            )
            counter + hd
          }
        }
        counterValue.getNode.component = m
        counterValue.getNode setName "c_value_%d".format(signal.cntrIdx)

        /****** Activity Counter *****/
        // 1) fire signal -> increment counter
        // 2) 'copy' control signal when the target is stalled -> reset
        addReg(m, counter, "counter_%d".format(signal.cntrIdx), Map(
          firedPins(m)   -> counterValue,
          counterCopy(m) -> Bits(0)
        ))

        // for debugging
        if (Driver.isBackannotating) {
          signal setName "signal_%d_%s".format(counterIdx, signal.pName)
        }
      }

      // visit children
      m.children map (queue enqueue _)
    }
  }  
 
  def generateDaisyChains(c: Module) {
    ChiselError.info("[CounterBackend] daisy chaining")
 
    val queue = new scala.collection.mutable.Queue[Module] 
    queue enqueue c

    // Daisy chaining
    while (!queue.isEmpty) {
      val m = queue.dequeue 
      if (!m.signals.isEmpty) {
        val head = m.signals.head
        val last = m.signals.last
        if (m == c) {
          // For the top component, the shaodw buffer is inserted
          // at the frontend of the daisy chain
          // so that the first counter value would not be missed
          // when shadow counter values are shifted
          val buf = Reg(next = head.shadow)
          addReg(m, buf, "shadow_buf")
          wire(daisyOuts(m).bits, buf)
        } else {
          // Ohterwise, just connect them
          wire(daisyOuts(m).bits, head.shadow)
        }

        for (s <- m.signals.sliding(2)) {
          val cur = s.head
          val next = s.last
          /****** Shaodw Counter *****/
          // 1) 'copy' control signal -> copy counter values from the activity counter
          // 2) 'read' control signal -> shift counter values from the next shadow counter
          addReg(m, cur.shadow, "shadow_%d".format(cur.cntrIdx), Map(
            counterCopy(m) -> cur.counter,
            counterRead(m) -> next.shadow
          ))
      
          // Signals are collected in order
          // so that counter values are verified 
          // and power numbers are calculated
          Driver.signals += cur
        }

        // For the last counter of the daisy chain
        // 1) the module has chilren -> its first child's daisy output
        // 2) otherwise -> its daisy input 
        val lastread = 
          if (m.children.isEmpty) daisyIns(m) 
          else daisyOuts(m.children.head).bits
        addReg(m, last.shadow, "shadow_%d".format(last.cntrIdx), Map(
          counterCopy(m) -> last.counter,
          counterRead(m) -> lastread
        ))

        Driver.signals += m.signals.last
      }

      // visit children
      m.children map (queue enqueue _)
    }
  }
}

class CounterCppBackend extends CppBackend with CounterTransform
class CounterVBackend extends VerilogBackend with CounterTransform


abstract class CounterTester[+T <: Module](c: T, isTrace: Boolean = true) extends Tester(c, isTrace) {
  val prevPeeks = new HashMap[Node, BigInt]
  val counts = new HashMap[Node, BigInt]

  // compute the hamming distance of 'a' and 'b'
  def calcHD(a: BigInt, b: BigInt) = {
    var xor = a ^ b
    var hd: BigInt = 0
    while (xor > 0) {
      hd = hd + (xor & 1)
      xor = xor >> 1
    }
    hd
  }

  // proceed 'n' clocks
  def clock (n: Int) {
    val clk = emulatorCmd("clock %d".format(n))
    if (isTrace) println("  CLOCK %s".format(clk))
  }

  override def reset(n: Int = 1) {
    super.reset(n)
    if (t >= 1) {
      // reset prevPeeks
      for (signal <- Driver.signals ; if signal.width > 1) {
        prevPeeks(signal) = 0
      }
    }
  }

  // proceed 'n' clocks
  def pokeClks (n: Int) {
    val clks = c.io("clks") match {
      case dio: DecoupledIO[_] => dio
    }
    val fire = c.io("fire") match {
      case bool: Bool => bool
    }
    // Wait until the clock counter is ready
    // (the target is stalled)
    while(peek(clks.ready) == 0) {
      clock(1)
    }
    // Set the clock counter
    pokeBits(clks.bits, n)
    pokeBits(clks.valid, 1)
    clock(1)
    pokeBits(clks.valid, 0)
  }

  // i = 0 -> daisy copy
  // i = 1 -> daisy read 
  def peekDaisy (i: Int) {
    val daisyCtrl = c.io("daisy_ctrl") match {
      case bits: Bits => bits
    }
    val daisyOut = c.io("daisy_out") match {
      case dio: DecoupledIO[_] => dio
    }
    // request the daisy output
    // until it is valid
    do {
      poke(daisyCtrl, i)
      poke(daisyOut.ready, 1)
      clock(1)
    } while (peek(daisyOut.valid) == 0)
    poke(daisyOut.ready, 0)
  }

  // Do you believe the diasy output
  def checkDaisy(count: BigInt) {
    val daisyOut = c.io("daisy_out") match {
      case dio: DecoupledIO[_] => dio
    }
    val daisyOutBits = daisyOut.bits match {
      case bits: Bits => bits
    }
    expect(daisyOutBits, count)
  }

  // Show me the current status of the daisy chain
  def showCurrentChain {
    if (isTrace) {
      println("--- CURRENT CHAIN ---")
      for (s <- Driver.signals) {
        peek(s.shadow)
      }
      println("---------------------")
    }
  }

  override def step (n: Int = 1) { 
    if (isTrace) {
      println("-------------------------")
      println("| Counter Strcture Step |")
      println("-------------------------")
    }

    for (signal <- Driver.signals) {
      counts(signal) = 0
    }

    // set clock register
    pokeClks(n)

    // run the target until it is stalled
    if (isTrace) println("*** RUN THE TAREGT / READ SIGNAL VALUES ***")
    for (i <- 0 until n) {
      for (signal <- Driver.signals) {
        val curPeek = peekBits(signal)
        if (signal.width == 1) {
          // increment by the signal's value
          counts(signal) += curPeek
        } else {
          // increment by the hamming distance
          counts(signal) += calcHD(curPeek, prevPeeks(signal))
          prevPeeks(signal) = curPeek
        }
      }
      clock(1)
    }

    // Check activity counter values 
    if (isTrace) println("*** CHECK COUNTER VALUES ***")
    for (signal <- Driver.signals) {
      expect(signal.counter, counts(signal))
    }

    if (isTrace) println("*** Daisy Copy ***")
    // Copy activity counter values to shadow counters
    peekDaisy(0)
    showCurrentChain

    for (signal <- Driver.signals) {
      if (isTrace) println("*** Daisy Read ***")
      // Read out the daisy chain
      peekDaisy(1)
      showCurrentChain
      // Check the daisy output
      checkDaisy(counts(signal))
    }

    t += n
  }

  // initialization
  for (signal <- Driver.signals ; if signal.width > 1) {
    prevPeeks(signal) = 0
  }
}

// Counter backend, which deals with FPGA Counter wrappers
trait CounterWrapperBackend extends CounterBackend {
  override def getPseudoPath(c: Module, delim: String = "/"): String = {
    if (!(c.parent == null)) {
      c.parent match {
        case _: CounterWrapper => extractClassName(c)
        case _ => getPseudoPath(c.parent, delim) + delim + c.pName
      }
    } else ""
  }

  override def setPseudoNames(c: Module) {
    c match {
      case m: CounterWrapper => super.setPseudoNames(m.top)
    }
  }

  override def decoupleTarget(c: Module) {
    c match {
      case m: CounterWrapper => {
        super.decoupleTarget(m.top)
        // write 4 => clks
        val clks = m.top.io("clks") match {
          case dio: DecoupledIO[_] => dio
        }
        val fire = m.top.io("fire") match {
          case bool: Bool => bool
        }
        wire(clks.bits,  m.io.in.bits)
        wire(clks.valid, m.wen(4))
        wire(m.wready(4), clks.ready)
      }
    }
  }

  override def connectDaisyPins(c: Module) {
    c match {
      case m: CounterWrapper => {
        super.connectDaisyPins(m.top)
        // read 4 => daisy outputs
        wire(m.rdata(4),  daisyOuts(m.top).bits)
        wire(m.rvalid(4), daisyOuts(m.top).valid)
        wire(daisyOuts(m.top).ready, m.ren(4))
        val daisyCtrlBits = m.io("addr") match {
          case bits: Bits => 
            if (m.conf.daisyCtrlWidth == 1) bits(m.conf.addrWidth - 1)
            else bits(m.conf.addrWidth - 1, m.conf.addrWidth - m.conf.daisyCtrlWidth)
        }
        wire(daisyCtrls(m.top), daisyCtrlBits)        
      }
    }
  }
 
  override def generateCounters(c: Module) {
    c match {
      case m: CounterWrapper => super.generateCounters(m.top)
    }
  }

  override def generateDaisyChains(c: Module) {
    c match {
      case m: CounterWrapper => super.generateDaisyChains(m.top)
    }
  }
}

class CounterWBackend extends CppBackend with CounterWrapperBackend
class CounterFPGABackend extends FPGABackend with CounterWrapperBackend

case class CounterConfiguration(
  addrWidth: Int = 5,
  dataWidth: Int = 32,
  daisyCtrlWidth: Int = 1,
  counterWidth: Int = 32) 
{
  val n = pow(2, addrWidth - daisyCtrlWidth).toInt
}

// IO port connected to AXI bus
class CounterWrapperIO(conf: CounterConfiguration) extends Bundle {
  val in = Decoupled(Bits(width = conf.dataWidth)).flip
  val out = Decoupled(Bits(width = conf.dataWidth))
  val addr = Bits(INPUT, conf.addrWidth)
}

// FPGA Counter Wrapper
// 'top' module should be specified
abstract class CounterWrapper(val conf: CounterConfiguration) extends Module {
  val io = new CounterWrapperIO(conf)
  def top: Module

  def wen(i: Int) = io.in.valid && io.addr(log2Up(conf.n)-1, 0) === UInt(i)
  def ren(i: Int) = io.out.ready && io.addr(log2Up(conf.n)-1, 0) === UInt(i)
  val rdata = Vec.fill(conf.n){Bits(width = conf.dataWidth)}
  val rvalid = Vec.fill(conf.n){Bool()}
  val wready = Vec.fill(conf.n){Bool()}

  io.in.ready  := wready(io.addr)
  io.out.valid := rvalid(io.addr)
  io.out.bits  := rdata(io.addr)
}

abstract class CounterWrapperTester[+T <: CounterWrapper](c: T, isTrace: Boolean = true) extends CounterTester(c, isTrace) {
  // poke 'bits' to the address 'addr'
  def pokeAddr(addr: BigInt, bits: BigInt) {
    do {
      poke(c.io.addr, addr)
      clock(1)
    } while (peek(c.io.in.ready) == 0)

    poke(c.io.in.bits, bits)
    poke(c.io.in.valid, 1)
    clock(1)
    poke(c.io.in.valid, 0)
  }

  // peek the signal from the address 'addr'
  def peekAddr(addr: BigInt) = {
    do {
      poke(c.io.addr, addr)
      poke(c.io.out.ready, 1)
      clock(1)
    } while (peek(c.io.out.valid) == 0)

    peek(c.io.out.bits)
  }

  // compare the signal value from the address 'addr' with 'expected'
  def expectAddr(addr: BigInt, expected: BigInt) = {
    do {
      poke(c.io.addr, addr)
      poke(c.io.out.ready, 1)
      clock(1)
    } while (peek(c.io.out.valid) == 0)
   
    expect(c.io.out.bits, expected)
  }

  // poke 'n' clocks to the clock cycle
  // whose address is 4
  override def pokeClks (n: Int) {
    do {
      poke(c.io.addr, 4)
      clock(1)
    } while (peek(c.io.in.ready) == 0)
 
    pokeBits(c.io.in.bits, n)
    pokeBits(c.io.in.valid, 1)
    clock(1)
    pokeBits(c.io.in.valid, 0)
  }

  // read at 4 | 0 << 4 -> daisy copy
  // read at 4 | 1 << 4 -> daisy read
  override def peekDaisy (i: Int) {
    do {
      pokeBits(c.io.addr, 4 | i << 4)
      pokeBits(c.io.out.ready, 1)
      clock(1)
    } while (peek(c.io.out.valid) == 0)
  }

  override def checkDaisy(count: BigInt) {
    expect(c.io.out.bits, count)
  }
}
*/
