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
import scala.math.pow

object DaisyChain {
  var top: Module = null
  var clks: DecoupledIO[UInt] = null
  var clksReg: Bits = null
  val fires = new HashMap[Module, Bool]
  val daisyIns = new HashMap[Module, UInt]
  val daisyOuts = new HashMap[Module, DecoupledIO[UInt]]
  val daisyCtrls = new HashMap[Module, Bits]
  val daisyCopy = new HashMap[Module, Bool]
  val daisyRead = new HashMap[Module, Bool]
}

trait DaisyChain extends Backannotation {
  val ioBuffers = new HashMap[Bits, Bits]
  Module.isBackannotating = true
  override def backannotationTransforms {
    super.backannotationTransforms

    transforms += ((c: Module) => c bfs (_.addConsumers))
    transforms += ((c: Module) => decouple(DaisyChain.top))
    transforms += ((c: Module) => appendFires(DaisyChain.top))
    transforms += ((c: Module) => genCounters(DaisyChain.top))
    transforms += ((c: Module) => daisyChain(DaisyChain.top))

    transforms += ((c: Module) => c.addClockAndReset)
    transforms += ((c: Module) => gatherClocksAndResets)
    transforms += ((c: Module) => connectResets)
    transforms += ((c: Module) => c.inferAll)
    transforms += ((c: Module) => c.forceMatchingWidths)
    transforms += ((c: Module) => c.removeTypeNodes)
    transforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  }

  val daisyNames = HashSet(
    "clk_cntr", "clks_bits", "clks_valid", "clks_ready", 
    "daisy_out_bits", "daisy_out_valid", "daisy_out_ready",
    "daisy_buf", "daisy_ctrl"
  )                 

  var counterIdx = -1
  def emitCounterIdx = {
    counterIdx = counterIdx + 1
    counterIdx
  }

  def wire(pin: Data, input: Node) {
    if (pin.inputs.isEmpty) pin.inputs += input
    else pin.inputs(0) = input
  }

  def addReg(m: Module, outType: Bits, name: String = "", updates: Map[Bool, Node] = Map()) {
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

  override def annotateSignals(c: Module) {
   
    /*** collect signals for snapshotting ***/
    // First, collect the top component's inputs
    for ((name, targetPin) <- c.io.flatten ; 
      if !(daisyNames contains name) && targetPin.dir == INPUT) {
        c.counter(targetPin)
      }

    // Second, collect all the state elements (Reg & Mem)
    val queue = new scala.collection.mutable.Queue[Module]
    queue enqueue c
    while (!queue.isEmpty) {
      val top = queue.dequeue
      // collect registers and memory access
      top.nodes map { 
        _ match {
          case reg: Reg if !(daisyNames contains reg.name) =>
            top.counter(reg)
          case mem: Mem[_] =>
            for (i <- 0 until mem.size)
              top.counter(new MemRead(mem, UInt(i)))
          case _ =>
        }
      }
      // visit children
      top.children map (queue enqueue _)
    }
  }

  /* target decoupling */
  def decouple (c: Module) = {
    for ((name, io) <- c.io.asInstanceOf[Bundle].elements) {
      io nameIt (name, true)
    }

    // For the input and output pins of the top component
    // insert buffers so that their values are avaiable
    // after the target is stalled
    for ((name, targetPin) <- c.io.flatten ; if !(daisyNames contains name)) {
      val bufName = name + "_buf"
      ioBuffers(targetPin) = {
        val buf = Reg(UInt())
        if (targetPin.dir == INPUT) {
          addReg(c, buf, bufName,
            Map(!DaisyChain.fires(c) -> targetPin)
          )
          for (consumer <- targetPin.consumers) {
            val idx = consumer.inputs indexOf targetPin
            consumer.inputs(idx) = buf
          }
        } else if (targetPin.dir == OUTPUT) {
          addReg(c, buf, bufName,
            Map(DaisyChain.fires(c) -> targetPin.inputs.head)
          )
          wire(targetPin, buf)
        }
        buf
      }
      daisyNames += bufName
    }
  }


  def appendFires(c: Module) {
    ChiselError.info("[DaisyChain] append fire signals to Reg and Mem")

    val queue = new scala.collection.mutable.Queue[Module]
    queue enqueue c

    while (!queue.isEmpty) {
      val top = queue.dequeue

      // Make all delay nodes be enabled by the fire signal
      for (node <- top.nodes ; if !(daisyNames contains node.name)) {
        node match {
          // For Reg, different muxes are generated by different backend
          case reg: Reg if this.isInstanceOf[VerilogBackend] && 
                           !Module.isBackannotating => {
            val enable = DaisyChain.fires(top) && reg.enable
            reg.inputs(reg.enableIndex) = enable.getNode
          }
          case reg: Reg => {
            reg.inputs(0) = Multiplex(DaisyChain.fires(top), reg.next, reg)
          }
          case mem: Mem[_] => {
            for (write <- mem.writeAccesses) {
              val en = Bool()
              val newEn = DaisyChain.fires(top) && en
              wire(en, write.inputs(1))
              write.inputs(1) = newEn
            }
          }
          case _ =>
        }
      }
    
      top.children map (queue enqueue _)
    }  
  }

  def genCounters (c: Module) {
    ChiselError.info("[DaisyChain] generate activity counters")

    val queue = new scala.collection.mutable.Queue[Module]
    queue enqueue c
 
    while (!queue.isEmpty) {
      val top = queue.dequeue

      // activity counters <- { Reg, MemWrite }
      for (signal <- top.signals) {
        signal.cntrIdx = emitCounterIdx
        signal.counter = signal match {
          case reg:   Reg     => UInt(reg)
          case read:  MemRead => UInt(read)
          case input: Bits    => ioBuffers(input)
        }
        signal.shadow  = Reg(Bits(width = 32))
      }

      // visit children
      top.children map (queue enqueue _)
    }
  }  

  def daisyChain(c: Module) {
    ChiselError.info("[DaisyChain] daisy chaining")
 
    val queue = new scala.collection.mutable.Queue[Module] 
    queue enqueue c

    // Daisy chaining
    while (!queue.isEmpty) {
      val top = queue.dequeue
      val copy = DaisyChain.daisyCopy(top)
      val read = DaisyChain.daisyRead(top)

      (top.children.isEmpty, top.signals.isEmpty, c == top) match {
        // no children & no singals 
        case (true, true, _) => {
          // daisy output <- daisy input
          wire(DaisyChain.daisyOuts(top).bits, DaisyChain.daisyIns(top))
        }
        // children but no signals
        case (false, true, false) => {
          // daisy output <- head child's daisy output
          wire(DaisyChain.daisyOuts(top).bits, DaisyChain.daisyOuts(top.children.head).bits)
          // last child's daisy input <- daisy input
          wire(DaisyChain.daisyIns(top.children.last), DaisyChain.daisyIns(top))
        }
        // no children but signals
        case (true, false, false) => {
          // daisy output <- head shadow
          wire(DaisyChain.daisyOuts(top).bits, top.signals.head.shadow)
          // last shadow <- daisy input
          addReg(top, top.signals.last.shadow, "shadow_" + top.signals.last.cntrIdx,
            Map (
              copy -> top.signals.last.counter,
              read -> DaisyChain.daisyIns(top)
            )
          )
        }
        // children & signals
        case (false, false, false) => {
          // daisy output <- head shadow
          wire(DaisyChain.daisyOuts(top).bits, top.signals.head.shadow)
          // last shadow <- daisy input
          addReg(top, top.signals.last.shadow, "shadow_" + top.signals.last.cntrIdx,
            Map (
              copy -> top.signals.last.counter,
              read -> DaisyChain.daisyOuts(top.children.head).bits
            )
          )
          // last child's daisy input <- daisy input
          wire(DaisyChain.daisyOuts(top.children.last).bits, DaisyChain.daisyIns(top))
        }
        /*-------------------------------------*/
        /* Insert a daisy output buffer        */
        /* so that the head shadow can be read */
        /*-------------------------------------*/
        // children but no signals and top component
        case (false, true, true) => {
          // daisy buf <- head child's daisy output
          val daisyBuf = Reg(next = DaisyChain.daisyOuts(top.children.head).bits)
          addReg(top, daisyBuf, "daisy_buf")
          // daisy output <- daisy buf
          wire(DaisyChain.daisyOuts(top).bits, daisyBuf)
          // last child's daisy input <- daisy input
          wire(DaisyChain.daisyIns(top.children.last), DaisyChain.daisyIns(top))
        }
        // no children but signals and top component
        case (true, false, true) => {
          // daisy buffer <- head shadow
          val daisyBuf = Reg(next = top.signals.head.shadow)
          addReg(top, daisyBuf, "daisy_buf")
          // daisy output <- daisy buffer
          wire(DaisyChain.daisyOuts(top).bits, daisyBuf)
          // last shadow <- daisy input
          addReg(top, top.signals.last.shadow, "shadow_" + top.signals.last.cntrIdx,
            Map (
              copy -> top.signals.last.counter,
              read -> DaisyChain.daisyIns(top)
            )
          )
        }
        // children & signals and top coponent
        case (false, false, true) => {
          // daisy buffer <- head shadow
          val daisyBuf = Reg(next = top.signals.head.shadow)
          addReg(top, daisyBuf, "daisy_buf")
          // daisy output <- daisy buffer
          wire(DaisyChain.daisyOuts(top).bits, daisyBuf)
          // last shadow <- daisy input
          addReg(top, top.signals.last.shadow, "shadow_" + top.signals.last.cntrIdx,
            Map (
              copy -> top.signals.last.counter,
              read -> DaisyChain.daisyOuts(top.children.head).bits
            )
          )
          // last child's daisy input <- daisy input
          wire(DaisyChain.daisyOuts(top.children.last).bits, DaisyChain.daisyIns(top))
        }
      }

      
      for (s <- top.children sliding 2 ; if s.size == 2) {
        val cur = s.head
        val next = s.last
        // cur child's daisy input <- next child's daisy output
        wire(DaisyChain.daisyIns(cur), DaisyChain.daisyOuts(next).bits)
      }

      for (s <- top.signals sliding 2 ; if s.size == 2) {
        val cur = s.head
        val next = s.last
        /****** Shaodw Counter *****/
        // daisy_ctrl == 'copy' -> current counter
        // daisy_ctrl == 'read' -> next shadow 
        addReg(top, cur.shadow, "shadow_" + cur.cntrIdx,
          Map(
            copy -> cur.counter,
            read -> next.shadow
          )
        )
      }

      // collect signal lists
      Module.signals ++= top.signals
      // visit children
      top.children map (queue enqueue _)
    }
  }  
}

class DaisyVerilogBackend extends VerilogBackend with DaisyChain
class DaisyCppBackend extends CppBackend with DaisyChain

object DaisyTransform {
  def clks(c: Module): DecoupledIO[UInt] = {
    c.io("clks") match {
      case dio: DecoupledIO[UInt] => dio
    }
  }
  def daisyOut(c: Module): DecoupledIO[UInt] = {
    c.io("daisy_out") match {
      case dio: DecoupledIO[UInt] => dio
    }
  }

  def daisyCtrl(c: Module): Bits = {
    c.io("daisy_ctrl") match {
      case bits: Bits => bits
    }
  }

  def apply[T <: Module](c: => T) = {
    // clock counters
    val clks = Decoupled(UInt(width = 32)).flip
    val clksReg = Reg(UInt(width = 32))
    val fired = clksReg.orR
    val notFired = !fired
    val top: T = Module(c)
    DaisyChain.top = top
    DaisyChain.clks = clks
    DaisyChain.clksReg = clksReg
    DaisyChain.fires(DaisyChain.top) = fired
    DaisyChain.daisyIns(DaisyChain.top) = UInt(0)
    addPin(top, clks, "clks")
    clks.ready.inputs += notFired
    fired.getNode.component = top

    clksReg.comp match {
      case reg: Reg => {
        reg.component = top
        reg.isEnable  = true
        reg.enable    = fired || (clks.valid && notFired)
        reg.updates ++= Map (
          (notFired && clks.valid) -> clks.bits,
          fired                    -> (clksReg - UInt(1))
        )
      }
    }
    clksReg nameIt ("clk_cntr", false)

    insertDaisyPins(top)

    top
  }

  def addPin(m: Module, pin: Data, name: String) {
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

  def insertDaisyPins (c: Module) = {
    /* insert pins */
    val queue = new scala.collection.mutable.Queue[Module]
    queue enqueue c
    while (!queue.isEmpty) {
      val m = queue.dequeue
      val fire = Bool(INPUT)
      val daisyIn = UInt(INPUT, 32)
      val daisyOut = Decoupled(UInt(width = 32))
      val daisyCtrl = UInt(INPUT, 1)
      addPin(m, daisyOut, "daisy_out")
      addPin(m, daisyCtrl, "daisy_ctrl")
      DaisyChain.daisyOuts(m) = daisyOut
      DaisyChain.daisyCtrls(m) = daisyCtrl
      if (m != c) {
        addPin(m, fire, "fire")
        addPin(m, daisyIn, "daisy_in")
        DaisyChain.fires(m) = fire
        DaisyChain.daisyIns(m) = daisyIn
        fire.inputs += DaisyChain.fires(m.parent)
        daisyCtrl.inputs += DaisyChain.daisyCtrls(m.parent)
        daisyOut.ready.inputs += DaisyChain.daisyOuts(m.parent).ready
      } 

      val daisyFire = daisyOut.ready && !DaisyChain.fires(m)
      val copy = daisyFire && daisyCtrl === Bits(0)
      val read = daisyFire && daisyCtrl === Bits(1)
      daisyFire.getNode.component = m
      daisyFire.getNode.inputs map (_.getNode.component = m)
      copy.getNode.component = m
      read.getNode.component = m
      copy.getNode.inputs map (_.getNode.component = m)
      read.getNode.inputs map (_.getNode.component = m)
      copy.getNode setName "copy"
      read.getNode setName "read"
      DaisyChain.daisyCopy(m) = copy
      DaisyChain.daisyRead(m) = read
      daisyOut.valid.inputs += daisyFire && !DaisyChain.fires(m)

      // visit children
      m.children map (queue enqueue _)
    }
  }
}

abstract class DaisyTester[+T <: Module](c: T, isTrace: Boolean = true) extends Tester(c, isTrace) {
  val peeks = new ArrayBuffer[BigInt]

  // proceed 'n' clocks
  def clock (n: Int) {
    val clk = emulatorCmd("clock %d".format(n))
    if (isTrace) println("  CLOCK %s".format(clk))
    delta += clk.toInt
  }

  def pokeClks (n: Int) {
    val clks  = DaisyTransform.clks(c)
    // Wait until the clock counter is ready
    // (the target is stalled)
    while(peek(clks.ready) == 0) {
      clock(1)
    }
    // Set the clock counter
    poke(clks.bits, n)
    poke(clks.valid, 1)
    clock(1)
    poke(clks.valid, 0)
  }

  // Show me the current status of the daisy chain
  def showCurrentChain() = {
    if (isTrace) {
      println("--- CURRENT CHAIN ---")
      Module.signals map (x => peek(x.shadow))
      println("---------------------")
    }
  }

  def daisyCopy() {
    val daisyOut  = DaisyTransform.daisyOut(c)
    val daisyCtrl = DaisyTransform.daisyCtrl(c)
    do {
      poke(daisyCtrl, 0)
      poke(daisyOut.ready, 1)
      clock(1)
    } while (peek(daisyOut.valid) == 0)
    poke(daisyOut.ready, 0)
    showCurrentChain()
  }

  def daisyRead() = {
    val daisyOut  = DaisyTransform.daisyOut(c)
    val daisyCtrl = DaisyTransform.daisyCtrl(c)
    do {
      poke(daisyCtrl, 1)
      poke(daisyOut.ready, 1)
      clock(1)
    } while (peek(daisyOut.valid) == 0)
    poke(daisyOut.ready, 0)
    showCurrentChain()
    peek(daisyOut.bits)
  }

  def daisyCheck(expected: BigInt) {
    val daisyOut  = DaisyTransform.daisyOut(c)
    expect(daisyOut.bits, expected)
  }

  override def dump(): Snapshot = {
    val snap = new Snapshot(t)

    if (isTrace) println("*** Daisy Copy ***")
    // Copy activity counter values to shadow counters
    daisyCopy()

    for ((signal, i) <- Module.signals.zipWithIndex) {
      if (isTrace) println("*** Daisy Read ***")
      // Read out the daisy chain
      val daisyValue = daisyRead()
      // Check the daisy output
      daisyCheck(peeks(i))
      signal match {
        case read: MemRead =>
          snap.pokes += Poke(read.mem, read.addr.litValue(0).toInt, daisyValue)
        case _ =>
          snap.pokes += Poke(signal, 0, daisyValue)
      }
    }
    snap
  }

  override def step (n: Int = 1) { 
    if (isTrace) {
      println("-------------------------")
      println("| Counter Strcture Step |")
      println("-------------------------")
    }

    /*** Snapshotting! ***/
    if (t != 0) snapshot()
    val target = t + n

    // set clock register
    pokeClks(n)

    // run the target until it is stalled
    if (isTrace) println("*** CYCLE THE TARGET ***")
    clock(n)
    // read out signal values
    if (isTrace) println("*** READ SIGNAL VALUES ***")
    peeks.clear
    peeks ++= Module.signals map {
      case read: MemRead =>
        peekBits(read.mem, read.addr.litValue(0).toInt)
      case signal =>
        peekBits(signal)
    }

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
  val top       = DaisyTransform(c)
  val clks      = DaisyTransform.clks(top)
  val daisyOut  = DaisyTransform.daisyOut(top)
  val daisyCtrl = DaisyTransform.daisyCtrl(top)
  // write 4 => clks
  clks.bits := io.in.bits
  clks.valid := wen(4)
  wready(4) := clks.ready
  // read 4 => daisychain
  rdata(4) := daisyOut.bits
  rvalid(4) := daisyOut.valid
  daisyOut.ready := ren(4)
  // daisy control bit <- MSB of addr
  daisyCtrl := io.addr(aw-1) 
}

abstract class DaisyWrapperTester[+T <: DaisyFPGAWrapper[_]](c: T, isTrace: Boolean = true) extends DaisyTester(c, isTrace) {
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

  // poke 'n' clocks to the clock counter
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
  override def daisyCopy() {
    peekAddr(4)
    showCurrentChain()
  }

  // read at 4 | 1 << 4 -> daisy read
  override def daisyRead() = {
    val daisyValue = peekAddr(4 | 1 << 4)
    showCurrentChain()
    daisyValue
  }

  override def daisyCheck(expected: BigInt) {
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
        if (Module.isBackannotating) {
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
          Module.signals += cur
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

        Module.signals += m.signals.last
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
      for (signal <- Module.signals ; if signal.width > 1) {
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
      for (s <- Module.signals) {
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

    for (signal <- Module.signals) {
      counts(signal) = 0
    }

    // set clock register
    pokeClks(n)

    // run the target until it is stalled
    if (isTrace) println("*** RUN THE TAREGT / READ SIGNAL VALUES ***")
    for (i <- 0 until n) {
      for (signal <- Module.signals) {
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
    for (signal <- Module.signals) {
      expect(signal.counter, counts(signal))
    }

    if (isTrace) println("*** Daisy Copy ***")
    // Copy activity counter values to shadow counters
    peekDaisy(0)
    showCurrentChain

    for (signal <- Module.signals) {
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
  for (signal <- Module.signals ; if signal.width > 1) {
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
