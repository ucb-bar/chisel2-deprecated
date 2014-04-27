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
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.{Queue => ScalaQueue}
import scala.math.pow

object wire {
  def apply(pair: (Node, Data)) {
    val input = pair._1
    val consumer = pair._2
    if (consumer.inputs.isEmpty) consumer.inputs += input
    else consumer.inputs(0) = input
  }
}

object addRegBase {
  def apply(m: Module, outType: Bits, name: String, updates: Seq[(Bool, Node)]) = {
    val reg = outType.comp match {
      case r: Reg => r
    }
    // assign component
    reg.component = m
    // assign clock
    reg.clock = m.clock
    // assign name
    if (name != "") reg setName name
    // add updates
    reg.updates ++= updates
    // set enable signal
    reg.isEnable = !updates.isEmpty
    if (reg.isEnable) {
      val (conds, assigns) = updates.unzip
      reg.enable = (conds.tail foldLeft conds.head)(_ || _) 
    }

    reg
  }
}

// DaisyTransform inserts the step counter and
// the snapshot and evenet counter pins
object DaisyTransform {
  var top: Module = null

  val stepsIn = Decoupled(UInt(width = 32)).flip
  val clockIn = Decoupled(UInt(width = 32)).flip
  val snapOut = Decoupled(UInt(width = 32))
  val snapCtrl = UInt(INPUT, 1)
  val cntrOut = Decoupled(UInt(width = 32))
  val cntrCtrl = UInt(INPUT, 1)

  val fires = new LinkedHashMap[Clock, HashMap[Module, Bool]]
  val isSteps = new HashMap[Module, Bool]
  val snapIns = new HashMap[Module, UInt]
  val snapOuts = new HashMap[Module, DecoupledIO[UInt]]
  val snapCtrls = new HashMap[Module, Bits]
  val snapCopy = new HashMap[Module, Bool]
  val snapRead = new HashMap[Module, Bool]
  val cntrIns = new HashMap[Module, UInt]
  val cntrOuts = new HashMap[Module, DecoupledIO[UInt]]
  val cntrCtrls = new HashMap[Module, Bits]
  val cntrCopy = new HashMap[Module, Bool]
  val cntrRead = new HashMap[Module, Bool]

  val states = new ArrayBuffer[Node]
  val signals = new ArrayBuffer[Node]
  lazy val clkRegs = (Driver.clocks map (_ -> Reg(UInt(width = 32)))).toMap
  lazy val clkCnts = (Driver.clocks map (_ -> Reg(UInt(width = 32)))).toMap

  val daisyNames = HashSet("steps", "is_step",
    "snap_in", "snap_ctrl", "cntr_in", "cntr_ctrl",
    "steps_in_ready", "steps_in_valid", "steps_in_bits",
    "clock_in_ready", "clock_in_valid", "clock_in_bits",
    "snap_out_ready", "snap_out_valid", "snap_out_bits",
    "cntr_out_ready", "cntr_out_valid", "cntr_out_bits",
    "snap_fire", "snap_copy", "snap_read",
    "cntr_fire", "cntr_copy", "cntr_read",
    "clk_num_reg")

  def apply[T <: Module](c: => T) = {
    top = Module(c)

    val isStep = addStepAndClkCnts(top)
    addDaisyPins(top, isStep)
    top.asInstanceOf[T]
  }

  def addStepAndClkCnts (top: Module) = {
    ChiselError.info("[DaisyTransform] add step and clock counts")
    // step counters
    val steps = Reg(init = UInt(0, 32))
    val isStep = addTypeNode(top, steps.orR, "is_step") 
    addReg(top, steps, "steps",
      (!isStep && stepsIn.valid) -> stepsIn.bits,
      isStep                     -> (steps - UInt(1)))

    // generate clock counters for multi clock domains
    if (Driver.clocks.size > 1) { 
      var clkIdx = Driver.clocks count (_.srcClock == null)
      val clkNum = Reg(init = UInt(clkIdx, 8))
      addReg(top, clkNum, "clk_num_reg", clockIn.valid -> (clkNum - UInt(1)))
      wire(clkNum.orR -> clockIn.ready)

      for ((clock, idx) <- Driver.clocks.zipWithIndex) {
        val clkRegName = "clock_reg_" + idx
        daisyNames += clkRegName
        if (clock.srcClock != null) {
          val clkExp = (clock.initStr split " ").tail
          clkExp(0) match {
            case "*" => 
              addReg(top, clkRegs(clock), clkRegName,
                Bool(true) -> (clkRegs(clock.srcClock) * UInt(clkExp(1))))
            case "/" =>
              addReg(top, clkRegs(clock), clkRegName,
                Bool(true) -> (clkRegs(clock.srcClock) / UInt(clkExp(1))))
          }
        } else {
          addReg(top, clkRegs(clock), clkRegName,
            (clockIn.valid && clkNum === UInt(clkIdx)) -> clockIn.bits)
          clkIdx = clkIdx - 1
        }
      }
 
      val min = addTypeNode(top, (Driver.clocks foldLeft UInt(1 << 31 - 1))(
        (mux, clock) => Mux(clkCnts(clock) < mux, clkCnts(clock), mux)), "min")
      top.debug(min) // for debug

      for ((clock, idx) <- Driver.clocks.zipWithIndex) {
        val fire = addTypeNode(top, !clkCnts(clock).orR, "fire_" + idx)
        val clkCntName = "clock_cnt_" + idx
        daisyNames += clkCntName
        fires(clock) = HashMap(top -> fire)
        addReg(top, clkCnts(clock), clkCntName,
          (!fire && isStep) -> (clkCnts(clock) - min),
          fire              -> clkRegs(clock))
      }
    }
  }

  def addDaisyPins (c: Module, isStep: Bool) = {
    ChiselError.info("[DaisyTransform] add daisy pins")

    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      if (m == c) {
        if (Driver.clocks.size > 1)
          addPin(top, clockIn, "clock_in")
        addPin(top, stepsIn, "steps_in")
        isSteps(m)   = isStep
        snapIns(m)   = UInt(0)
        cntrIns(m)   = UInt(0)
	snapOuts(m)  = addPin(m, snapOut, "snap_out")
	snapCtrls(m) = addPin(m, snapCtrl, "snap_ctrl")
	cntrOuts(m)  = addPin(m, cntrOut, "cntr_out")
	cntrCtrls(m) = addPin(m, cntrCtrl, "cntr_ctrl") 
        wire(!isSteps(m) -> stepsIn.ready)
      } else {
        isSteps(m)   = addPin(m, Bool(INPUT), "is_step")
        snapIns(m)   = addPin(m, UInt(INPUT, 32), "snap_in")
        cntrIns(m)   = addPin(m, UInt(INPUT, 32), "cntr_in")
	snapOuts(m)  = addPin(m, Decoupled(UInt(width = 32)), "snap_out")
	snapCtrls(m) = addPin(m, UInt(INPUT, 1), "snap_ctrl")
	cntrOuts(m)  = addPin(m, Decoupled(UInt(width = 32)), "cntr_out")
	cntrCtrls(m) = addPin(m, UInt(INPUT, 1), "cntr_cntrl")
        wire(isSteps(m.parent) -> isSteps(m))
        wire(snapCtrls(m.parent) -> snapCtrls(m))
        wire(snapOuts(m.parent).ready -> snapOuts(m).ready)
      }

      if (m != c && Driver.clocks.size > 1) {
        for ((clock, idx) <- Driver.clocks.zipWithIndex) {
          fires(clock)(m) = addPin(m, Bool(INPUT), "fire_" + idx)
          wire(fires(clock)(m.parent) -> fires(clock)(m))
        }
      }

      val snapFire = addTypeNode(m, snapOuts(m).ready && !isSteps(m), "snap_fire")
      val cntrFire = addTypeNode(m, cntrOuts(m).ready && !isSteps(m), "cntr_fire")
      snapCopy(m) = addTypeNode(m, snapFire && snapCtrls(m) === Bits(0), "snap_copy")
      snapRead(m) = addTypeNode(m, snapFire && snapCtrls(m) === Bits(1), "snap_read")
      cntrCopy(m) = addTypeNode(m, cntrFire && cntrCtrls(m) === Bits(0), "cntr_copy")
      cntrRead(m) = addTypeNode(m, cntrFire && cntrCtrls(m) === Bits(1), "cntr_read")
      wire(snapFire -> snapOuts(m).valid)
      wire(cntrFire -> cntrOuts(m).valid)

      // visit children
      m.children foreach (queue enqueue _)
    }
  }

  def addReg(m: Module, outType: Bits, name: String, updates: (Bool, Node)*) {
    val reg = addRegBase(m, outType, name, updates)
  }

  def addPin[T <: Data](m: Module, pin: T, name: String) = {
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
    pin
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
    transforms += ((c: Module) => decoupleTarget(DaisyTransform.top))
    transforms += ((c: Module) => appendFires(DaisyTransform.top))
    if (Driver.isSnapshotting) {
      transforms += (c => findStates(DaisyTransform.top))
      transforms += (c => genDaisyChain(DaisyTransform.top, SnapshotChain))
    }
    if (Driver.genCounter) {
      transforms += (c => genDaisyChain(DaisyTransform.top, CounterChain))
    }
    transforms += ((c: Module) => c.addClockAndReset)
    transforms += ((c: Module) => gatherClocksAndResets)
    transforms += ((c: Module) => connectResets)
    transforms += ((c: Module) => c.inferAll)
    transforms += ((c: Module) => c.forceMatchingWidths)
    transforms += ((c: Module) => c.removeTypeNodes)
    transforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  }

  val ioBuffers = new HashMap[Node, Bits]
  lazy val daisyNames = DaisyTransform.daisyNames

  def addReg(m: Module, outType: Bits, name: String, updates: (Bool, Node)*) {
    val reg = addRegBase(m, outType, name, updates)
    // genreate muxes
    reg genMuxes reg
    // assign reset
    if (reg.isReset) 
      reg.inputs += m.reset
  }

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
            DaisyTransform.isSteps(c) -> targetPin.inputs.head)
          wire(buf -> targetPin)
        }
        buf
      }
    }
  }

  def appendFires(c: Module) {
    ChiselError.info("[DaisyChain] append fire signals to Reg and Mem")

    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val top = queue.dequeue
      val isStep = DaisyTransform.isSteps(top)
      if (Driver.clocks.size > 1) {
        top.clocks.clear
        top.clock = Driver.implicitClock
        top.clocks += Driver.implicitClock
      }

      // Make all delay nodes be enabled by the fire signal
      for (node <- top.nodes ; if !(daisyNames contains node.name)) {
        val clk = node.clock
        val fire = 
          if (Driver.clocks.size <= 1 || clk == null) Bool(true)
          else DaisyTransform.fires(clk)(top)
        node match {
          // For Reg, different muxes are generated by different backend
          case reg: Reg if this.isInstanceOf[VerilogBackend] && 
                           !Driver.isBackannotating => {
            val enable = fire && isStep && reg.enable
            if (reg.isEnable) {
              reg.inputs(reg.enableIndex) = enable.getNode  
            } else {
              reg.isEnable = true
              reg.enableIndex = if (reg.isReset) 2 else 1
              reg.inputs insert (reg.enableIndex, enable.getNode)
            }
            if (Driver.clocks.size > 1)
              reg.clock = top.clock
          }
          case reg: Reg => {
            reg.inputs(0) = Multiplex(fire && isStep, reg.next, reg)
            if (Driver.clocks.size > 1)
              reg.clock = top.clock
          }
          case mem: Mem[_] => {
            for (write <- mem.writeAccesses) {
              val en = Bool()
              val newEn = fire && isStep && en
              wire(write.inputs(1) -> en)
              write.inputs(1) = newEn
            }
            if (Driver.clocks.size > 1)
              mem.clock = top.clock
          }
          case _ =>
        }
      }
 
      top.children foreach (queue enqueue _)
    }  
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

  def genCounters (c: Module) {
    ChiselError.info("[CounterBackend] generate counters")
    val queue = ScalaQueue(c)
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

  trait ChainType
  object SnapshotChain extends ChainType
  object CounterChain extends ChainType

  var snapIdx = -1
  def emitSnapIdx = {
    snapIdx = snapIdx + 1
    snapIdx
  }

  def genDaisyChain(c: Module, chainT: ChainType) {
    ChiselError.info("[SnapshotChain] generate snapshot chains")
 
    val queue = ScalaQueue(c)
    // Daisy chaining
    while (!queue.isEmpty) {
      val m = queue.dequeue
      val copy = chainT match {
        case SnapshotChain => DaisyTransform.snapCopy(m)
        case CounterChain  => DaisyTransform.cntrCopy(m)
      }
      val read = chainT match {
        case SnapshotChain => DaisyTransform.snapRead(m)
        case CounterChain  => DaisyTransform.cntrRead(m)
      }
      val chain = chainT match {
        case SnapshotChain => {
          m.states foreach { node =>
            node.snapShadow = Reg(UInt(width=32))
            node.snapIdx = emitSnapIdx
          }
          m.states
        }
        case CounterChain  => m.signals
      }
      val daisyOut = chainT match {
        case SnapshotChain => DaisyTransform.snapOuts(m)
        case CounterChain  => DaisyTransform.cntrOuts(m)
      }
      val daisyIn = chainT match {
        case SnapshotChain => DaisyTransform.snapIns(m)
        case CounterChain  => DaisyTransform.cntrIns(m)
      }

      (m.children.isEmpty, chain.isEmpty) match {
        // no children & no singals 
        case (true, true) => {
          wire(daisyIn -> daisyOut.bits)
        }
        // children but no signals
        case (false, true) => {
	  val headDaisyOut = chainT match {
	    case SnapshotChain => DaisyTransform.snapOuts(m.children.head)
	    case CounterChain  => DaisyTransform.cntrOuts(m.children.head)
	  }
	  val lastDaisyIn = chainT match {
	    case SnapshotChain => DaisyTransform.snapIns(m.children.last)
	    case CounterChain  => DaisyTransform.cntrIns(m.children.last)
	  }
          // the head child's daisy output -> daisy output
          wire(headDaisyOut.bits -> daisyOut.bits)
          // daisy input -> last child's daisy input 
          wire(daisyIn -> lastDaisyIn)
        }
        // no children but signals
        case (true, false) => {
          val lastShadow = chainT match {
            case SnapshotChain => chain.last.snapShadow
            case CounterChain  => chain.last.cntrShadow
          }
          val shadowName = chainT match {
            case SnapshotChain => "snap_shadow_" + chain.last.snapIdx
            case CounterChain  => "cntr_shadow_" + chain.last.cntrIdx
          }
          // snap output -> head shadow
          wire(chain.head.snapShadow -> daisyOut.bits)
          val state = ioBuffers getOrElse (chain.last, chain.last)
          // snap input -> last shadow 
          addReg(m, lastShadow, shadowName, copy -> state, read -> daisyIn)
        }
        // children & signals
        case (false, false) => {
	  val headDaisyOut = chainT match {
	    case SnapshotChain => DaisyTransform.snapOuts(m.children.head)
	    case CounterChain  => DaisyTransform.cntrOuts(m.children.head)
	  }
	  val lastDaisyIn = chainT match {
	    case SnapshotChain => DaisyTransform.snapIns(m.children.last)
	    case CounterChain  => DaisyTransform.cntrIns(m.children.last)
	  }
          val headShadow = chainT match {
            case SnapshotChain => chain.head.snapShadow
            case CounterChain  => chain.head.cntrShadow
          }
          val lastShadow = chainT match {
            case SnapshotChain => chain.last.snapShadow
            case CounterChain  => chain.last.cntrShadow
          }
          val shadowName = chainT match {
            case SnapshotChain => "snap_shadow_" + chain.last.snapIdx
            case CounterChain  => "cntr_shadow_" + chain.last.cntrIdx
          }
          // head shadow -> daisy output
          wire(headShadow -> daisyOut.bits)
          val state = ioBuffers getOrElse (chain.last, chain.last)
          // daisy input -> last shadow
          addReg(m, lastShadow, shadowName, copy -> state, read -> headDaisyOut.bits)
          // daisy input -> last child's snap input
          wire(daisyIn -> lastDaisyIn)
        }
      }
 
      for (s <- m.children sliding 2 ; if s.size == 2) {
        val curDaisyIn = chainT match {
          case SnapshotChain => DaisyTransform.snapIns(s.head)
          case CounterChain  => DaisyTransform.cntrIns(s.head)
        }
        val nextDaisyOut = chainT match {
          case SnapshotChain => DaisyTransform.snapOuts(s.last)
          case CounterChain  => DaisyTransform.cntrOuts(s.last)
        }
        // the next child's daisy output -> the cur child's daisy input
        wire(nextDaisyOut.bits -> curDaisyIn)
      }

      for (s <- chain sliding 2 ; if s.size == 2) {
        val state = ioBuffers getOrElse (s.head, s.head)
        val curShadow = chainT match {
          case SnapshotChain => s.head.snapShadow
          case CounterChain  => s.head.cntrShadow
        }
        val nextShadow = chainT match {
          case SnapshotChain => s.last.snapShadow
          case CounterChain  => s.last.cntrShadow
        }
        val shadowName = chainT match {
          case SnapshotChain => "snap_shadow_" + s.head.snapIdx
          case CounterChain  => "cntr_shadow_" + s.head.cntrIdx
        }
        /****** Shaodw Counter *****/
        // daisy_ctrl == 'copy' -> current state
        // daisy_ctrl == 'read' -> next shadow 
        addReg(m, curShadow, shadowName, copy -> state, read -> nextShadow)
      }
      // collect signal lists
      chainT match {
        case SnapshotChain => DaisyTransform.states  ++= m.states
        case CounterChain  => DaisyTransform.signals ++= m.signals
      }
      // visit children
      m.children foreach (queue enqueue _)
    }
  }  
}

class DaisyVerilog extends VerilogBackend with DaisyChain
class DaisyCpp     extends CppBackend     with DaisyChain

abstract class DaisyTester[+T <: Module](c: T, isTrace: Boolean = true) extends Tester(c, isTrace) {
  val peeks = new ArrayBuffer[BigInt]
  val clockIn  = DaisyTransform.clockIn
  val stepsIn  = DaisyTransform.stepsIn
  val snapOut  = DaisyTransform.snapOut
  val snapCtrl = DaisyTransform.snapCtrl
  val states   = DaisyTransform.states
  val clockVals = new LinkedHashMap[Clock, Int]
  val clockCnts = new LinkedHashMap[Clock, Int]

  def takeSteps (n: Int) {
    val clk = emulatorCmd("step %d".format(n))
    if (isTrace) println("  STEP %d".format(n))
  }

  def pokeSteps (n: Int) {
    // Wait until the clock counter is ready
    // (the target is stalled)
    while(peek(stepsIn.ready) == 0) {
      takeSteps(1)
    }
    // Set the step counter
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

  override def setClocks(clocks: HashMap[Clock, Int]) {
    for (clock <- Driver.clocks) {
      if (clock.srcClock == null) {
        while (peek(clockIn.ready) == 0) {
          takeSteps(1)
        }
        // set clock counters
        poke(clockIn.bits, clocks(clock))
        poke(clockIn.valid, 1)
        takeSteps(1)
        poke(clockIn.valid, 0)
        // set clock values for the tester
        clockVals(clock) = clocks(clock)
      } else {
        val tokens = clock.initStr split " "
        tokens(1) match {
          case "*" => 
            clockVals(clock) = clockVals(clock.srcClock) * tokens(2).toInt
          case "/" => 
            clockVals(clock) = clockVals(clock.srcClock) / tokens(2).toInt
        }
      }
      clockCnts(clock) = clockVals(clock)
    }
  }

  override def dump(): Snapshot = {
    val snap = new Snapshot(t)

    if (isTrace) println("*** Snapshot Chain Copy ***")
    // Copy activity counter values to shadow counters
    snapCopy()

    for ((state, i) <- states.zipWithIndex) {
      if (isTrace) println("*** Snapshot Chain Read %d ***".format(i))
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

    // set t & delta
    t += n
    if (Driver.clocks.size > 1) {
      for (i <- 0 until n) {
        val delta_i = clockCnts.minBy(_._2)._2
        for (clock <- Driver.clocks) {
          clockCnts(clock) -= delta_i
          if (clockCnts(clock) == 0)
            clockCnts(clock) = clockVals(clock)
        }
        delta += delta_i
      }
    }
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
  val clockIn  = DaisyTransform.clockIn
  val snapOut  = DaisyTransform.snapOut
  val snapCtrl = DaisyTransform.snapCtrl
  val cntrOut  = DaisyTransform.cntrOut
  val cntrCtrl = DaisyTransform.cntrCtrl
  // write 4 => steps
  stepsIn.bits := io.in.bits
  stepsIn.valid := wen(4)
  wready(4) := stepsIn.ready
  // write 5 => clocks
  clockIn.bits := io.in.bits
  clockIn.valid := wen(5)
  wready(5) := clockIn.ready
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
