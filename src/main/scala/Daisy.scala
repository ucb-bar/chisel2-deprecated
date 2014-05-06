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
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.{Queue => ScalaQueue}
import scala.math.pow
import scala.math.max

// Counter type definition
trait CounterType
object Default extends CounterType
object Activity extends CounterType
object Ones extends CounterType
object Zeros extends CounterType
object Posedge extends CounterType
object Negedge extends CounterType

object DaisyType {
  val states = new ArrayBuffer[State]
  val eventCounters = new ArrayBuffer[EventCounter]
  lazy val counters = eventCounters sortWith ((x, y) => {
      val compX = x.src.component
      val compY = y.src.component
      (compX.level > compY.level) || 
      (compX.level == compY.level && compX.traversal < compY.traversal) })
}

abstract class DaisyType(val idx: Int, val w: Int = 32) {
  def src: Node
  val shadow = Reg(Bits(width=w))
}

import DaisyType._

object State {
  var snapIdx = -1
  def emitSnapIdx = {
    snapIdx = snapIdx + 1
    snapIdx
  }
  def apply(src: Node) = {
    val state = new State(src, emitSnapIdx)
    states += state
    state
  }
}
class State(val src: Node, idx: Int) extends DaisyType(idx)

object EventCounter {
  var cntrIdx = -1
  def emitCntrIdx = {
    cntrIdx = cntrIdx + 1
    cntrIdx
  }
  def apply(signal: Node, counterT: CounterType) = {
    val counter = new EventCounter(signal, counterT, emitCntrIdx)
    eventCounters += counter
    counter
  }
}
class EventCounter(
    val signal: Node, 
    val cntrT: CounterType,
    idx: Int, w: Int =32) extends DaisyType(idx){
  val src = Reg(init = Bits(0, w))
}

object addPin {
  val tempStack = new ListBuffer[Module]

  private def start() {
    while (!Driver.compStack.isEmpty) 
      tempStack prepend Driver.compStack.pop
  }

  private def finish() {
    while (!tempStack.isEmpty) {
      Driver.compStack push tempStack.head
      tempStack -= tempStack.head
    }
  }

  def apply[T <: Data](m: Module, gen: => T, name: String) = {
    require(name != "")
    start()
    val pin = gen
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
    finish()
    pin
  }

  def wire(pair: => ((Node, Node))) {
    start()
    val (input, consumer) = pair
    if (consumer.inputs.isEmpty) consumer.inputs += input
    else consumer.inputs(0) = input
    finish()
  }
}

import addPin._

// DaisyTransform inserts the step counter and
// the snapshot and evenet counter pins
object DaisyTransform {
  var top: Module = null
  var done = false
  var inNum = -1
  var outNum = -1
  var stallVal: Bool = null
  var stallAck: Bool = null

  lazy val stepsIn  = addPin(top, Decoupled(UInt(width = 32)).flip, "steps_in")
  lazy val clockIn  = addPin(top, Decoupled(UInt(width = 32)).flip, "clock_in")
  lazy val snapOut  = addPin(top, Decoupled(UInt(width = 32)), "snap_out")
  lazy val snapCtrl = addPin(top, UInt(INPUT, 1), "snap_ctrl")
  lazy val cntrOut  = addPin(top, Decoupled(UInt(width = 32)), "cntr_out")
  lazy val cntrCtrl = addPin(top, UInt(INPUT, 1), "cntr_ctrl")
  lazy val stalled  = addPin(top, Bool(OUTPUT), "stalled")

  val snapIns = new HashMap[Module, UInt]
  val snapOuts = new HashMap[Module, DecoupledIO[UInt]]
  val snapCtrls = new HashMap[Module, Bits]
  val cntrIns = new HashMap[Module, UInt]
  val cntrOuts = new HashMap[Module, DecoupledIO[UInt]]
  val cntrCtrls = new HashMap[Module, Bits]

  def apply[T <: Module](c: => T, fromDriver: Boolean = false) = {
    top = if (fromDriver) c else Module(c)
    inNum  = top.io.flatten count (x => x._2.dir == INPUT)
    outNum = top.io.flatten count (x => x._2.dir == OUTPUT)
    addDaisyPins(top)
    done = true
    top.asInstanceOf[T]
  }

  def addDaisyPins (c: Module) = {
    ChiselError.info("[DaisyTransform] add daisy pins")
    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      if (m == c) {
        if (Driver.isSnapshotting) {
          snapIns(m)   = UInt(0)
          snapOuts(m)  = snapOut 
          snapCtrls(m) = snapCtrl 
        }
        if (Driver.isCounting) {
          cntrIns(m)   = UInt(0)
          cntrOuts(m)  = cntrOut 
          cntrCtrls(m) = cntrCtrl 
        }
        // make the target as needed
        wire(Bool(true) -> stepsIn.ready)
      } else {
        if (Driver.isSnapshotting) {
          snapIns(m)   = addPin(m, UInt(INPUT, 32), "snap_in")
          snapOuts(m)  = addPin(m, Decoupled(UInt(width = 32)), "snap_out")
          snapCtrls(m) = addPin(m, UInt(INPUT, 1), "snap_ctrl")
          wire(snapCtrls(m.parent) -> snapCtrls(m))
          wire(snapOuts(m.parent).ready -> snapOuts(m).ready)
        }
        if (Driver.isCounting) {
          cntrIns(m)   = addPin(m, UInt(INPUT, 32), "cntr_in")
          cntrOuts(m)  = addPin(m, Decoupled(UInt(width = 32)), "cntr_out")
          cntrCtrls(m) = addPin(m, UInt(INPUT, 1), "cntr_ctrl")
          wire(cntrCtrls(m.parent) -> cntrCtrls(m))
          wire(cntrOuts(m.parent).ready -> cntrOuts(m).ready)
        }
      }

      // visit children
      m.children foreach (queue enqueue _)
    }
  }
}

import DaisyTransform._

object DaisyChain extends Backend {
  val keywords = HashSet(
    "stalled", "steps", "step_pin", "step_pin_buf", "fire",
    "state_num", "counter_num", "fire_pin", "fire_pin_buf",
    "snap_in", "snap_ctrl", "cntr_in", "cntr_ctrl",
    "steps_in_ready", "steps_in_valid", "steps_in_bits",
    "clock_in_ready", "clock_in_valid", "clock_in_bits",
    "snap_out_ready", "snap_out_valid", "snap_out_bits",
    "cntr_out_ready", "cntr_out_valid", "cntr_out_bits",
    "snap_fire", "snap_copy", "snap_read",
    "cntr_fire", "cntr_copy", "cntr_read",
    "clk_num_reg")

  def apply (b: Backend) {
    b.transforms += ((c: Module) => c bfs (_.addConsumers))
    b.transforms += ((c: Module) => addStepCounter(top))
    b.transforms += ((c: Module) => setClockDomains(top))
    b.transforms += ((c: Module) => decoupleTarget(top))
    b.transforms += ((c: Module) => appendFires(top))
    if (Driver.isCounting) {
      b.transforms += (c => genCounters(top))
      b.transforms += (c => genDaisyChain(top, CounterChain))
    }
    if (Driver.isSnapshotting) {
      b.transforms += (c => findStates(top))
      b.transforms += (c => genDaisyChain(top, SnapshotChain))
    }
    b.transforms += ((c: Module) => c.addClockAndReset)
    b.transforms += ((c: Module) => gatherClocksAndResets)
    b.transforms += ((c: Module) => connectResets)
    b.transforms += ((c: Module) => c.inferAll)
    b.transforms += ((c: Module) => c.forceMatchingWidths)
    b.transforms += ((c: Module) => c.removeTypeNodes)
    b.transforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
    b.analyses   += ((c: Module) => genChainOffsets(c))
  }

  def addNode[T <: Bits](m: Module, gen: => T, name: String = ""): T = {
    val res = gen
    if (name != "") res.getNode setName name
    res.getNode.component = m
    res
  }

  def addReg[T <: Bits](m: Module, gen: => T, name: String = "") = {
    val res = gen
    val reg = res.comp match { case r: Reg => r }
    // assign component
    reg.component = m
    // assign clock & reset
    reg.assignClock(m.clock)
    reg.assignReset(m.reset)
    // assign name
    if (name != "") reg setName name
    res
  }

  def updateReg(regType: Bits, updates: (Bool, Node)*) {
    val reg = regType.comp match { case r: Reg => r }
    // add updates
    for ((cond, value) <- updates)
      reg.doProcAssign(value, cond)
  }

  val ioBuffers = new HashMap[Node, Bits]
  val fires = new HashMap[Module, Bool]
  val fireBufs = new HashMap[Module, Bool]
  val firePins = new HashMap[Module, Bool]
  val enClks = new LinkedHashMap[Clock, HashMap[Module, Bool]]
  val clkRegs = new HashMap[Clock, UInt]
  val clkCnts = new HashMap[Clock, UInt]
  val snapCopy = new HashMap[Module, Bool]
  val snapRead = new HashMap[Module, Bool]
  val cntrCopy = new HashMap[Module, Bool]
  val cntrRead = new HashMap[Module, Bool]

  def addStepCounter(c: Module) = {
    ChiselError.info("[DaisyChain] add step counters")
    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      if (m == c) {
        // Top component: add the step counter
        val steps  = addReg(m, Reg(init = UInt(0, 32)), "steps")
        val isStep = addNode(m, steps.orR, "step_pin")
        updateReg(steps, isStep -> (steps - UInt(1)), stepsIn.valid -> stepsIn.bits)
        if (stallAck != null) {
          firePins(m) = addNode(m, isStep && stallAck, "fire")
        } else {
          firePins(m) = isStep
        }
        if (stallVal != null) {
          wire(isStep -> stallVal)
        }
      } else {
        firePins(m) = addPin(m, Bool(INPUT), "fire_pin")
        wire(firePins(m.parent) -> firePins(m))
      }

      // add a fire pin buffer used by event counters
      // Event counters should increase one cycle after
      // the target is activated
      fireBufs(m) = addReg(m, Reg(next=firePins(m)), "fire_pin_buf")
      fireBufs(m).comp.clock = Driver.implicitClock

      if (Driver.isSnapshotting) {
        val snapValid = !firePins(m) && !fireBufs(m)
        val snapFire = addNode(m, snapOuts(m).ready && snapValid, "snap_fire")
        snapCopy(m) = addNode(m, snapFire && (snapCtrls(m) === Bits(0)), "snap_copy")
        snapRead(m) = addNode(m, snapFire && (snapCtrls(m) === Bits(1)), "snap_read")
        if (m == c) {
          wire(snapValid -> stalled)
        } else {
          wire(snapValid -> snapOuts(m).valid)
        }
      }
      if (Driver.isCounting) {
        val cntrValid = !firePins(m) && !fireBufs(m) 
        val cntrFire = addNode(m, cntrOuts(m).ready && cntrValid, "cntr_fire")
        cntrCopy(m) = addNode(m, cntrFire && (cntrCtrls(m) === Bits(0)), "cntr_copy")
        cntrRead(m) = addNode(m, cntrFire && (cntrCtrls(m) === Bits(1)), "cntr_read")
        if (m == c) {
          wire(cntrValid -> stalled)
        } else {
          wire(cntrValid -> cntrOuts(m).valid)
        }
      }
      m.children foreach (queue enqueue _)
    }
  }

  def setClockDomains(c: Module) = {
    if (Driver.clocks.size > 1) { 
      ChiselError.info("[DaisyChain] set clock domains")
      // generate clock counters for multi clock domains
      var clkIdx = Driver.clocks count (_.srcClock == null)
      val clkNum = addReg(c, Reg(init = UInt(clkIdx, 8)), "clk_num_reg")
      updateReg(clkNum, clockIn.valid -> (clkNum - UInt(1)))
      wire(clkNum.orR -> clockIn.ready)

      val isClkInput = new HashMap[Clock, Bool]
      for ((clock, idx) <- Driver.clocks.zipWithIndex) {
        val clkRegName = "clock_reg_" + idx
        keywords += clkRegName
        clkRegs(clock) = addReg(c, Reg(UInt(width=8)), clkRegName)
        if (clock.srcClock != null) {
          val clkExp = (clock.initStr split " ").tail
          clkExp(0) match {
            case "*" => {
              val clkRegMul = clkRegs(clock.srcClock) * UInt(clkExp(1))
              updateReg(clkRegs(clock), Bool(true) -> clkRegMul)
            }
            case "/" => {
              val clkRegDiv = clkRegs(clock.srcClock) / UInt(clkExp(1))
              updateReg(clkRegs(clock), Bool(true) -> clkRegDiv)
            }
          }
        } else {
          isClkInput(clock) = clockIn.valid && (clkNum === UInt(clkIdx))
          updateReg(clkRegs(clock), isClkInput(clock) -> clockIn.bits)
          clkIdx = clkIdx - 1
        }
      }
 
      for ((clock, idx) <- Driver.clocks.zipWithIndex) {
        val clkCntName = "clock_cnt_" + idx
        keywords += clkCntName
        clkCnts(clock) = addReg(c, Reg(init=UInt(0, 8)), clkCntName)
      }

      val min = addNode(c, (Driver.clocks foldLeft UInt(1 << 31 - 1))(
        (mux, clock) => Mux(clkCnts(clock) < mux, clkCnts(clock), mux)), "min")
      c.debug(min) // for debug

      for ((clock, idx) <- Driver.clocks.zipWithIndex) {
        val enClkName = "en_clk_" + idx
        val enClk     = addNode(c, !clkCnts(clock).orR, enClkName)
        keywords += enClkName
        enClks(clock) = HashMap(c -> enClk)
        updateReg(clkCnts(clock), 
          isClkInput(clock)                   -> clockIn.bits, 
          (firePins(c) && !enClk)             -> (clkCnts(clock) - min), 
          (firePins(c) && !isClkInput(clock)) -> clkRegs(clock))
      }
    }

    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      if (m != c && Driver.clocks.size > 1) {
        for ((clock, idx) <- Driver.clocks.zipWithIndex) {
          val enClkName = "en_clk_" + idx
          keywords += enClkName
          enClks(clock)(m) = addPin(m, Bool(INPUT), enClkName)
          wire(enClks(clock)(m.parent) -> enClks(clock)(m))
        }
      }
      m.children foreach (queue enqueue _)
    }
  }

  def decoupleTarget(c: Module) = {
    ChiselError.info("[DaisyChain] target decoupling")
    for ((name, io) <- c.io.asInstanceOf[Bundle].elements) {
      io nameIt (name, true)
    }
    
    // For the input and output pins of the 'c' component
    // insert buffers so that their values are avaiable
    // even though the target is stalled
    for ((name, targetPin) <- c.io.flatten; if !(keywords contains name)) {
      val bufName = name + "_buf"
      keywords += bufName
      ioBuffers(targetPin) = addReg(c, Reg(UInt()), bufName)
      if (targetPin.dir == INPUT) {
        // Input buffers work when the clock counter value is set
        updateReg(ioBuffers(targetPin), stepsIn.valid -> targetPin)
        for (consumer <- targetPin.consumers) {
          val idx = consumer.inputs indexOf targetPin
          consumer.inputs(idx) = ioBuffers(targetPin)
        }
      } else if (targetPin.dir == OUTPUT) {
        updateReg(ioBuffers(targetPin), fireBufs(c) -> targetPin.inputs.head)
        wire(ioBuffers(targetPin) -> targetPin)
      }
    }
  }

  def appendFires(c: Module) {
    ChiselError.info("[DaisyChain] append fire signals to Reg and Mem")

    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      if (Driver.clocks.size > 1) {
        m.clocks.clear
        m.clock = Driver.implicitClock
        m.clocks += Driver.implicitClock
      }

      // Make all delay nodes be enabled by the fire signal
      for (node <- m.nodes ; if !(keywords contains node.name)) {
        val fire = 
          if (Driver.clocks.size <= 1 || node.clock == null) fireBufs(m) 
          else fireBufs(m) && enClks(node.clock)(m) 
        node match {
          case reg: Reg => {
            reg.inputs(0) = Multiplex(fire, reg.next, reg)
            if (Driver.clocks.size > 1) reg.assignClock(m.clock)
          }
          case mem: Mem[_] => {
            for (write <- mem.writeAccesses) {
              val en = Bool()
              val newEn = fire && en
              wire(write.inputs(1) -> en)
              write.inputs(1) = newEn
            }
            if (Driver.clocks.size > 1) mem.assignClock(m.clock)
          }
          case _ =>
        }
      }
 
      m.children foreach (queue enqueue _)
    }
  }

  def findStates(c: Module) { 
    ChiselError.info("[SnapshotChain] find state elements")
    /*** collect state elements for snapshotting ***/
    // First, collect the top component's inputs
    for ((name, targetPin) <- c.io.flatten ; 
      if !(keywords contains name) && targetPin.dir == INPUT) {
        c.states += State(targetPin)
      }

    // Second, collect all the state elements (Reg & Mem)
    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      // collect registers and memory access
      m.nodes foreach { 
        _ match {
          case reg: Reg if !(keywords contains reg.name) =>
            m.states += State(reg)
          case mem: Mem[_] =>
            for (i <- 0 until mem.size)
              m.states += State(new MemRead(mem, UInt(i)))
          case _ =>
        }
      }
      // visit children
      m.children foreach (queue enqueue _)
    }

    // turn off the snapOut valid signal after reading out all the values 
    val stateNum = addReg(c, Reg(UInt(width=32)), "state_num")
    updateReg(stateNum, 
      (snapRead(c) && stateNum.orR) -> (stateNum - UInt(1)),
      stepsIn.valid -> UInt(states.size))
    wire((snapCopy(c) || stateNum.orR) -> snapOuts(c).valid)
  }

  def addBuffer(c: Module, i: Int, w: Int, updates: (Bool, Node)) = {
    val bufName = "buffer_%d".format(i)
    val buffer = addReg(c, Reg(init=UInt(0, w)), bufName)
    keywords += bufName
    updateReg(buffer, updates)
    buffer
  }

  def genCounters(c: Module) {
    ChiselError.info("[CounterBackend] generate counters")

    for (counter <- counters) {
      val signal = counter.signal
      val width = signal.width
      val c = counter.src.comp.component
      val fire = 
        if (Driver.clocks.size <= 1 || signal.clock == null) fireBufs(c) 
        else fireBufs(c) && enClks(signal.clock)(c) 
      val signalValue = signal match { 
        case io: Bits => 
          ioBuffers getOrElse (io, io)
        case _ => 
          UInt(signal)
      }
      val cntrValue = counter.cntrT match {
        case Default => counter.src + signalValue
        case Activity => {
          val buffer = addBuffer(c, counter.idx, width, fire -> signalValue)
          val xor = signalValue ^ buffer
          xor.inferWidth = (x: Node) => width
          counter.src + PopCount(xor)
        }
        case Ones => {
          signalValue.inferWidth = (x: Node) => width
          counter.src + PopCount(signalValue)
        }
        case Zeros => {
          signalValue.inferWidth = (x: Node) => width
          counter.src + (UInt(width) - PopCount(signalValue))
        }
        case Posedge => {
          val buffer = addBuffer(c, counter.idx, width, fire -> signalValue)
          val res = (signalValue ^ buffer) & signalValue
          res.inferWidth = (x: Node) => width
          counter.src + PopCount(res)          
        } 
        case Negedge => {
          val buffer = addBuffer(c, counter.idx, width, fire -> signalValue)
          val res = (signalValue ^ buffer) & (~signalValue)
          res.inferWidth = (x: Node) => width
          counter.src + PopCount(res)          
        }
      }
    
      cntrValue.getNode setName "cntr_val_%d".format(counter.idx)

      /* Activity Counter */
      // 1) steps > 0 -> increment counter
      // 2) the target is stalled with a copy bit -> reset
      val counterName = "counter_%d".format(counter.idx)
      keywords += counterName
      counter.src.comp setName counterName
      updateReg(counter.src, firePins(c) -> cntrValue, cntrCopy(c) -> Bits(0))
    }

    // turn off the cntrOut valid signal after reading out all the values 
    val counterNum = addReg(c, Reg(UInt(width=32)), "counter_num")
    updateReg(counterNum, 
      (cntrRead(c) && counterNum.orR) -> (counterNum - UInt(1)),
      stepsIn.valid -> UInt(counters.size))
    wire((cntrCopy(c) || (counterNum.orR)) -> cntrOuts(c).valid)
  }

  trait ChainType
  object SnapshotChain extends ChainType
  object CounterChain extends ChainType

  def genDaisyChain(c: Module, chainT: ChainType) {
    ChiselError.info("[SnapshotChain] generate snapshot chains")
 
    val queue = ScalaQueue(c)
    // Daisy chaining
    while (!queue.isEmpty) {
      val m = queue.dequeue
      val copy = chainT match {
        case SnapshotChain => snapCopy(m)
        case CounterChain  => cntrCopy(m)
      }
      val read = chainT match {
        case SnapshotChain => snapRead(m)
        case CounterChain  => cntrRead(m)
      }
      val daisyOut = chainT match {
        case SnapshotChain => snapOuts(m)
        case CounterChain  => cntrOuts(m)
      }
      val daisyIn = chainT match {
        case SnapshotChain => snapIns(m)
        case CounterChain  => cntrIns(m)
      }
      val chain = chainT match {
        case SnapshotChain => m.states
        case CounterChain  => m.counters
      }

      (m.children.isEmpty, chain.isEmpty) match {
        // no children & no singals 
        case (true, true) => {
          wire(daisyIn -> daisyOut.bits)
        }
        // children but no signals
        case (false, true) => {
	  val headDaisyOut = chainT match {
	    case SnapshotChain => snapOuts(m.children.head)
	    case CounterChain  => cntrOuts(m.children.head)
	  }
	  val lastDaisyIn = chainT match {
	    case SnapshotChain => snapIns(m.children.last)
	    case CounterChain  => cntrIns(m.children.last)
	  }
          // the head child's daisy output -> daisy output
          wire(headDaisyOut.bits -> daisyOut.bits)
          // daisy input -> last child's daisy input 
          wire(daisyIn -> lastDaisyIn)
        }
        // no children but signals
        case (true, false) => {
          val realSrc = ioBuffers getOrElse (chain.last.src, chain.last.src)
          val shadowName = ( chainT match {
            case SnapshotChain => "snap_shadow_"
            case CounterChain  => "cntr_shadow_"
          } ) + chain.last.idx
          keywords += shadowName
          chain.last.shadow.comp.component = m 
          chain.last.shadow.comp setName shadowName
          // snap output -> head shadow
          wire(chain.head.shadow -> daisyOut.bits)
          // snap input -> last shadow 
          updateReg(chain.last.shadow, copy -> realSrc, read -> daisyIn)
        }
        // children & signals
        case (false, false) => {
	  val headDaisyOut = chainT match {
	    case SnapshotChain => snapOuts(m.children.head)
	    case CounterChain  => cntrOuts(m.children.head)
	  }
	  val lastDaisyIn = chainT match {
	    case SnapshotChain => snapIns(m.children.last)
	    case CounterChain  => cntrIns(m.children.last)
	  }
          val realSrc = ioBuffers getOrElse (chain.last.src, chain.last.src)
          val shadowName = ( chainT match {
            case SnapshotChain => "snap_shadow_"
            case CounterChain  => "cntr_shadow_"
          } ) + chain.last.idx
          keywords += shadowName
          chain.last.shadow.comp.component = m 
          chain.last.shadow.comp setName shadowName
          // head shadow -> daisy output
          wire(chain.head.shadow -> daisyOut.bits)
          // daisy input -> last shadow
          updateReg(chain.last.shadow, copy -> realSrc, read -> headDaisyOut.bits)
          // daisy input -> last child's snap input
          wire(daisyIn -> lastDaisyIn)
        }
      }
 
      for (s <- m.children sliding 2 ; if s.size == 2) {
        val curDaisyIn = chainT match {
          case SnapshotChain => snapIns(s.head)
          case CounterChain  => cntrIns(s.head)
        }
        val nextDaisyOut = chainT match {
          case SnapshotChain => snapOuts(s.last)
          case CounterChain  => cntrOuts(s.last)
        }
        // the next child's daisy output -> the cur child's daisy input
        wire(nextDaisyOut.bits -> curDaisyIn)
      }

      for (s <- chain sliding 2 ; if s.size == 2) {
        val realSrc = ioBuffers getOrElse (s.head.src, s.head.src)
        val shadowName = ( chainT match {
          case SnapshotChain => "snap_shadow_"
          case CounterChain  => "cntr_shadow_"
        } ) + s.head.idx
        keywords += shadowName
        s.head.shadow.comp.component = m 
        s.head.shadow.comp setName shadowName
        /* Shaodw Counter*/
        // daisy_ctrl == 'copy' -> current source
        // daisy_ctrl == 'read' -> next shadow
        updateReg(s.head.shadow, copy -> realSrc, read -> s.last.shadow) 
      }
      // visit children
      m.children foreach (queue enqueue _)
    }
  }

  def genChainOffsets (c: Module) {
    if (Driver.isSnapshotting) {
      val snapchain = createOutputFile(c.name + ".snapchain")
      val res = new StringBuilder
      for (state <- states) {
        state.src match {
          case read: MemRead => 
            res append ("%s[%s] %d\n".format(
              read.mem.chiselName, 
              read.addr.litValue(0), 
              state.idx) ) 
          case _ =>
            res append ("%s %d %d\n".format(
              state.src.chiselName stripPrefix (top.name + "Wrapper."), 
              state.src.width, state.idx)) 
        }
      }
      try {
        snapchain write res.result
      } finally {
        snapchain.close
      }
    }
    if (Driver.isCounting) {
      val cntrchain = createOutputFile(c.name + ".cntrchain")
      val res = new StringBuilder
      for (counter <- counters) {
        res append ("%s %d %d\n".format(
          counter.signal.chiselName stripPrefix (top.name + "Wrapper."), 
          counter.signal.width, counter.idx))
      }
      try {
        cntrchain write res.result
      } finally {
        cntrchain.close
      }
    }
  }  
}

abstract class DaisyTester[+T <: Module](c: T, isTrace: Boolean = true) extends Tester(c, isTrace) {
  require(DaisyTransform.done)
  val statePeeks = new ArrayBuffer[BigInt]
  val counterVals = new ArrayBuffer[BigInt]
  val counterPeeks = new ArrayBuffer[BigInt]
  val clockVals = new LinkedHashMap[Clock, Int]
  val clockCnts = new LinkedHashMap[Clock, Int]
  override val outputs = top.io.flatten.unzip._2 filter (x => {
    val name = x.chiselName.split('.').last
    x.dir == OUTPUT && !(DaisyChain.keywords contains (name stripPrefix "io_"))
  })

  counterPeeks.clear
  counterPeeks ++= Array.fill(counters.size)(BigInt(0))

  def popCount(x: BigInt) = {
    var in = x
    var res: BigInt = 0
    while (in > 0) {
      res = res + (in & 1)
      in = in >> 1
    }
    res
  }

  def calcCounterVal(counter: EventCounter, cur: BigInt, prev: BigInt) = {
    counter.cntrT match {
      case Default => cur
      case Activity => popCount(cur ^ prev)
      case Ones => popCount(cur)
      case Zeros => (counter.signal.width - popCount(cur))
      case Posedge => popCount((cur ^ prev) & cur)
      case Negedge => popCount((cur ^ prev) & ~cur)
    }
  }

  // set clock counters
  def pokeClock (clk: Int) {
    while (peek(clockIn.ready) == 0) {
      takeSteps(1)
    }
    poke(clockIn.bits, clk)
    poke(clockIn.valid, 1)
    takeSteps(1)
    poke(clockIn.valid, 0)
  }

  def takeSteps (n: Int) {
    val clk = emulatorCmd("step %d".format(n))
    if (isTrace) println("  STEP %d".format(n))
  }

  def pokeSteps (n: Int) {
    // Wait until the clock counter is ready
    // (the target is stalled)
    while(peek(stalled) == 0) {
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
      println("--- CURRENT SNAPSHOT CHAIN ---")
      states foreach (x => peek(x.shadow))
      println("------------------------------")
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

  // Show me the current status of the daisy chain
  def showCurrentCntrChain() = {
    if (isTrace && !counters.isEmpty) {
      println("--- CURRENT COUNTER CHAIN ---")
      counters foreach (x => peek(x.shadow))
      println("-----------------------------")
    }
  }

  def cntrCopy() {
    do {
      poke(cntrCtrl, 0)
      poke(cntrOut.ready, 1)
      takeSteps(1)
    } while (peek(cntrOut.valid) == 0)
    poke(cntrOut.ready, 0)
    showCurrentCntrChain()
  }

  def cntrRead() = {
    do {
      poke(cntrCtrl, 1)
      poke(cntrOut.ready, 1)
      takeSteps(1)
    } while (peek(cntrOut.valid) == 0)
    poke(cntrOut.ready, 0)
    showCurrentCntrChain()
    peek(cntrOut.bits)
  }

  def cntrCheck(expected: BigInt) {
    expect(cntrOut.bits, expected)
  }

  def dumpCounters() {
    if (isTrace) println("*** Counter Values ***")
    for (counter <- counters) {
      val counterVal = peek(counter.src)
    }

    if (isTrace) println("*** Counter Chain Copy ***")
    // Copy activity counter values to shadow counters
    cntrCopy()

    for ((counter, i) <- counters.zipWithIndex) {
      if (isTrace) println("*** Counter Chain Read %d ***".format(i))
      // Read out the cntr chain
      val cntrValue = cntrRead()
      // Check the cntr output
      cntrCheck(counterVals(i))
    }
  }

  override def reset(n: Int) {
    super.reset(n)
    if (t > 0) {
      counterPeeks.clear
      counterPeeks ++= Array.fill(counters.size)(BigInt(0))
    }
  }

  override def setClocks(clocks: HashMap[Clock, Int]) {
    for (clock <- Driver.clocks) {
      if (clock.srcClock == null) {
        // set clock values for the tester
        pokeClock(clocks(clock))
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

  val (names, ios) = top.io.flatten.unzip
  override def dumpName(data: Node): String = {
    if (finished) {
      if (ios contains data)
        "%s %d".format(data.chiselName.split('.').last, data.width)
      else
        "%s %d".format(data.chiselName, data.width) 
    } else
      super.dumpName(data)
  }

  override def dump(): Snapshot = {
    val snap = new Snapshot(t)

    // Copy snapshot values to shadow counters
    if (isTrace) println("*** Snapshot Chain Copy ***")
    snapCopy()

    for ((state, i) <- states.zipWithIndex) {
      if (isTrace) println("*** Snapshot Chain Read %d ***".format(i))
      // Read out the snap chain
      val snapValue = snapRead()
      // Check the snap output
      snapCheck(statePeeks(i))
      state.src match {
        case read: MemRead =>
          snap.pokes += Poke(read.mem, read.addr.litValue(0).toInt, snapValue)
        case _ =>
          snap.pokes += Poke(state.src, 0, snapValue)
      }
    }
    snap
  }

  override def step (n: Int = 1) { 
    if (isTrace) {
      println("-------------------------")
      println("|   Daisy Chain Step    |")
      println("-------------------------")
    }

    /*** Snapshotting and counter dumpig ***/
    if (t > 0) {
      if (Driver.isSnapshotting) {
        checkSnapshots()
        snapshot()
      }
      if (Driver.isCounting) dumpCounters()
    }

    // set clock register
    pokeSteps(n)

    // run the target until it is stalled
    if (isTrace) println("*** CYCLE THE TARGET ***")
    counterVals.clear
    counterVals ++= Array.fill(counters.size)(BigInt(0))
    for (k <- 0 until n) {
      takeSteps(1)
      if (Driver.isCounting) {
        if (isTrace) println("*** READ COUNTER SIGNALS ***")
        for ((counter, i) <- counters.zipWithIndex) {
          val curPeek = counter.signal match {
            case read: MemRead =>
               peekBits(read.mem, read.addr.litValue(0).toInt)
            case signal =>
              peekBits(signal) }
          counterVals(i) += calcCounterVal(counter, curPeek, counterPeeks(i))
          counterPeeks(i) = curPeek
        }
      }
    }

    while (peek(stalled) == 0)
      takeSteps(1)

    // read out signal values
    if (Driver.isSnapshotting) {
      if (isTrace) println("*** READ STATE VALUES ***")
      statePeeks.clear
      statePeeks ++= states map { _.src match {
        case read: MemRead =>
          peekBits(read.mem, read.addr.litValue(0).toInt)
        case signal =>
          peekBits(signal) }
      }
    }

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

  var finished = false
  override def finish(): Boolean = {
    checkSnapshots()
    finished = true
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

class DaisyWrapper[+T <: Module](c: => T) extends AXISlave(n = 16 /* 2^(aw - 1) */){
  val top    = DaisyTransform(c)
  val outNum = max(n-3, DaisyTransform.outNum)
  val inNum  = max(n-3, DaisyTransform.inNum)
  val wdata  = Vec.fill(inNum) { Reg(UInt()) }
  // write n-2 => steps
  stepsIn.bits := io.in.bits
  stepsIn.valid := wen(n-2)
  wready(n-2) := stepsIn.ready
  // write n-1 => clocks
  clockIn.bits := io.in.bits
  clockIn.valid := wen(n-1)
  wready(n-1) := clockIn.ready
  // read n-2 => snapchain
  rdata(n-2) := snapOut.bits
  rvalid(n-2) := snapOut.valid
  snapOut.ready := ren(n-2)
  // read n-1 => cntrchain
  rdata(n-1) := cntrOut.bits
  rvalid(n-1) := cntrOut.valid
  cntrOut.ready := ren(n-1)
  // snap & cntr control bit <- MSB of addr
  snapCtrl := io.addr(aw-1) 
  cntrCtrl := io.addr(aw-1)
  // writes are ready when the target is stalled 
  for (i <- 0 until outNum) {
    rvalid(i) := stalled
  }
  // reads are ready when the target is stalled
  // with the corresponding address bits 
  for (i <- 0 until inNum) {
    wready(i) := stalled
    when (wen(i)) {
      wdata(i) := io.in.bits
    }
  }
}

abstract class DaisyWrapperTester[+T <: DaisyWrapper[_]](c: T, isTrace: Boolean = true) extends DaisyTester(c, isTrace) {
  val stepAddr = c.n-2
  val clockAddr = c.n-1
  val snapAddr = c.n-2
  val cntrAddr = c.n-1
  val daisyCtrl = c.aw-1

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

  // poke 'clk' to  clock counters
  override def pokeClock (clk: Int) {
    pokeAddr(clockAddr, clk)
  }

  // poke 'n' to the step counter
  // whose address is 'stepAddr'
  override def pokeSteps (n: Int) {
    pokeAddr(stepAddr, n)
  }

  // read(snapAddr) -> snapshot copy
  override def snapCopy() {
    peekAddr(snapAddr)
    showCurrentSnapChain()
  }

  // read(snapAddr | 1 << daisyCtrl) -> snapshot read
  override def snapRead() = {
    val snapValue = peekAddr(snapAddr | 1 << daisyCtrl)
    showCurrentSnapChain()
    snapValue
  }

  override def snapCheck(expected: BigInt) {
    expect(c.io.out.bits, expected)
  }

  // read(cntrAddr) -> counter copy
  override def cntrCopy() {
    peekAddr(cntrAddr)
    showCurrentSnapChain()
  }

  // read(cntrAddr | 1 << daisyCtrl) -> counter read
  override def cntrRead() = {
    val cntrValue = peekAddr(cntrAddr | 1 << daisyCtrl)
    showCurrentSnapChain()
    cntrValue
  }

  override def cntrCheck(expected: BigInt) {
    expect(c.io.out.bits, expected)
  }

  override def dumpName(data: Node): String = {
    if (finished)
      super.dumpName(data) stripPrefix (c.name + ".")
    else
      super.dumpName(data)
  }
}
