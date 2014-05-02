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

object addNode {
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

  def apply[T <: Bits](gen: =>T, name: String = ""): T = {
    val res = gen
    start()
    if (name != "") res.getNode setName name
    finish()
    res
  }

  def apply[T <: Bits](m: Module, gen: => T, name: String = ""): T = {
    val res = apply(gen, name)
    res.getNode.component = m
    res
  }

  def addPin[T <: Data](m: Module, gen: => T, name: String) = {
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

  def addReg[T <: Bits](m: Module, gen: => T, name: String = "") = {
    start()
    val res = gen
    val reg = res.comp match { case r: Reg => r }
    // assign component
    reg.component = m
    // assign clock
    reg.clock = m.clock
    // assign name
    if (name != "") reg setName name
    finish()
    res
  }

  def wire[T <: Bits](regType: T, updates: => Seq[(Bool, Node)]) {
    start()
    val reg = regType.comp match { case r: Reg => r }
    // add updates
    reg.updates ++= updates
    reg.isEnable = !updates.isEmpty
    // set enable signal
    if (reg.isEnable) {
      val (conds, values) = updates.unzip
      reg.enable = addNode(reg.component, (conds.tail foldLeft conds.head)((x, y) => addNode(reg.component, x || y)))
    }
    finish()
  }

  def wire(pair: => ((Node, Node))) {
    start()
    val input = pair._1
    val consumer = pair._2
    if (consumer.inputs.isEmpty) consumer.inputs += input
    else consumer.inputs(0) = input
    finish()
  }
}

import addNode._

// DaisyTransform inserts the step counter and
// the snapshot and evenet counter pins
object DaisyTransform {
  var top: Module = null
  var done = false

  lazy val stepsIn =  addPin(top, Decoupled(UInt(width = 32)).flip, "steps_in")
  lazy val clockIn =  addPin(top, Decoupled(UInt(width = 32)).flip, "clock_in")
  lazy val snapOut =  addPin(top, Decoupled(UInt(width = 32)), "snap_out")
  lazy val snapCtrl = addPin(top, UInt(INPUT, 1), "snap_ctrl")
  lazy val cntrOut =  addPin(top, Decoupled(UInt(width = 32)), "cntr_out")
  lazy val cntrCtrl = addPin(top, UInt(INPUT, 1), "cntr_ctrl")
  lazy val stalled   = addPin(top, Bool(OUTPUT), "stalled")

  val fires = new LinkedHashMap[Clock, HashMap[Module, Bool]]
  val fireBufs = new LinkedHashMap[Clock, HashMap[Module, Bool]]
  val isSteps = new HashMap[Module, Bool]
  val isStepBufs = new HashMap[Module, Bool]
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
  val clkRegs = new HashMap[Clock, UInt]
  val clkCnts = new HashMap[Clock, UInt]

  val daisyNames = HashSet("stalled",
    "steps", "is_step", "is_step_buf",
    "snap_in", "snap_ctrl", "cntr_in", "cntr_ctrl",
    "steps_in_ready", "steps_in_valid", "steps_in_bits",
    "clock_in_ready", "clock_in_valid", "clock_in_bits",
    "snap_out_ready", "snap_out_valid", "snap_out_bits",
    "cntr_out_ready", "cntr_out_valid", "cntr_out_bits",
    "snap_fire", "snap_copy", "snap_read",
    "cntr_fire", "cntr_copy", "cntr_read",
    "clk_num_reg")

  def apply[T <: Module](c: => T, fromDriver: Boolean = false) = {
    top = if (fromDriver) c else Module(c)
    done = true
    addStepAndClkCnts(top)
    addDaisyPins(top)
    top.asInstanceOf[T]
  }

  def addStepAndClkCnts (c: Module) = {
    ChiselError.info("[DaisyTransform] add step and clock counts")
    // step counters
    val steps =  addReg(c, Reg(init = UInt(0, 32)), "steps")
    isSteps(c) = addNode(c, steps.orR, "is_step") 
    val isStepsIn = addNode(c, addNode(c, !isSteps(c)) && stepsIn.valid)
    val decOne = addNode(c, steps - addNode(UInt(1)))
    wire(steps, Seq(isStepsIn -> stepsIn.bits, isSteps(c) -> decOne))

    // generate clock counters for multi clock domains
    if (Driver.clocks.size > 1) { 
      var clkIdx = Driver.clocks count (_.srcClock == null)
      val clkNum    = addReg (c, Reg(init = UInt(clkIdx, 8)), "clk_num_reg")
      val clkNumDec = addNode(c, clkNum - addNode(c, UInt(1)))
      wire(clkNum, Seq(clockIn.valid -> clkNumDec))
      wire(addNode(c, clkNum.orR) -> clockIn.ready)

      for ((clock, idx) <- Driver.clocks.zipWithIndex) {
        val clkRegName = "clock_reg_" + idx
        daisyNames += clkRegName
        clkRegs(clock) = addReg(c, Reg(UInt(width=8)), clkRegName)
        if (clock.srcClock != null) {
          val clkExp = (clock.initStr split " ").tail
          clkExp(0) match {
            case "*" => {
              val clkRegMul = addNode(c, clkRegs(clock.srcClock) * addNode(c, UInt(clkExp(1))))
              wire(clkRegs(clock), Seq(Bool(true) -> clkRegMul))
            }
            case "/" => {
              val clkRegDiv = addNode(c, clkRegs(clock.srcClock) / addNode(c, UInt(clkExp(1))))
              wire(clkRegs(clock), Seq(Bool(true) -> clkRegDiv))
            }
          }
        } else {
          val clkNumEqIdx = addNode(c, clkNum === addNode(c, UInt(clkIdx)))
          val isClkInput =  addNode(c, clockIn.valid && clkNumEqIdx)
          wire(clkRegs(clock), Seq(isClkInput -> clockIn.bits))
          clkIdx = clkIdx - 1
        }
      }
 
      for ((clock, idx) <- Driver.clocks.zipWithIndex) {
        val clkCntName = "clock_cnt_" + idx
        daisyNames += clkCntName
        clkCnts(clock) = addReg(c, Reg(UInt(width = 8)), clkCntName)
      }

      val min = addNode(c, (Driver.clocks foldLeft addNode(c, UInt(1 << 31 - 1)))((mux, clock) => {
        val lessThanMux = addNode(c, clkCnts(clock) < mux)
        addNode(c, Mux(lessThanMux, clkCnts(clock), mux))
        }) )
      c.debug(min) // for debug

      for ((clock, idx) <- Driver.clocks.zipWithIndex) {
        val fireName = "fire_" + idx
        val fireBufName = "fire_buf_" + idx
        daisyNames += fireName
        daisyNames += fireBufName
        val fire    = addNode(c, !addNode(c, clkCnts(clock).orR), fireName)
        val fireBuf = addReg (c, Reg(next=fire), fireBufName)
        fires(clock)    = HashMap(c -> fire)
        fireBufs(clock) = HashMap(c -> fireBuf)
        val notFire     = addNode(c, !fire)
        val isClkCntDec = addNode(c, notFire && isSteps(c))
        val decMin      = addNode(c, clkCnts(clock) - min)
        wire(clkCnts(clock), Seq(isClkCntDec -> decMin, fire -> clkRegs(clock)))
      }
    }
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
        if (Driver.genCounter) {
          cntrIns(m)   = UInt(0)
          cntrOuts(m)  = cntrOut 
          cntrCtrls(m) = cntrCtrl 
        }
        wire(!isSteps(c) -> stepsIn.ready)
      } else {
        isSteps(m) = addPin(m, Bool(INPUT), "is_step")
        if (Driver.isSnapshotting) {
          snapIns(m)   = addPin(m, UInt(INPUT, 32), "snap_in")
          snapOuts(m)  = addPin(m, Decoupled(UInt(width = 32)), "snap_out")
          snapCtrls(m) = addPin(m, UInt(INPUT, 1), "snap_ctrl")
          wire(snapCtrls(m.parent) -> snapCtrls(m))
          wire(snapOuts(m.parent).ready -> snapOuts(m).ready)
        }
        if (Driver.genCounter) {
          cntrIns(m)   = addPin(m, UInt(INPUT, 32), "cntr_in")
          cntrOuts(m)  = addPin(m, Decoupled(UInt(width = 32)), "cntr_out")
          cntrCtrls(m) = addPin(m, UInt(INPUT, 1), "cntr_ctrl")
          wire(cntrCtrls(m.parent) -> cntrCtrls(m))
          wire(cntrOuts(m.parent).ready -> cntrOuts(m).ready)
        }
        wire(isSteps(m.parent) -> isSteps(m))
      }

      // add a 'is_step' buffer used by event counters
      // Event counters should increase one cycle after
      // the target is activated
      isStepBufs(m) = addReg(m, Reg(next=isSteps(m)), "is_step_buf")
      if (m == c) wire((!isSteps(m) && !isStepBufs(m)) -> stalled)
 
      if (m != c && Driver.clocks.size > 1) {
        for ((clock, idx) <- Driver.clocks.zipWithIndex) {
          val fireName = "fire_" + idx
          val fireBufName = "fire_buf_" + idx
          daisyNames += fireName
          daisyNames += fireBufName
          fires(clock)(m)    = addPin(m, Bool(INPUT), fireName)
          fireBufs(clock)(m) = addReg(m, Reg(next=fires(clock)(m)), fireBufName)
          wire(fires(clock)(m.parent) -> fires(clock)(m))
        }
      }

      if (Driver.isSnapshotting) {
        val snapFire = addNode(m, snapOuts(m).ready && addNode(m, !isSteps(m)), "snap_fire")
        snapCopy(m) = addNode(m, snapFire && addNode(m, snapCtrls(m) === addNode(m, Bits(0))), "snap_copy")
        snapRead(m) = addNode(m, snapFire && addNode(m, snapCtrls(m) === addNode(m, Bits(1))), "snap_read")
        wire(snapFire -> snapOuts(m).valid)
      }
      if (Driver.genCounter) {
        val cntrFire = addNode(m, cntrOuts(m).ready && addNode(m, !isSteps(m)), "cntr_fire")
        cntrCopy(m) = addNode(m, cntrFire && addNode(m, cntrCtrls(m) === addNode(m, Bits(0))), "cntr_copy")
        cntrRead(m) = addNode(m, cntrFire && addNode(m, cntrCtrls(m) === addNode(m, Bits(1))), "cntr_read")
        wire(cntrFire -> cntrOuts(m).valid)
      }

      // visit children
      m.children foreach (queue enqueue _)
    }
  }
}

import DaisyTransform._

object DaisyChain extends Backend {
  val keywords = HashSet[String]()

  def apply (b: Backend) {
    b.transforms += ((c: Module) => c bfs (_.addConsumers))
    b.transforms += ((c: Module) => decoupleTarget(top))
    b.transforms += ((c: Module) => appendFires(top))
    if (Driver.genCounter) {
      b.transforms += (c => genCounters)
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
  }

  val ioBuffers = new HashMap[Node, Bits]

  def wire(pair: (Node, Node)) {
    addNode.wire(pair)
  }

  def wire(outType: Bits, updates: (Bool, Node)*) {
    addNode.wire(outType, updates)

    val reg = outType.comp match { case r: Reg => r }
    // genreate muxes
    reg genMuxes reg
    // assign reset
    if (reg.isReset) 
      reg.inputs += reg.component.reset
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
      ioBuffers(targetPin) = addReg(c, Reg(UInt()), bufName)
      if (targetPin.dir == INPUT) {
        // Input buffers work when the clock counter value is set
        wire(ioBuffers(targetPin), stepsIn.valid -> targetPin)
        for (consumer <- targetPin.consumers) {
          val idx = consumer.inputs indexOf targetPin
          consumer.inputs(idx) = ioBuffers(targetPin)
        }
      } else if (targetPin.dir == OUTPUT) {
        wire(ioBuffers(targetPin), isStepBufs(c) -> targetPin.inputs.head)
        wire(ioBuffers(targetPin) -> targetPin)
      }
    }
  }

  def appendFires(c: Module) {
    ChiselError.info("[DaisyChain] append fire signals to Reg and Mem")

    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      val isStepBuf = isStepBufs(m)
      if (Driver.clocks.size > 1) {
        m.clocks.clear
        m.clock = Driver.implicitClock
        m.clocks += Driver.implicitClock
      }

      // Make all delay nodes be enabled by the fire signal
      for (node <- m.nodes ; if !(daisyNames contains node.name)) {
        val fireBuf = 
          if (Driver.clocks.size <= 1 || node.clock == null) Bool(true)
          else fireBufs(node.clock)(m)
        node match {
          // For Reg, different muxes are generated by different backend
          case reg: Reg if Driver.backend.isInstanceOf[VerilogBackend] && 
                           !Driver.isBackannotating => {
            val enable = fireBuf && isStepBuf && reg.enable
            if (reg.isEnable) {
              reg.inputs(reg.enableIndex) = enable.getNode  
            } else {
              reg.isEnable = true
              reg.enableIndex = if (reg.isReset) 2 else 1
              reg.inputs insert (reg.enableIndex, enable.getNode)
            }
            if (Driver.clocks.size > 1)
              reg.clock = m.clock
          }
          case reg: Reg => {
            reg.inputs(0) = Multiplex(fireBuf && isStepBuf, reg.next, reg)
            if (Driver.clocks.size > 1)
              reg.clock = m.clock
          }
          case mem: Mem[_] => {
            for (write <- mem.writeAccesses) {
              val en = Bool()
              val newEn = fireBuf && isStepBuf && en
              wire(write.inputs(1) -> en)
              write.inputs(1) = newEn
            }
            if (Driver.clocks.size > 1)
              mem.clock = m.clock
          }
          case _ =>
        }
      }
 
      m.children foreach (queue enqueue _)
    }
 
    // assign clocks to isStep & fire buffers
    if (Driver.clocks.size > 1) { 
      for ((m, isStepBuf) <- isStepBufs) {
        isStepBuf.comp.clock = m.clock
      }
      for (clock <- Driver.clocks ; (m, fireBuf) <- fireBufs(clock)) {
        fireBuf.comp.clock = m.clock
      }
    }
  }

  def findStates(c: Module) { 
    ChiselError.info("[SnapshotChain] find state elements")
    /*** collect state elements for snapshotting ***/
    // First, collect the top component's inputs
    for ((name, targetPin) <- c.io.flatten ; 
      if !(daisyNames contains name) && targetPin.dir == INPUT) {
        c.states += State(targetPin)
      }

    // Second, collect all the state elements (Reg & Mem)
    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      // collect registers and memory access
      m.nodes foreach { 
        _ match {
          case reg: Reg if !(daisyNames contains reg.name) =>
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
  }

  def addBuffer(c: Module, i: Int, w: Int, update: (Bool, Node)) = {
    val bufName = "buffer_%d".format(i)
    val buffer = addReg(c, Reg(init=UInt(0, w)), bufName)
    daisyNames += bufName
    wire(buffer, update)
    buffer
  }

  def genCounters() {
    ChiselError.info("[CounterBackend] generate counters")

    for (counter <- counters) {
      val signal = counter.signal
      val width = signal.width
      val c = counter.src.comp.component
      val isStep = isSteps(c)
      val isStepBuf = isStepBufs(c)
      val fire = 
        if (Driver.clocks.size <= 1 || signal.clock == null) Bool(true)
        else fires(signal.clock)(c)
      val fireBuf = 
        if (Driver.clocks.size <= 1 || signal.clock == null) Bool(true)
        else fireBufs(signal.clock)(c)
      val signalValue = signal match { 
        case io: Bits => 
          ioBuffers getOrElse (io, io)
        case _ => 
          UInt(signal)
      }
      val cntrValue = counter.cntrT match {
        case Default => counter.src + signalValue
        case Activity => {
          val buffer = addBuffer(c, counter.idx, width, (fireBuf && isStepBuf) -> signalValue)
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
          val buffer = addBuffer(c, counter.idx, width, (fireBuf && isStepBuf) -> signalValue)
          val res = (signalValue ^ buffer) & signalValue
          res.inferWidth = (x: Node) => width
          counter.src + PopCount(res)          
        } 
        case Negedge => {
          val buffer = addBuffer(c, counter.idx, width, (fireBuf && isStepBuf) -> signalValue)
          val res = (signalValue ^ buffer) & (~signalValue)
          res.inferWidth = (x: Node) => width
          counter.src + PopCount(res)          
        }
      }
    
      cntrValue.getNode setName "cntr_val_%d".format(counter.idx)

      /****** Activity Counter *****/
      // 1) fire signal -> increment counter
      // 2) 'copy' control signal when the target is stalled -> reset
      val counterName = "counter_%d".format(counter.idx)
      daisyNames += counterName
      counter.src.comp setName counterName
      wire(counter.src, (fire && isStep) -> cntrValue, cntrCopy(c) -> Bits(0))
    }
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
          daisyNames += shadowName
          chain.last.shadow.comp.component = m 
          chain.last.shadow.comp setName shadowName
          // snap output -> head shadow
          wire(chain.head.shadow -> daisyOut.bits)
          // snap input -> last shadow 
          wire(chain.last.shadow, copy -> realSrc, read -> daisyIn)
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
          daisyNames += shadowName
          chain.last.shadow.comp.component = m 
          chain.last.shadow.comp setName shadowName
          // head shadow -> daisy output
          wire(chain.head.shadow -> daisyOut.bits)
          // daisy input -> last shadow
          wire(chain.last.shadow, copy -> realSrc, read -> headDaisyOut.bits)
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
        daisyNames += shadowName
        s.head.shadow.comp.component = m 
        s.head.shadow.comp setName shadowName
        /****** Shaodw Counter *****/
        // daisy_ctrl == 'copy' -> current source
        // daisy_ctrl == 'read' -> next shadow
        wire(s.head.shadow, copy -> realSrc, read -> s.last.shadow) 
      }
      // visit children
      m.children foreach (queue enqueue _)
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

    /*** Snapshotting and  counter dumpig ***/
    if (t > 0) {
      if (Driver.isSnapshotting) snapshot()
      if (Driver.genCounter) dumpCounters()
    }

    // set clock register
    pokeSteps(n)

    // run the target until it is stalled
    if (isTrace) println("*** CYCLE THE TARGET ***")
    counterVals.clear
    counterVals ++= Array.fill(counters.size)(BigInt(0))
    for (k <- 0 until n) {
      takeSteps(1)
      if (Driver.genCounter) {
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

class DaisyWrapper[+T <: Module](c: => T) extends AXISlave(n = 16 /* 2^(aw - 1) */){
  val top = DaisyTransform(c)
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
  // read & write are ready when the target is stalled 
  for (i <- 0 until n-3) {
    wready(i) := stalled
    rvalid(i) := stalled
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
}
