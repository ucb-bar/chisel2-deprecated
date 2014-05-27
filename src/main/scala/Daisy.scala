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
import scala.math.min

// Counter type definition
trait CounterType
object Default extends CounterType
object Activity extends CounterType
object Ones extends CounterType
object Zeros extends CounterType
object Posedge extends CounterType
object Negedge extends CounterType

object DaisyType {
  val delays = new ArrayBuffer[DelayElem]
  val counters = new ArrayBuffer[EventCounter]
}

abstract class DaisyType(val idx: Int, val limit: Int = 0, val offset: Int = 0) {
  def src: Node
  val shadow = Reg(Bits(width=AXISlave.dw))
}

import DaisyType._

object DelayElem {
  var snapIdx = -1
  def emitSnapIdx = {
    snapIdx = snapIdx + 1
    snapIdx
  }
  def apply(src: Node) = {
    val width = src.width
    val res = new ArrayBuffer[DelayElem]
    if (width > AXISlave.dw) {
      var offset = 0
      while (offset < width) {
        val limit = min(AXISlave.dw-1+offset, width-1)
        val delay = new DelayElem(src, emitSnapIdx, limit, offset)
        delays += delay
        res += delay
        offset += AXISlave.dw
      }
    } else {
      val delay = new DelayElem(src, emitSnapIdx)
      delays += delay
      res += delay
    }
    res
  }
}
class DelayElem(val src: Node, idx: Int, limit: Int = 0, offset: Int = 0) extends DaisyType(idx, limit, offset)

object EventCounter {
  var cntrIdx = -1
  def emitCntrIdx = {
    cntrIdx = cntrIdx + 1
    cntrIdx
  }
  def apply(signal: Node, counterT: CounterType) = {
    val width = signal.width
    val res = new ArrayBuffer[EventCounter]
    if (counterT == Default && width > AXISlave.dw) {
      var offset = 0
      while (offset < width) {
        val limit = min(AXISlave.dw-1+offset, width-1)
        val counter = new EventCounter(signal, counterT, emitCntrIdx, limit, offset)
        counters += counter
        res += counter
        offset += AXISlave.dw
      }
    } else {
      val counter = new EventCounter(signal, counterT, emitCntrIdx)
      counters += counter
      res += counter
    }
    res
  }
}
class EventCounter(val signal: Node, val cntrT: CounterType, 
                   idx: Int, limit: Int = 0, offset: Int = 0) extends DaisyType(idx, limit, offset){
  val src = Reg(init = Bits(0, AXISlave.dw))
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

import addPin.wire

// DaisyTransform inserts the step counter and
// the snapshot and evenet counter pins
object DaisyTransform {
  var top: Module = null
  var done = false
  var inNum = -1
  var outNum = -1
  var stallVal: Bool = null
  var stallAck: Bool = null

  lazy val stepsIn  = addPin(top, Decoupled(UInt(width = AXISlave.dw)).flip, "steps_in")
  lazy val clockIn  = addPin(top, Decoupled(UInt(width = AXISlave.dw)).flip, "clock_in")
  lazy val snapOut  = addPin(top, Decoupled(UInt(width = AXISlave.dw)), "snap_out")
  lazy val snapCtrl = addPin(top, UInt(INPUT, 1), "snap_ctrl")
  lazy val cntrOut  = addPin(top, Decoupled(UInt(width = AXISlave.dw)), "cntr_out")
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
          snapIns(m)   = addPin(m, UInt(INPUT, AXISlave.dw), "snap_in")
          snapOuts(m)  = addPin(m, Decoupled(UInt(width = AXISlave.dw)), "snap_out")
          snapCtrls(m) = addPin(m, UInt(INPUT, 1), "snap_ctrl")
          wire(snapCtrls(m.parent) -> snapCtrls(m))
          wire(snapOuts(m.parent).ready -> snapOuts(m).ready)
        }
        if (Driver.isCounting) {
          cntrIns(m)   = addPin(m, UInt(INPUT, AXISlave.dw), "cntr_in")
          cntrOuts(m)  = addPin(m, Decoupled(UInt(width = AXISlave.dw)), "cntr_out")
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
    "delay_num", "counter_num", "fire_pin", "fire_pin_buf",
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
    b.transforms += ((c: Module) => addStepCounter(c))
    b.transforms += ((c: Module) => setClockDomains(c))
    b.transforms += ((c: Module) => assignIOBuffers(c))
    b.transforms += ((c: Module) => appendFires(top))
    if (Driver.isSnapshotting) {
      b.transforms += (c => findDelays(top))
      b.transforms += (c => genDaisyChain(top, SnapshotChain))
    }
    if (Driver.isCounting) {
      b.transforms += (c => genCounters(top))
      b.transforms += (c => genDaisyChain(top, CounterChain))
    }
    b.transforms += ((c: Module) => c.addClockAndReset)
    b.transforms += ((c: Module) => gatherClocksAndResets)
    b.transforms += ((c: Module) => connectResets)
    b.transforms += ((c: Module) => c.inferAll)
    b.transforms += ((c: Module) => c.forceMatchingWidths)
    b.transforms += ((c: Module) => c.removeTypeNodes)
    b.transforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
    b.analyses   += ((c: Module) => printOutMappings(c))
  }

  def addNode[T <: Bits](m: Module, gen: T, name: String = ""): T = {
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
    val steps  = addReg(c, Reg(init = UInt(0, AXISlave.dw)), "steps")
    val isStep = addNode(c, steps.orR, "is_step")
    updateReg(steps, isStep -> (steps - UInt(1)), stepsIn.valid -> stepsIn.bits)
    if (stallAck != null)
      firePins(c) = addNode(c, isStep || !stallAck, "fire")
    else
      firePins(c) = isStep 

    val queue = ScalaQueue(top)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      if (m != c) {
        firePins(m) = addPin(m, Bool(INPUT), "fire_pin")
        wire(firePins(m.parent) -> firePins(m))
      }

      if (Driver.isSnapshotting) {
        val snapValid = !firePins(m) 
        val snapFire = addNode(m, snapOuts(m).ready && snapValid, "snap_fire")
        snapCopy(m) = addNode(m, firePins(m) /*snapFire && (snapCtrls(m) === Bits(0))*/, "snap_copy")
        snapRead(m) = addNode(m, snapFire && (snapCtrls(m) === Bits(1)), "snap_read")
        if (m != c) {
          wire(snapValid -> snapOuts(m).valid)
        }
      }
      if (Driver.isCounting) {
        val cntrValid = !firePins(m) 
        val cntrFire = addNode(m, cntrOuts(m).ready && cntrValid, "cntr_fire")
        cntrCopy(m) = addNode(m, cntrFire && (cntrCtrls(m) === Bits(0)), "cntr_copy")
        cntrRead(m) = addNode(m, cntrFire && (cntrCtrls(m) === Bits(1)), "cntr_read")
        if (m != c) {
          wire(cntrValid -> cntrOuts(m).valid)
        }
      }
      m.children foreach (queue enqueue _)
    }

    wire(!firePins(top) -> stalled)
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
          isClkInput(clock)             -> clockIn.bits, 
          (!enClk && firePins(c))       -> (clkCnts(clock) - min), 
          (enClk && !isClkInput(clock)) -> clkRegs(clock))
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

  def assignIOBuffers(c: Module) = {
    for ((name, io) <- top.io.asInstanceOf[Bundle].elements) {
      io nameIt (name, true)
    }

    // For daisy wrappers, assign IO addresses and buffers for IOs
    // (except daisy IOs, which are allocated in the frontend)
    if (c.isInstanceOf[DaisyWrapper[_]]) {
      ChiselError.info("[DaisyChain] assign IO addresses")
      val wrapper = c.asInstanceOf[DaisyWrapper[Module]]
      val sortedIOs = top.io.flatten.unzip._2 sortWith (_.width < _.width)
      val ins = new ArrayBuffer[Bits]
      val outs = new ArrayBuffer[Bits]
      var inWidth = 0
      var outWidth = 0
      
      def assignInputs(ins: ArrayBuffer[Bits]) {
        var offset = 0
        // When the input's width is greater than the AXI bus width
        if (ins.head.width > AXISlave.dw) {
          val in = ins.head
          val buffers = new ArrayBuffer[Bits]
          wrapper.ioMap(in) = ((wrapper.waddr, 0))
          while (offset < in.width) {
            wire(stalled -> wrapper.wready(wrapper.waddr))
            updateReg(wrapper.wbuffers(wrapper.waddr), 
                      wrapper.wen(wrapper.waddr) -> wrapper.io.in.bits)
            buffers prepend wrapper.wbuffers(wrapper.waddr)
            offset += AXISlave.dw
            wrapper.waddr += 1
          }
          wire(Cat(buffers) -> in)
        } else {
          wire(stalled -> wrapper.wready(wrapper.waddr))
          for (in <- ins) {
            wrapper.ioMap(in) = ((wrapper.waddr, offset))
            wire(wrapper.wbuffers(wrapper.waddr)(in.width-1+offset, offset) -> in)
            offset += in.width
          }
          updateReg(wrapper.wbuffers(wrapper.waddr), 
                    wrapper.wen(wrapper.waddr) -> wrapper.io.in.bits)
          wrapper.waddr += 1
        }
        ins.clear
      }

      def assignOutputs(outs: ArrayBuffer[Bits]) {
        var offset = 0
        // When the output's width is greater than the AXI bus width
        if (outs.head.width > AXISlave.dw) {
          val out = outs.head
          val buffers = new ArrayBuffer[Bits]
          wrapper.ioMap(out) = ((wrapper.raddr, 0))
          while (offset < out.width) {
            val limit = min(AXISlave.dw-1+offset, out.width-1)
            updateReg(wrapper.rbuffers(wrapper.raddr),
                      firePins(wrapper) -> out(limit, offset))
            wire(wrapper.rbuffers(wrapper.raddr) -> wrapper.rdata(wrapper.raddr))
            wire(stalled                         -> wrapper.rvalid(wrapper.raddr))
            buffers prepend wrapper.rbuffers(wrapper.raddr)
            offset += AXISlave.dw
            wrapper.raddr += 1
          }
        } else {
          for (out <- outs.reverse) {
            wrapper.ioMap(out) = ((wrapper.raddr, offset))
            offset += out.width
          }
          updateReg(wrapper.rbuffers(wrapper.raddr), 
                    firePins(wrapper) -> Cat(outs))        
          wire(wrapper.rbuffers(wrapper.raddr) -> wrapper.rdata(wrapper.raddr))
          wire(stalled                         -> wrapper.rvalid(wrapper.raddr))
          wrapper.raddr += 1
        }
        outs.clear
      }

      for (pin <- sortedIOs ; if !(keywords contains pin.name)) {
        if (pin.dir == INPUT) {
          if (pin.width + inWidth > AXISlave.dw) {
            assignInputs(ins)
            inWidth = 0
          }
          ins += pin
          inWidth += pin.width
        } else if (pin.dir == OUTPUT) {
          if (pin.width + outWidth > AXISlave.dw) {
            assignOutputs(outs)
            outWidth = 0
          }
          outs += pin
          outWidth += pin.width
        }
      }
      if (!ins.isEmpty) assignInputs(ins)
      if (!outs.isEmpty) assignOutputs(outs)
    } 
    // For designs without daisy wrappers,
    // Just insert IO buffers to all IOs execpt daisy IOs
    else {
      ChiselError.info("[DaisyChain] insert IO buffers")
      // For the input and output pins of the 'c' component
      // insert buffers so that their values are avaiable
      // even though the target is stalled
      for ((name, targetPin) <- c.io.flatten; if !(keywords contains name)) {
        val bufName = name + "_buf"
        keywords += bufName
        if (targetPin.dir == INPUT) {
          ioBuffers(targetPin) = addReg(c, Reg(UInt()), bufName)
          updateReg(ioBuffers(targetPin), (stepsIn.valid || firePins(c)) -> targetPin)
          for (consumer <- targetPin.consumers) {
            val idx = consumer.inputs indexOf targetPin
            if (idx >= 0) consumer.inputs(idx) = ioBuffers(targetPin)
          }
        } else if (targetPin.dir == OUTPUT && !targetPin.inputs.isEmpty) {
          val pinInput = targetPin.inputs.head.getNode
          val pinWidth = targetPin.width
          ioBuffers(targetPin) = addReg(c, Reg(UInt(width = pinWidth)), bufName)
          updateReg(ioBuffers(targetPin), firePins(c) -> pinInput)
          wire(ioBuffers(targetPin) -> targetPin)
        }
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
          if (Driver.clocks.size <= 1 || node.clock == null) firePins(m) 
          else firePins(m) && enClks(node.clock)(m) 
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

  def findDelays(c: Module) { 
    ChiselError.info("[SnapshotChain] find delay elements")
    /*** collect inputs ***/
    for ((n, io) <- c.io.flatten; if io.dir == INPUT && !(keywords contains io.name)) {
      c.delays ++= DelayElem(io)
    }

    /*** collect delay elements(Reg/Mem) for snapshotting ***/
    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      // collect registers and memory access
      m.nodes foreach { 
        _ match {
          case reg: Reg if !(keywords contains reg.name) => 
            m.delays ++= DelayElem(reg)
          case mem: Mem[_] =>
            for (i <- 0 until mem.size)
              m.delays ++= DelayElem(new MemRead(mem, UInt(i)))
          case _ =>
        }
      }
      // visit children
      m.children foreach (queue enqueue _)
    }

    // turn off the snapOut valid signal after reading out all the values 
    val delayNum = addReg(c, Reg(UInt(width=32)), "delay_num")
    updateReg(delayNum, 
      (snapRead(c) && delayNum.orR) -> (delayNum - UInt(1)),
      stepsIn.valid -> UInt(delays.size))
    wire((snapCopy(c) || delayNum.orR) -> snapOuts(c).valid)
  }

  def addBuffer(c: Module, i: Int, w: Int, updates: (Bool, Node)) = {
    val bufName = "buffer_%d".format(i)
    val buffer = addReg(c, Reg(init=UInt(0, w)), bufName)
    keywords += bufName
    updateReg(buffer, updates)
    buffer
  }

  def genCounters(c: Module) {
    ChiselError.info("[CounterChain] generate event counters")

    val queue = ScalaQueue(c)
    while (!queue.isEmpty) {
      val m = queue.dequeue
      m.events foreach (event => m.counters ++= EventCounter(event._1, event._2))
      for (daisyType <- m.counters) {
        val counter = daisyType.asInstanceOf[EventCounter]
        val signal = counter.signal
        val width = signal.width
        val fire = 
          if (Driver.clocks.size <= 1 || signal.clock == null) firePins(m) 
          else firePins(m) && enClks(signal.clock)(m) 
        val signalValue = signal match { 
          case io: Bits if counter.limit - counter.offset > 0 => 
            (ioBuffers getOrElse (io, io))(counter.limit, counter.offset)
          case io: Bits => 
            ioBuffers getOrElse (io, io)
          case _ if counter.limit - counter.offset > 0 => 
            UInt(signal)(counter.limit, counter.offset)
          case _ => 
            UInt(signal)
        }
        val cntrValue = counter.cntrT match {
          case Default => counter.src + signalValue
          case Activity => {
            val buffer = addBuffer(m, counter.idx, width, fire -> signalValue)
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
            val buffer = addBuffer(m, counter.idx, width, fire -> signalValue)
            val res = (signalValue ^ buffer) & signalValue
            res.inferWidth = (x: Node) => width
            counter.src + PopCount(res)          
          } 
          case Negedge => {
            val buffer = addBuffer(m, counter.idx, width, fire -> signalValue)
            val res = (signalValue ^ buffer) & (~signalValue)
            res.inferWidth = (x: Node) => width
            counter.src + PopCount(res)          
          }
        }
    
        cntrValue.getNode setName "cntr_val_%d".format(counter.idx)

        /* Event Counter */
        // 1) steps > 0 -> increment counters
        // 2) A copy bit with the target's stall -> reset
        val counterName = "counter_%d".format(counter.idx)
        keywords += counterName
        addReg(m, counter.src, counterName)
        updateReg(counter.src, firePins(m) -> cntrValue, cntrCopy(m) -> Bits(0))
      }

      m.children foreach (queue enqueue _)
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
    ChiselError.info("[%sChain] generate daisy chains".format(
      chainT match {
        case SnapshotChain => "Snapshot"
        case CounterChain  => "Counter"
      } ))
 
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
        case SnapshotChain => m.delays
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
          val limit = chain.last.limit
          val offset = chain.last.offset
          val src = chain.last.src match {
            case bits: Bits if chainT == SnapshotChain && limit - offset > 0 =>
              (ioBuffers getOrElse (bits, bits))(limit, offset)
            case bits: Bits => 
              ioBuffers getOrElse (bits, bits)
            case any if chainT == SnapshotChain && limit - offset > 0 =>
              UInt(any)(limit, offset)
            case any => any
          } 
          val shadowName = ( chainT match {
            case SnapshotChain => "snap_shadow_"
            case CounterChain  => "cntr_shadow_"
          } ) + chain.last.idx
          keywords += shadowName
          // snap output -> head shadow
          wire(chain.head.shadow -> daisyOut.bits)
          // snap input -> last shadow 
          addReg(m, chain.last.shadow, shadowName)
          updateReg(chain.last.shadow, copy -> src, read -> daisyIn)
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
          val limit = chain.last.limit
          val offset = chain.last.offset
          val src = chain.last.src match {
            case bits: Bits if chainT == SnapshotChain && limit - offset > 0 =>
              (ioBuffers getOrElse (bits, bits))(limit, offset)
            case bits: Bits => 
              ioBuffers getOrElse (bits, bits)
            case any if chainT == SnapshotChain && limit - offset > 0 =>
              UInt(any)(limit, offset)
            case any => any
          } 
          val shadowName = ( chainT match {
            case SnapshotChain => "snap_shadow_"
            case CounterChain  => "cntr_shadow_"
          } ) + chain.last.idx
          keywords += shadowName
          // head shadow -> daisy output
          wire(chain.head.shadow -> daisyOut.bits)
          // daisy input -> last shadow
          addReg(m, chain.last.shadow, shadowName)
          updateReg(chain.last.shadow, copy -> src, read -> headDaisyOut.bits)
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
        val limit = s.head.limit
        val offset = s.head.offset
        val src = s.head.src match {
          case bits: Bits if chainT == SnapshotChain && limit - offset > 0 =>
            (ioBuffers getOrElse (bits, bits))(limit, offset)
          case bits: Bits => 
            ioBuffers getOrElse (bits, bits)
          case any if chainT == SnapshotChain && limit - offset > 0 =>
            UInt(any)(limit, offset)
          case any => any
        }
        val shadowName = ( chainT match {
          case SnapshotChain => "snap_shadow_"
          case CounterChain  => "cntr_shadow_"
        } ) + s.head.idx
        keywords += shadowName
        /* Shaodw Counter */
        // daisy_ctrl == 'copy' -> current source
        // daisy_ctrl == 'read' -> next shadow
        addReg(m, s.head.shadow, shadowName)
        updateReg(s.head.shadow, copy -> src, read -> s.last.shadow) 
      }
      // visit children
      m.children foreach (queue enqueue _)
    }
  }

  def printOutMappings (c: Module) {
    ChiselError.info("[DaisyChain] print out mappings")
    val wrapperPrefix = top.name + "Wrapper."
    if (c.isInstanceOf[DaisyWrapper[_]]) {
      val iomap = createOutputFile(c.name + ".iomap")
      val wrapper = c.asInstanceOf[DaisyWrapper[Module]]
      val (ins, outs) = wrapper.ioMap partition (_._1.dir == INPUT)
      val res = new StringBuilder
      res append "inputs\n"
      for ((pin, (addr, off)) <- ins) {
        res append ("%s %d %d %d\n".format(
          pin.chiselName stripPrefix wrapperPrefix,
          pin.width, addr, off) )
      }
      res append "outputs\n"
      for ((pout, (addr, off)) <- outs) {
        res append ("%s %d %d %d\n".format(
          pout.chiselName stripPrefix wrapperPrefix,
          pout.width, addr, off) )
      }
      try {
        iomap write res.result
      } finally {
        iomap.close
      }
    }
    if (Driver.isSnapshotting) {
      val snapchain = createOutputFile(c.name + ".snap.chain")
      val res = new StringBuilder
      for (delay <- delays) {
        val limit = delay.limit
        val offset = delay.offset
        delay.src match {
          case read: MemRead if limit - offset > 0 => 
            res append ("%s[%s](%d,%d) %d %d\n".format(
              read.mem.chiselName stripPrefix wrapperPrefix, 
              read.addr.litValue(0), limit, offset, read.mem.width, delay.idx) ) 
          case read: MemRead => 
            res append ("%s[%s] %d %d\n".format(
              read.mem.chiselName stripPrefix wrapperPrefix, 
              read.addr.litValue(0), read.mem.width, delay.idx) ) 
          case any if limit - offset > 0 =>
            res append ("%s(%d,%d) %d %d\n".format(
              any.chiselName stripPrefix wrapperPrefix,
              limit, offset, any.width, delay.idx)) 
          case any =>
            res append ("%s %d %d\n".format(
              any.chiselName stripPrefix wrapperPrefix,
              any.width, delay.idx)) 
        }
      }
      try {
        snapchain write res.result
      } finally {
        snapchain.close
      }
    }
    if (Driver.isCounting) {
      val cntrchain = createOutputFile(c.name + ".cntr.chain")
      val res = new StringBuilder
      for (counter <- counters) {
        val limit = counter.limit
        val offset = counter.offset
        if (limit - offset > 0) {
          res append ("%s(%d,%d) %d %d\n".format(
            counter.signal.chiselName stripPrefix wrapperPrefix, 
            limit, offset, counter.signal.width, counter.idx))
        } else {
          res append ("%s %d %d\n".format(
            counter.signal.chiselName stripPrefix wrapperPrefix, 
            counter.signal.width, counter.idx))
        }
      }
      try {
        cntrchain write res.result
      } finally {
        cntrchain.close
      }
    }
  }  
}

abstract class DaisyTester[+T <: Module](c: T, isTrace: Boolean = true, val snapsize: Int = 10) extends Tester(c, isTrace) {
  require(DaisyTransform.done)
  val delayPeeks = new ArrayBuffer[BigInt]
  val counterVals = new ArrayBuffer[BigInt]
  val counterPeeks = new ArrayBuffer[BigInt]
  val clockVals = new LinkedHashMap[Clock, Int]
  val clockCnts = new LinkedHashMap[Clock, Int]

  override val outputs = top.io.flatten.unzip._2 filter (x => {
    val name = x.chiselName.split('.').last
    x.dir == OUTPUT && !(DaisyChain.keywords contains (name stripPrefix "io_"))
  })
  var isInSnapshot = false
  var snapCount = 0

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

  def takeSteps (n: Int) {
    val clk = emulatorCmd("step %d".format(n))
    if (isTrace) println("  STEP %d".format(n))
  }

  // set clock counters
  def pokeClock (clk: Int) {
    while (peek(clockIn.ready) == 0) {
      takeSteps(1)
    }
    pokeBits(clockIn.bits, clk)
    pokeBits(clockIn.valid, 1)
    takeSteps(1)
    pokeBits(clockIn.valid, 0)
  }

  def pokeSteps (n: Int) {
    // Wait until the clock counter is ready
    // (the target is stalled)
    while(peek(stalled) == 0) {
      takeSteps(1)
    }
    // Set the step counter
    pokeBits(stepsIn.bits, n)
    pokeBits(stepsIn.valid, 1)
    takeSteps(1)
    pokeBits(stepsIn.valid, 0)
  }

  // Show me the current status of the daisy chain
  def showCurrentSnapChain() = {
    if (isTrace && !delays.isEmpty) {
      println("--- CURRENT SNAPSHOT CHAIN ---")
      delays foreach (x => peek(x.shadow))
      println("------------------------------")
    }
  }

  def snapCopy() {
    do {
      pokeBits(snapCtrl, 0)
      pokeBits(snapOut.ready, 1)
      takeSteps(1)
    } while (peek(snapOut.valid) == 0)
    pokeBits(snapOut.ready, 0)
    showCurrentSnapChain()
  }

  def snapRead() = {
    do {
      pokeBits(snapCtrl, 1)
      pokeBits(snapOut.ready, 1)
      takeSteps(1)
    } while (peek(snapOut.valid) == 0)
    pokeBits(snapOut.ready, 0)
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
      pokeBits(cntrCtrl, 0)
      pokeBits(cntrOut.ready, 1)
      takeSteps(1)
    } while (peek(cntrOut.valid) == 0)
    pokeBits(cntrOut.ready, 0)
    showCurrentCntrChain()
  }

  def cntrRead() = {
    do {
      pokeBits(cntrCtrl, 1)
      pokeBits(cntrOut.ready, 1)
      takeSteps(1)
    } while (peek(cntrOut.valid) == 0)
    pokeBits(cntrOut.ready, 0)
    showCurrentCntrChain()
    peek(cntrOut.bits)
  }

  def cntrCheck(expected: BigInt) {
    expect(cntrOut.bits, expected)
  }

  override def poke(data: Bits, x: BigInt) {
    if (isInSnapshot) 
      addPoke(snapshots, t, Poke(data, -1, x))
    else 
      addPoke(pokez, t, Poke(data, -1, x))
    super.pokeBits(data, x)
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

  override def dumpName(data: Node): String = {
    if (data.name != "")
      data.chiselName
    else
      data.component.getPathName(".") + "." + Driver.backend.emitRef(data)    
  }

  override def dump(): Snapshot = {
    val snap = new Snapshot(t)
    var value = BigInt(0)
    for ((delay, i) <- delays.zipWithIndex) {
      if (isTrace) println("*** Snapshot Chain Read %d ***".format(i))
      // Read out the snap chain
      val snapValue = snapRead()
      // Check the snap output
      snapCheck(delayPeeks(i))
      delay.src match {
        case read: MemRead if delay.offset > 0 => {
          value |= (snapValue << delay.offset)
          snap.pokes -= snap.pokes.last
          snap.pokes += Poke(read.mem, read.addr.litValue(0).toInt, value)
        }
        case read: MemRead => {
          value = snapValue
          snap.pokes += Poke(read.mem, read.addr.litValue(0).toInt, value)
        }
        case _ if delay.offset > 0 => {
          value |= (snapValue << delay.offset)
          snap.pokes -= snap.pokes.last
          snap.pokes += Poke(delay.src, -1, value)
        }
        case _ => {
          value = snapValue
          snap.pokes += Poke(delay.src, -1, value)
        }
      }
    }

    snap
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


  def extract(value: BigInt, width: Int, offset: Int) =
    (value & (((BigInt(1) << width) - 1) << offset)) >> offset

  override def step (n: Int = 1) { 
    if (isTrace) {
      println("-------------------------")
      println("|   Daisy Chain Step    |")
      println("-------------------------")
    }

    /*** Snapshotting and counter dumpig ***/
    if (t > 0) {
      if (Driver.isCounting) dumpCounters()
    }

    // set the step counter
    pokeSteps(n)

    // run the target until it is stalled
    if (isTrace) println("*** CYCLE THE TARGET ***")
    counterVals.clear
    counterVals ++= Array.fill(counters.size)(BigInt(0))
    for (k <- 0 until n) {
      propagate()
      if (Driver.isCounting) {
        if (isTrace) println("*** READ COUNTER SIGNALS ***")
        for ((counter, i) <- counters.zipWithIndex) {
          val curPeek = counter.signal match {
            case read: MemRead if counter.limit - counter.offset > 0 =>
               extract(peekBits(read.mem, read.addr.litValue(0).toInt),
                       counter.limit - counter.offset + 1, counter.offset)
            case read: MemRead =>
               peekBits(read.mem, read.addr.litValue(0).toInt)
            case signal if counter.limit - counter.offset > 0 =>
              extract(peekBits(signal),
                      counter.limit - counter.offset + 1, counter.offset)
            case signal =>
              peekBits(signal) }
          counterVals(i) += calcCounterVal(counter, curPeek, counterPeeks(i))
          counterPeeks(i) = curPeek
        }
      }
      if (Driver.isSnapshotting) {
        if (isTrace) println("*** READ STATE VALUES ***")
        delayPeeks.clear
        delayPeeks ++= delays map { delay => delay.src match {
          case read: MemRead if delay.limit - delay.offset > 0 =>
            extract(peekBits(read.mem, read.addr.litValue(0).toInt), 
                    delay.limit - delay.offset + 1, delay.offset)
          case read: MemRead =>
            peekBits(read.mem, read.addr.litValue(0).toInt)
          case signal if delay.limit - delay.offset > 0 =>
            extract(peekBits(signal), 
                    delay.limit - delay.offset + 1, delay.offset)
          case signal =>
            peekBits(signal) }
        }
      }
      tick()
    }

    while (peek(stalled) == 0)
      takeSteps(1)

    val dice = rnd.nextInt(30)

    /*** Snapshot Sampling ***/
    if (Driver.isSnapshotting && dice == 0 && !isInSnapshot) {
      // take a snapshot
      snapshot()
      isInSnapshot = true
      snapCount = snapsize
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

    if (isInSnapshot && snapCount > 0) {
      snapCount -= 1
    } else if (isInSnapshot) {
      addExpects(snapshots)
      isInSnapshot = false 
    }
  }

  override def finish(): Boolean = {
    if (isInSnapshot) addExpects(snapshots)
    dumpSnapshots(c.name + ".snaps", snapshots)
    super.finish()
  }
}

object AXISlave {
  val aw = 5
  val dw = 32
}

abstract class AXISlave(val n: Int = 32 /* 2^aw */) extends Module {
  val io = new Bundle {
    val in = Decoupled(Bits(width = AXISlave.dw)).flip
    val out = Decoupled(Bits(width = AXISlave.dw))
    val addr = Bits(INPUT, AXISlave.aw)
  }

  def wen(i: Int) = io.in.valid && io.addr(log2Up(n)-1, 0) === UInt(i)
  def ren(i: Int) = io.out.ready && io.addr(log2Up(n)-1, 0) === UInt(i)
  val rdata = Vec.fill(n){Bits(width = AXISlave.dw)}
  val rvalid = Vec.fill(n){Bool()}
  val wready = Vec.fill(n){Bool()}

  io.in.ready  := wready(io.addr)
  io.out.valid := rvalid(io.addr)
  io.out.bits  := rdata(io.addr)
}

abstract class DaisyWrapper[+T <: Module](c: => T) extends AXISlave(n = 16 /* 2^(aw - 1) */){
  val top = DaisyTransform(c)
  val wbuffers = Vec.fill(n-3) { Reg(UInt()) }
  val rbuffers = Vec.fill(n-3) { Reg(UInt()) }
  val ioMap = new HashMap[Bits, (Int, Int)]
  var raddr = 0
  var waddr = 0
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
  snapCtrl := io.addr(AXISlave.aw-1) 
  cntrCtrl := io.addr(AXISlave.aw-1)
}

abstract class DaisyWrapperTester[+T <: DaisyWrapper[_]](c: T, isTrace: Boolean = true) extends DaisyTester(c, isTrace) {
  val stepAddr = c.n-2
  val clockAddr = c.n-1
  val snapAddr = c.n-2
  val cntrAddr = c.n-1
  val daisyCtrl = AXISlave.aw-1
  val peekValues = new HashMap[Int, BigInt] 
  val pokeValues = new HashMap[Int, BigInt]
  var isPoked = false
  for (i <- 0 until c.waddr) {
    pokeValues(i) = 0
  }

  override def poke(data: Bits, x: BigInt) {
    if (isInSnapshot) addPoke(snapshots, t, Poke(data, -1, x))
    var (addr, off) = c.ioMap(data)
    if (data.width > AXISlave.dw) {
      while (off < data.width) {
        pokeValues(addr) = extract(x, AXISlave.dw, off)
        off += AXISlave.dw
        addr += 1
      }
    } else {
      pokeValues(addr) = pokeValues(addr) & ~(((BigInt(1) << data.width) - 1) << off)
      pokeValues(addr) = pokeValues(addr) | (x << off)
    }
    if (isTrace) println("  POKE %s <- %d".format(dumpName(data), x))
    isPoked = true
  }

  override def peek(data: Bits) = {
    if (c.ioMap contains data) {
      var (addr, off) = c.ioMap(data)
      if (data.width > AXISlave.dw) {
        var res = BigInt(0)
        while (off < data.width) {
          res = res | (peekValues(addr) << off)
          off += AXISlave.dw
          addr += 1
        }
        res
      } else {
        val res = signed_fix(data, extract(peekValues(addr), data.width, off)) 
        if (isTrace) println("  PEEK %s -> %d".format(dumpName(data), res))
        res
      }
    } else super.peek(data)
  }

  override def step(n: Int) = {
    if (isPoked) {
      if (isTrace) println("*** POKES ***")
      for (i <- 0 until c.waddr) {
        pokeAddr(i, pokeValues(i))
      }
      isPoked = false
    }

    super.step(n)

    if (isTrace) println("*** PEEKS ***")
    for (i <- 0 until c.raddr) {
      peekValues(i) = peekAddr(i) 
    }
  }

  // poke 'bits' to the address 'addr'
  def pokeAddr(addr: BigInt, bits: BigInt) {
    do {
      pokeBits(c.io.addr, addr)
      takeSteps(1)
    } while (peek(c.io.in.ready) == 0)
    pokeBits(c.io.in.bits, bits)
    pokeBits(c.io.in.valid, 1)
    takeSteps(1)
    pokeBits(c.io.in.valid, 0)
  }

  // peek the signal from the address 'addr'
  def peekAddr(addr: BigInt) = {
    do {
      pokeBits(c.io.addr, addr)
      pokeBits(c.io.out.ready, 1)
      takeSteps(1)
    } while (peek(c.io.out.valid) == 0)

    peek(c.io.out.bits)
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
    showCurrentCntrChain()
  }

  // read(cntrAddr | 1 << daisyCtrl) -> counter read
  override def cntrRead() = {
    val cntrValue = peekAddr(cntrAddr | 1 << daisyCtrl)
    showCurrentCntrChain()
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

  override def reset(n: Int) {
    super.reset(n)
    if (t > 1) for (i <- 0 until c.waddr) pokeValues(i) = 0
  }

  var finished = false
  override def finish(): Boolean = {
    finished = true
    super.finish()
  }
}
