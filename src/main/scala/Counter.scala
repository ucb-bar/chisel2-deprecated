package Chisel

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.math.pow
import scala.io.Source

object AXISlave {
  val aw = 5
  val dw = 32
  val daisyw = 1
  val n = pow(2, aw-daisyw).toInt
  val signals = new ArrayBuffer[Node]
}

abstract class AXISlave extends Module {
  def top: Module
  val io = new Bundle {
    val in = Decoupled(Bits(width = AXISlave.dw)).flip
    val out = Decoupled(Bits(width = AXISlave.dw))
    val addr = Bits(INPUT, AXISlave.aw)
  }
  def wen(i: Int) = io.in.valid && io.addr(log2Up(AXISlave.n)-1, 0) === UInt(i)
  def ren(i: Int) = io.out.ready && io.addr(log2Up(AXISlave.n)-1, 0) === UInt(i)
  val rdata = Vec.fill(AXISlave.n){Bits(width = AXISlave.dw)}
  val rvalid = Vec.fill(AXISlave.n){Bool()}
  val wready = Vec.fill(AXISlave.n){Bool()}
  
  io.in.ready := wready(io.addr)
  io.out.valid := rvalid(io.addr)
  io.out.bits := rdata(io.addr) 
}

/*
class AXISlaveTester(s: AXISlave) extends Tester(s, Array(s.io) ++ s.signals ++ (Module.clocks filter (_.isEnabled)) ) {
  defTests {
    val (a, b, z) = (80, 16, 16)
    val svars = new HashMap[Node, Node]
    val ovars = new HashMap[Node, Node]
    val counters = new HashMap[Int, Int]

    for (i <- 0 until 16) {
      counters(i) = 0
    }

    var t = 0
    svars.clear
    // Set inputs
    svars(s.io.addr) = Bits(0 << 2)
    svars(s.io.in.bits) = Bits(80 << 16 | 16)
    svars(s.io.in.valid) = Bool(true)
    svars(s.io.out.ready) = Bool(false)
    step(svars, ovars)
    svars(s.io.in.bits) = Bits(0)
    svars(s.io.in.valid) = Bool(false)
    step(svars, ovars)
    step(svars, ovars)
//    do {
      val first = (t == 0)
      svars(c.io.addr) = Bits(1 << 2)
      svars(c.io.in.bits) = Bits(1)
      svars(c.io.in.valid) = Bool(true)
      step(svars, ovars)
      svars(c.io.addr) = Bits(2 << 2 | 1)
      svars(c.io.out.ready) = Bool(true)
      step(svars, ovars)
      svars(c.io.addr) = Bits(2 << 2 | 2)
      for (i <- 0 until 16) {
        step(svars, ovars)
        counters(i) = counters(i) + ovars(s.io.out.bits).litValue().toInt
      }
      svars(c.io.addr) = Bits(3 << 2 | 2)
      step(svars, ovars)
//    } while (t <= 1 || ovars(c.io.out.valid) == Bool(true))
    for (i <- 0 until 16) {
      ChiselError.info("counter%d: %d".format(i, counters(i)))
    }
    ovars(s.io.out.bits) == Bits(z)
  }
}
*/

trait CounterBackend extends Backannotation {
  val crosses = new ArrayBuffer[(Double, Array[Node])]
  val dreadyPairs = new HashMap[Module, (Bool, Bool)]

  var counterIdx = -1

  // Define different clock domains for the daisy chain
  val daisyClock = Module.implicitClock
  val stopClock = -daisyClock

  preElaborateTransforms += ((c: Module) => Module.components map (_.addDefaultReset))
  preElaborateTransforms += ((c: Module) => c bfs (_.addConsumers))
  preElaborateTransforms += ((c: Module) => annotateSignals(c))
  preElaborateTransforms += ((c: Module) => generateCtrls(c))
  preElaborateTransforms += ((c: Module) => generateCounters)
  preElaborateTransforms += ((c: Module) => generateDaisyChains)
  preElaborateTransforms += ((c: Module) => c.genAllMuxes)
  // preElaborateTransforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  // analyses += ((c: Module) => findClockCrossingPoints(c))
  // analyses += ((c: Module) => initTester(c))
  // analyses += ((c: Module) => reportCounters(c))


  def emitCounterIdx = {
    counterIdx = counterIdx + 1
    counterIdx
  }

  def getTypeNode(node: Node) = {
    def genTypeNode(node: Node) = {
      val typeNode = Bits()
      typeNode.inputs += node
      for (consumer <- node.consumers) {
        consumer.inputs -= node
        consumer.inputs += typeNode
      }
      typeNode
    }

    node match {
      case bits: Bits => bits
      case _ if node.consumers.size == 1=> {
        node.consumers.head match {
          case bits: Bits => bits
          case _ => genTypeNode(node)
        }
      }
      case _ => genTypeNode(node)
    } 
  }

  def addPin(m: Module, pin: Data, name: String = "") {
    pin.component = m
    pin.isIo = true
    pin setName name

    pin match {
      case dio: DecoupledIO[_] => {
        dio.ready.component = m
        dio.valid.component = m
        dio.bits.component = m
        dio.ready.isIo = true
        dio.valid.isIo = true
        dio.bits.isIo = true
        dio.ready setName (name + "_ready")
        dio.valid setName (name + "_valid")
        dio.bits setName (name + "_bits")
      }
      case vio: ValidIO[_] => {
        vio.valid.component = m
        vio.bits.component = m
        vio.valid.isIo = true
        vio.bits.isIo = true
        vio.valid setName (name + "_valid")
        vio.bits setName (name + "_bits")
      }
      case _ =>
    }
 
    (m.io) match {
      case io: Bundle => 
        io += pin
      case _ =>
    }
  }

  private def annotateSignals(m: Module) {
    ChiselError.info("Backannotation: annotate signals")

    // Read the signal list file
    // TODO: generalize the signal file format
    val lines = Source.fromFile(Module.signalFilename).getLines
    val TermRegex = """\s*([\w\._\:]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)""".r
    val signalNames = new HashSet[String]
    val signalNameMap = new HashMap[String, Node]
    val coeffs = new HashSet[(Double, Array[String])]

    for (line <- lines) {
      line match {
        case TermRegex(exp, coeff, se, tstat, pvalue) => {
          val vars = exp split ":"
          if (tstat != "NaN" && pvalue != "NaN") {
            signalNames ++= vars
            coeffs += ((coeff.toDouble, vars))
          }
        }
        case _ =>
      }
    }

    // Find correspoinding nodes
    for (m <- Module.components ; if !m.isInstanceOf[AXISlave]) {
      val reset = m.reset
      val resetName = getSignalPathName(reset, ".")
      if (signalNames contains resetName) {
        m.signals += reset
        signalNameMap(resetName) = reset
        if (!(m.debugs contains reset))
          m.debugs += reset
      }
      for (node <- m.nodes ; if !node.isTypeNode) {
        val signalName = getSignalPathName(node, ".")
        if (signalNames contains signalName) {
          m.signals += node
          signalNameMap(signalName) = node
          if (!(m.debugs contains node))
            m.debugs += node
        }
      }
    }
   
    for ((coeff, vars) <- coeffs) {
      val cross = vars map { x => signalNameMap getOrElse (x, null) }
      if (!(cross contains null)) {
        crosses += ((coeff, cross))
      }
    } 
  }

  def generateCtrls(c: Module) {
    ChiselError.info("Counter Backend: generate daisy controls")

    // Add pins
    for (m <- Module.components ; if !m.isInstanceOf[AXISlave]) {
      val stall = Bool(INPUT)
      val daisyControl = Bits(INPUT, width = AXISlave.daisyw)
      val daisyOut = Decoupled(Bits(width = AXISlave.dw))
      val daisyIn = Valid(Bits(width = AXISlave.dw)).flip
      
      def daisyReady(i: Int) = {
        val dready = stall && daisyOut.ready && daisyControl === Bits(i)
        dready.getNode.component = m
        if (i == 0) { dready.getNode setName "copy" }
        if (i == 1) { dready.getNode setName "read" }
        if (!(m.debugs contains dready.getNode))
          m.debugs += dready.getNode
        dready
      }

      m.clocks.clear
      m.clocks += daisyClock

      dreadyPairs(m) = (daisyReady(0), daisyReady(1))

      addPin(m, stall,        "stall")
      addPin(m, daisyControl, "daisy_control")
      addPin(m, daisyOut,     "daisy_out")
      addPin(m, daisyIn,      "daisy_in")

      if (!m.children.isEmpty) {
        val notStop = Bool(INPUT)
        // val init = Bool(INPUT)
        addPin(m, notStop, "not_stop")
        // addPin(m, init,    "init")
        val childClock = new Clock
        val enable = (!stall && notStop || m.reset) 
        enable.getNode.component = m
        childClock setName ("emul_clk_" + m.level)
        childClock enabledBy (daisyClock, enable)
        for (child <- m.children) {
          child.clock = childClock
        }
      }

      daisyOut.valid.updates += ((Bool(true), stall))
      // daisyOut.valid.updates += ((Bool(true), daisyReady(2)))
    }

    val slave = c match {
      case s: AXISlave => {
        initializeSlave(s)
        s
      }
    }
    val stack = new Stack[Module]
    val walked = new HashSet[Module]
    stack push slave.top

    while (!stack.isEmpty) {
      val m = stack.pop
      connectDaisyPins(m)
      for (child <- m.children) {
        stack push child
      }
    }
  }

  def initializeSlave (slave: AXISlave) {
    // AXISlave & daisy  pins 
    val slaveAddr = slave.io("addr") match {
      case bits: Bits => bits }
    val slaveIn = slave.io("in") match {
      case dio: DecoupledIO[_] => dio }
    val slaveInBits = slaveIn.bits match {
      case bits: Bits => bits }
    val daisyReady = slave.ren(4) match {
      case bool: Bool => bool }
    val clkReady = slave.wen(4) match {
      case bool: Bool => bool }
    val daisyIn = slave.top.io("daisy_in") match {
      case vio: ValidIO[_] => vio }
    val daisyOut = slave.top.io("daisy_out") match {
      case dio: DecoupledIO[_] => dio }
    val daisyOutBits = daisyOut.bits match {
      case bits: Bits => bits }

    val clkCounter = Reg(init = Bits(0), clock = daisyClock)
    val notStall = clkCounter.orR
    val stall = !notStall
    clkCounter.comp.component = slave
    clkCounter.comp setName "clk_counter"
    notStall.getNode.component = slave
    stall.getNode.component = slave
    stall.getNode setName "stall"

    val stop = Reg(init = Bool(false), clock = stopClock) // works at negative edges
    stop.comp.component = slave
    stop.comp setName "stop" 

    slave.top.io("daisy_control") match {
      case bits: Bits => bits.updates   += ((Bool(true), slaveAddr(AXISlave.aw - AXISlave.daisyw))) } 
    slave.top.io("stall") match {
      case bool: Bool => bool.updates   += ((Bool(true), stall)) }
    if (!slave.top.children.isEmpty) {
      slave.top.io("not_stop") match {
        case bool: Bool => bool.updates += ((Bool(true), !stop)) }
    }

    // top's daisy input pin <= 0
    daisyIn.bits match {
      case bits: Bits => bits.updates += ((Bool(true), Bits(0))) }
    daisyIn.valid.updates             += ((Bool(true), daisyReady && stall))

    // read cr4 -> daisy_out
    slave.rdata(4) match {
      case bits: Bits => bits.updates += ((Bool(true), daisyOutBits)) }
    slave.rvalid(4) match {
      case bool: Bool => bool.updates += ((Bool(true), daisyOut.valid)) }
    daisyOut.ready.updates            += ((Bool(true), daisyReady && stall)) 

    // write r4 -> clk_counter
    slave.wready(4) match {
      case bool: Bool => bool.updates += ((Bool(true), stall)) }
    clkCounter.comp.updates           += ((Bool(true), clkCounter))
    clkCounter.comp.updates           += ((notStall, clkCounter - UInt(1)))
    clkCounter.comp.updates           += ((clkReady && stall, slaveInBits)) 

    stop.comp.updates                 += ((Bool(true), stop))
    stop.comp.updates                 += ((clkCounter === UInt(1), Bool(true)))
    stop.comp.updates                 += ((clkReady, Bool(false)))

    // val init = Reg(init = Bool(true), clock = daisyClock)
    // init.comp.component = slave
    // init.comp setName "init" 
    // init.comp.updates       += ((Bool(true), init))
    // init.comp.updates       += ((inValid(4), Bool(false))) // Is it right?

    // Set emulation clock
    val emulClock = new Clock
    val enable = (notStall && !stop || slave.reset) 
    enable.getNode.component = slave
    emulClock setName "emul_clk"
    emulClock enabledBy (daisyClock, enable)
    slave.top.clock = emulClock
  }

  def connectDaisyPins (top: Module) {
    // daisy pins of 'top' component
    val daisyOut = top.io("daisy_out") match {
        case dio: DecoupledIO[_] => dio }
    val daisyOutBits = daisyOut.bits match {
      case bits: Bits => bits }
    val daisyIn = top.io("daisy_in") match {
      case vio: ValidIO[_] => vio }
    val daisyInBits = daisyIn.bits match {
      case bits: Bits => bits }
    val daisyControl = top.io("daisy_control") match {
      case addr: Bits => addr }
    val stall = top.io("stall") match {
      case bool: Bool => bool }

    // 'top' has children components
    // ==> connect 'top' and the first and last child
    if (!(top.children.isEmpty)) {
      val notStop = top.io("not_stop") match {
        case bool: Bool => bool }
      // val init = top.io("init") match {
      //   case bool: Bool => bool }
      val headDaisyIn = top.children.head.io("daisy_in") match {
        case vio: ValidIO[_] => vio }
      val headDaisyInBits = headDaisyIn.bits match {
        case bits: Bits => bits }
      val lastDaisyOut = top.children.last.io("daisy_out") match {
        case dio: DecoupledIO[_] => dio }
      val lastDaisyOutBits = lastDaisyOut.bits match {
        case bits: Bits => bits }
      val headStall = top.children.head.io("stall") match {
        case bool: Bool => bool }
      val headDaisyControl = top.children.head.io("daisy_control") match {
        case addr: Bits => addr }

      // top's daisy input pins  =>  the first child's daisy input pins
      // top's daisy output pins <=  the last child's daisy output pins
      headDaisyIn.valid.updates  += ((Bool(true), daisyIn.valid))
      headDaisyInBits.updates    += ((Bool(true), daisyInBits))
      lastDaisyOut.ready.updates += ((Bool(true), daisyOut.ready))
      daisyOut.valid.updates     += ((Bool(true), lastDaisyOut.valid))
      daisyOutBits.updates       += ((Bool(true), lastDaisyOutBits))
      headDaisyControl.updates   += ((Bool(true), daisyControl))
      headStall.updates          += ((Bool(true), stall))

      // connect 'not_stop' & 'init' pins 
      if (!top.children.last.children.isEmpty) {
        val lastNotStop = top.children.last.io("not_stop") match {
          case bool: Bool => bool.updates += ((Bool(true), notStop)) }
        /*
        val lastInit = m.children.last.io("init") match {
          case bool: Bool => bool }
        lastInit.updates         += ((Bool(true), init))
        */
      }
    }

    // Chain children along daisy pins
    for (i <- 0 until top.children.size - 1) {
      val notStop = top.io("not_stop") match {
        case bool: Bool => bool }
      // val init = top.io("init") match {
      //   case bool: Bool => bool }

      val child = top.children(i)
      val nextChild = top.children(i+1)
      
      val cDaisyOut = child.io("daisy_out") match {
        case dio: DecoupledIO[_] => dio }
      val cDaisyOutBits = cDaisyOut.bits match {
        case bits: Bits => bits }
      val ncDaisyIn = nextChild.io("daisy_in") match {
        case vio: ValidIO[_] => vio }
      val ncDaisyInBits = ncDaisyIn.bits match {
        case bits: Bits => bits }
      val ncDaisyControl = nextChild.io("daisy_control") match {
        case addr: Bits => addr }

      // child's daisy input pin     =>  next child's input pin
      cDaisyOut.ready.updates += ((Bool(true), daisyOut.ready))
      ncDaisyIn.valid.updates += ((Bool(true), cDaisyOut.valid))
      ncDaisyInBits.updates   += ((Bool(true), cDaisyOutBits))
      // child's stall pin           <=  top's stall pin 
      child.io("stall") match {
        case bool: Bool => bool.updates += ((Bool(true), stall)) }
      // child's daisy control pin   <=  top's daisy control pin
      child.io("daisy_control") match {
        case bits: Bits => bits.updates += ((Bool(true), daisyControl)) }

      // connect 'not_stop' & 'init' pins 
      if (!child.children.isEmpty) {
        child.io("not_stop") match {
          case bool: Bool => bool.updates += ((Bool(true), notStop)) }
        /*
        val cInit = child.io("init") match {
          case bool: Bool => bool }
        cNotStop.updates      += ((Bool(true), notStop))
        cInit.updates         += ((Bool(true), init))
        */
      }
    }
  }

  def generateCounters {
    ChiselError.info("Counter Backend: generate counters")

    val stack = new Stack[Module]
    val walked = new HashSet[Module]

    for (m <- Module.components ; signal <- m.signals ; if !m.isInstanceOf[AXISlave]) {
      val stall = m.io("stall") match {
        case bool: Bool => bool }
      val signalValue = getTypeNode(signal)
      val signalWidth = signal.getWidth
      val copy = dreadyPairs(signal.component)._1
      val counter = Reg(init = Bits(0, 32), clock = daisyClock)
      val shadow  = Reg(init = Bits(0, 32), clock = daisyClock)
      counter.comp.component = m
      counter.comp setName "counter_%d".format(emitCounterIdx)
      signal.counter = counter
      shadow.comp.component = m
      shadow.comp setName "shadow_%d".format(counterIdx)
      signal.shadow = shadow

      // Signal
      if (signalWidth == 1) {
        val counterValue = counter + signalValue
        counterValue.getNode.component = m
        counterValue.getNode setName "c_value_%d".format(counterIdx)
        if ( !(m.debugs contains counterValue.getNode))
          m.debugs += counterValue.getNode
        counter.comp.updates += ((Bool(true), counter))
        counter.comp.updates += ((!stall, counterValue))
        counter.comp.updates += ((copy, Bits(0, 32)))
      // Bus
      } else {
        val buffer = Reg(init = Bits(0, signalWidth), clock = daisyClock)
        val xor = signalValue ^ buffer
        val hd = PopCount(xor)
        val counterValue = counter + hd
        counterValue.getNode.component = m
        counterValue.getNode setName "c_value_%d".format(counterIdx)
        if ( !(m.debugs contains counterValue.getNode))
          m.debugs += counterValue.getNode
        buffer.comp setName "buffer_%d".format(counterIdx)
        buffer.comp.component = m
        buffer.comp.updates += ((Bool(true), signalValue))
        counter.comp.updates += ((Bool(true), counter))
        counter.comp.updates += ((!stall, counterValue))
        counter.comp.updates += ((copy, Bits(0)))
      }
    }
  }  
  
  def generateDaisyChains {
    ChiselError.info("Counter Backend: generate daisy chains")
 
    // Daisy chaining
    for (m <- Module.components ; if !m.isInstanceOf[AXISlave]) {
      val copy = dreadyPairs(m)._1
      val read = dreadyPairs(m)._2
      val daisyOut = m.io("daisy_out") match {
        case dio: DecoupledIO[_] => dio
      }
      val daisyOutBits = daisyOut.bits match {
        case bits: Bits => bits
      }
      val daisyIn = m.io("daisy_in") match {
        case vio: ValidIO[_] => vio
      }
      val daisyInBits = daisyIn.bits match {
        case bits: Bits => bits
      }

      // Copy logic
      if (!m.signals.isEmpty) {
        daisyOutBits.updates += ((Bool(true), m.signals.head.shadow))
        m.signals.last.shadow.comp.updates += ((read, daisyInBits))
      } else if (m.children.isEmpty){
        daisyOutBits.updates += ((Bool(true), daisyInBits))
      }

      for (i <- 0 until m.signals.size - 1) {
        val counter = m.signals(i).counter
        val shadow = m.signals(i).shadow
        val nextShadow = m.signals(i+1).shadow
      
        shadow.comp.updates += ((Bool(true), shadow))
        shadow.comp.updates += ((copy, counter))
        shadow.comp.updates += ((read, nextShadow))
      }
    }
  }

  def findClockCrossingPoints(m: Module) {
    m bfs { node =>
      for (input <- node.inputs) {
        if (node.isReg && node.clock.edge == NegEdge) {
          if (!(node.component.debugs contains input))
            node.component.debugs += input
        } else  if (node.clock != input.clock) {
          input match {
            case _: Reg =>
            case _ => {
              if (!(node.component.debugs contains input)) {
                node.component.debugs += input
              }
            }
          }
        }
      }
    }
  }

  /*
  def initTester(m: Module) {
    m match {
      case s: AXISlave if Module.isTesting => Module.tester = new AXISlaveTester(s)
      case _ =>
    }
  }
  */

  def reportCounters (m: Module) {
    val rptdir  = ensureDir(targetdir)
    val rptfile = new java.io.FileWriter(rptdir+"%s_signal.rpt".format(m.name))
    val report = new StringBuilder();

    ChiselError.info("Counter Backend: report annotated signals")
    
    report append "\t\t+-------------------------------------+\n"
    report append "\t\t|          Counter Report             |\n"
    report append "\t\t|                     by Donggyu Kim  |\n"
    report append "\t\t+-------------------------------------+\n\n"

    report append "CountNo\t"
    // report append signals.size.toString + "\n" 
    report append "CrossNo\t"
    report append crosses.size.toString + "\n"
    report append "Weigts\t\t\tCross\t\t\tSingals\n"
    for ((coeff, cross) <- crosses) {
      report append "%.5e".format(coeff)
      report append "\t\t"
      if (!cross.isEmpty) {
        // report append emitRef(signalCounterMap(cross.head).comp)
        for (term <- cross.tail) {
          report append "*"
          // report append emitRef(signalCounterMap(term).comp)
        }
      }
      report append "\t\t"
      if (!cross.isEmpty) {
        report append getSignalPathName(cross.head) 
        for (term <- cross.tail) {
          report append ", "
          report append getSignalPathName(term) 
        }
      }
      report append "\n"
    }

    try {
      rptfile.write(report.result)
    } finally {
      rptfile.close()
    }
  }
}
