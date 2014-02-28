package Chisel

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.math.pow
import scala.io.Source


class Slave extends Module {
  val addr_width = 5
  val data_width = 32
  val io = new Bundle {
    val in = Decoupled(Bits(width = data_width)).flip
    val out = Decoupled(Bits(width = data_width))
    val addr = Bits(INPUT, addr_width)
  }
  val signals = new ArrayBuffer[Node]
  val signalCounterMap = new HashMap[Node, Node]
}

class SlaveTester(s: Slave) extends Tester(s, Array(s.io) ++ s.signals /* ++ (Module.clocks filter (_.isEnabled))*/ ) {
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

trait CounterBackend extends Backannotation {
  val addr_width = 5
  val data_width = 32
  val daisy_ctrl_width = 2
  val data_addr_width = addr_width - daisy_ctrl_width

  val signals = new HashSet[Node]
  val shadows = new HashMap[Module, ArrayBuffer[Bits]]
  val crosses = new ArrayBuffer[(Double, Array[Node])]
  val signalCounterMap = new HashMap[Node, Bits]
  val shadowCounterMap = new HashMap[Bits, Bits]
  val ops = new HashMap[Module, (Bool, Bool)]

  var counterIdx = -1
  var shadowIdx = -1

  // Define two different clock domains for the daisy chain
  val daisyClock = Module.implicitClock
  val stopClock = -daisyClock
  val emulClock = new Clock

  preElaborateTransforms += ((c: Module) => annotateSignals(c))
  preElaborateTransforms += ((c: Module) => generateCtrls(c))
  preElaborateTransforms += ((c: Module) => generateCounters(c))
  preElaborateTransforms += ((c: Module) => generateDaisyChain(c))
  preElaborateTransforms += ((c: Module) => generateSlave(c))
  analyses += ((c: Module) => findClockCrossingPoints(c))
  analyses += ((c: Module) => initTester(c))
  analyses += ((c: Module) => reportCounters(c))

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

    if (node.consumers.size == 1) {
      node.consumers.head match {
        case bits: Bits => bits
        case _ => genTypeNode(node)
      }
    } else {
      genTypeNode(node)
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

  def addCounter(signal: Node, counter: Bits) = {
    val reg = counter.comp

    counterIdx += 1
    reg.component = signal.component
    reg setName "counter%d".format(counterIdx)
    signalCounterMap(signal) = counter
    counter
  }

  def addShadow(counter: Bits, shadow: Bits) = {
    val reg = shadow.comp
    val m = counter.comp.component
    
    shadowIdx += 1
    reg setName "shadow%d".format(shadowIdx)
    reg.component = m

    // shadows += shadow
    if (!(shadows contains m)) {
      shadows(m) = new ArrayBuffer[Bits]
    }
    shadows(m) += shadow
    shadow
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

    collectNodesIntoComp(initializeDFS)

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
    for (m <- Module.sortedComps) {
      val reset = m.reset
      val resetName = getSignalPathName(reset, ".")
      if (signalNames contains resetName) {
        signals += reset
        if (!(m.debugs contains reset))
          m.debugs += reset
      }
      for (node <- m.nodes ; if !node.isTypeNode) {
        val signalName = getSignalPathName(node, ".")
        if (signalNames contains signalName) {
          signals += node
          signalNameMap(signalName) = node
          if (!(node.component.debugs contains node))
            node.component.debugs += node
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

  def generateCtrls(m: Module) {
    ChiselError.info("Counter Backend: generate daisy controls")

    // Create pins
    for (m <- Module.sortedComps) {
      val stall = Bool(INPUT)
      val daisy_control = Bits(INPUT, 3)
      val daisy_out = Decoupled(Bits(width = 32))
      val daisy_in = Valid(Bits(width = 32)).flip
      val in_valid = m.io("in") match { 
        case in: DecoupledIO[_] => in.ready match {
          case bool: Bool => bool
        }
      }
      
      def ready(i: Int) = {
        val readyVal = stall && daisy_out.ready &&
          (daisy_control === Bits(i, daisy_control.getWidth))
        readyVal.getNode.component = m
        if (i == 1) {
          readyVal.getNode setName "copy"
        } else if (i == 2) {
          readyVal.getNode setName "read"
        }
        if (!(m.debugs contains readyVal.getNode))
          m.debugs += readyVal.getNode

        readyVal
      }

      m.clocks.clear
      m.clocks += daisyClock

      ops(m) = (ready(1), ready(2))

      addPin(m, stall, "stall")
      addPin(m, daisy_control, "daisy_control")
      addPin(m, daisy_out, "daisy_out")
      addPin(m, daisy_in, "daisy_in")

      if (!m.children.isEmpty) {
        val not_stop = Bool(INPUT)
        val init = Bool(INPUT)
        addPin(m, not_stop, "not_stop")
        addPin(m, init, "init")
        val childClock = new Clock
        val enable = (!stall && not_stop || init) 
        enable.getNode.component = m
        childClock setName ("emul_clk_" + m.level)
        childClock enabledBy (daisyClock, enable)
        for (child <- m.children) {
          child.clock = childClock
        }
      }

      /* daisy_out.valid := ready(2) */
      daisy_out.valid.updates += ((Bool(true), stall))
    }

    // Connect components
    val daisyInBits = m.io("daisy_in") match {
      case vio: ValidIO[_] => vio.bits match {
        case bits: Bits => bits
      }
    }
    daisyInBits.updates += ((Bool(true), Bits(0)))

    val stack = new Stack[Module]
    val walked = new HashSet[Module]
    stack push m
    while (!stack.isEmpty) {
      val top = stack.pop
      val daisyOut = top.io("daisy_out") match {
        case dio: DecoupledIO[_] => dio
      }
      val daisyOutBits = daisyOut.bits match {
        case bits: Bits => bits
      }
      val daisyIn = top.io("daisy_in") match {
        case vio: ValidIO[_] => vio
      }
      val daisyInBits = daisyIn.bits match {
        case bits: Bits => bits
      }
      val daisyControl = m.io("daisy_control") match {
        case addr: Bits => addr
      }
      val stall = m.io("stall") match {
        case bool: Bool => bool
      }
      val notStop = m.io("not_stop") match {
        case bool: Bool => bool
      }
      val init = m.io("init") match {
        case bool: Bool => bool
      }

      if (!(top.children.isEmpty)) {
        val headDaisyIn = top.children.head.io("daisy_in") match {
          case vio: ValidIO[_] => vio
        }
        val headDaisyInBits = headDaisyIn.bits match {
          case bits: Bits => bits
        }
        val lastDaisyOut = top.children.last.io("daisy_out") match {
          case dio: DecoupledIO[_] => dio
        }
        val lastDaisyOutBits = lastDaisyOut.bits match {
          case bits: Bits => bits
        }
        val headStall = top.children.head.io("stall") match {
          case bool: Bool => bool
        }
        val headDaisyControl = top.children.head.io("daisy_control") match {
          case addr: Bits => addr
        }

        headDaisyIn.valid.updates  += ((Bool(true), daisyIn.valid))
        headDaisyInBits.updates    += ((Bool(true), daisyInBits))
        lastDaisyOut.ready.updates += ((Bool(true), daisyOut.ready))
        daisyOut.valid.updates     += ((Bool(true), lastDaisyOut.valid))
        daisyOutBits.updates       += ((Bool(true), lastDaisyOutBits))
        headDaisyControl.updates   += ((Bool(true), daisyControl))
        headStall.updates          += ((Bool(true), stall))

        if (!top.children.head.children.isEmpty) {
          val headNotStop = top.children.head.io("not_stop") match {
            case bool: Bool => bool
          }
          val headInit = top.children.head.io("init") match {
            case bool: Bool => bool
          }
          headNotStop.updates      += ((Bool(true), notStop))
          headInit.updates         += ((Bool(true), init))
        }
 
        stack push top.children.head
      }

      for (i <- 1 until top.children.size) {
        val child = top.children(i-1)
        val nextChild = top.children(i)
      
        val cDaisyOut = child.io("daisy_out") match {
          case dio: DecoupledIO[_] => dio
        }
        val cDaisyOutBits = cDaisyOut.bits match {
          case bits: Bits => bits
        }
        val ncDaisyIn = nextChild.io("daisy_in") match {
          case vio: ValidIO[_] => vio
        }
        val ncDaisyInBits = ncDaisyIn.bits match {
          case bits: Bits => bits
        }
        val ncStall = nextChild.io("stall") match {
          case bool: Bool => bool
        }
        val ncDaisyControl = nextChild.io("daisy_control") match {
          case addr: Bits => addr
        }

        cDaisyOut.ready.updates  += ((Bool(true), daisyOut.ready))
        ncDaisyIn.valid.updates  += ((Bool(true), cDaisyOut.valid))
        ncDaisyInBits.updates    += ((Bool(true), cDaisyOutBits))
        ncStall.updates          += ((Bool(true), stall))
        ncDaisyControl.updates   += ((Bool(true), daisyControl))

        if (!nextChild.children.isEmpty) {
          val ncNotStop = nextChild.io("not_stop") match {
            case bool: Bool => bool
          }
          val ncInit = nextChild.io("init") match {
            case bool: Bool => bool
          }
          ncNotStop.updates      += ((Bool(true), notStop))
          ncInit.updates         += ((Bool(true), init))
        }

        stack push nextChild
      }
    }
  }

  def generateCounters (m: Module) {
    ChiselError.info("Counter Backend: generate counters")

    m bfs (_.addConsumers)

    val stack = new Stack[Module]
    val walked = new HashSet[Module]

    for (m <- Module.sortedComps ; signal <- m.nodes) {
      if (signals contains signal) {
        // ChiselError.info(emitRef(signal) + ": " + nodeToString(signal))
        val stall = signal.component.io("stall")
        val daisyControl = signal.component.io("daisy_control")
        val signalValue = getTypeNode(signal)
        val signalWidth = signal.getWidth
        val counter = addCounter(signal, Reg(init = Bits(0, 32), clock = daisyClock))
        val copy = ops(signal.component)._1

        stall match {
          case isStall: Bool => {
            if (signalWidth == 1) {
              /*
              when(!isStall) {
                counter := counter + signalValue
              }.elsewhen(copy) {
                counter := Bits(0, 32)
              }
              */
              val counterValue = counter + signalValue
              counterValue.getNode.component = signal.component
              counterValue.getNode setName "c_value%d".format(counterIdx)
              if ( !(signal.component.debugs contains counterValue.getNode))
                signal.component.debugs += counterValue.getNode
              counter.comp.updates += ((Bool(true), counter))
              counter.comp.updates += ((!isStall, counterValue))
              counter.comp.updates += ((copy, Bits(0, 32)))
            }
            // This is a bus
            else {
              val buffer = Reg(init = Bits(0, signalWidth), clock = daisyClock)
              val xor = signalValue ^ buffer
              val hd = PopCount(xor)
              val counterValue = counter + hd
              counterValue.getNode.component = signal.component
              counterValue.getNode setName "c_value%d".format(counterIdx)
              if ( !(signal.component.debugs contains counterValue.getNode))
                signal.component.debugs += counterValue.getNode
              /*
              buffer := signalValue
              when(!isStall){
                counter := counter + hd
              }.elsewhen(copy) {
                counter := Bits(0, 32)
              }
              */
              buffer.comp setName "buffer%d".format(counterIdx)
              buffer.comp.component = m
              buffer.comp.updates += ((Bool(true), signalValue))
              counter.comp.updates += ((Bool(true), counter))
              counter.comp.updates += ((!isStall, counterValue))
              counter.comp.updates += ((copy, Bits(0)))
            }
          }
          case _ =>
        }
      }
    }
  }  
  
  def generateDaisyChain(m: Module) {
    ChiselError.info("Counter Backend: generate daisy chains")
 
    // Couple counters with shadows
    for ((signal, counter) <- signalCounterMap) {
      val shadow = addShadow(counter, Reg(init = Bits(0, 32), clock = daisyClock))
      shadowCounterMap(shadow) = counter
    }

    // Daisy chaining
    for (m <- Module.sortedComps) {
      val shadowlist = shadows getOrElse (m, new ArrayBuffer[Bits])
      val copy = ops(m)._1
      val read = ops(m)._2
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
      if (!shadowlist.isEmpty) {
        daisyOutBits.updates += ((Bool(true), shadowlist.head))
        shadowlist.last.comp.updates += ((read, daisyInBits))
      } else if (m.children.isEmpty){
        daisyOutBits.updates += ((Bool(true), daisyInBits))
      }

      for (i <- 1 until shadowlist.size) {
        val shadow = shadowlist(i-1)
        val nextShadow = shadowlist(i)
        val counter = shadowCounterMap(shadow)
      
        shadow.comp.updates += ((Bool(true), shadow))
        shadow.comp.updates += ((copy, counter))
        shadow.comp.updates += ((read, nextShadow))
      }
    }
  }

  def generateSlave(top: Module) {
    ChiselError.info("Counter Backend: generate the slave")

    // initialize Slave module
    val slave = Module(new Slave)
    slave.name = "Slave"
    slave.children += top
    top.parent = slave
    Module.setAsTopComponent(slave)
    slave markComponent nameSpace
    Module.sortedComps prepend slave
    slave.signals ++= signals
    slave.signalCounterMap ++= ( for ((x, y) <- signalCounterMap) yield ((x, y.getNode)) )

    // Registers to control the daisy chain
    val topInReg = Reg(init = Bits(0), clock = daisyClock)
    val clkCounter = Reg(init = Bits(0), clock = daisyClock)
    val inputRdy = Reg(init = Bool(true), clock = daisyClock)
    val init = Reg(init = Bool(true), clock = daisyClock)
    val stop = Reg(init = Bool(false), clock = stopClock) // works at negative edges
    val notStop = !stop
    val notStall = clkCounter.orR
    val stall = !notStall
    val clkCounterIsOne = clkCounter === Bits(1)
    val clkCounterDecr  = clkCounter - Bits(1, 32)

    topInReg.comp.component = slave
    topInReg.comp setName "top_in_bits"
    clkCounter.comp.component = slave
    clkCounter.comp setName "clk_counter" 
    inputRdy.comp.component = slave
    inputRdy.comp setName "input_rdy"
    stop.comp.component = slave
    stop.comp setName "stop" 
    init.comp.component = slave
    init.comp setName "init" 
    notStall.getNode setName "not_stall"
    notStall.getNode.component = slave
    stall.getNode setName "stall"
    stall.getNode.component = slave
    clkCounterIsOne.getNode setName "clk_cnt_is_one"
    clkCounterIsOne.getNode.component = slave
    clkCounterDecr.getNode setName "clk_cnt_decr"
    clkCounterDecr.getNode.component = slave

    // Pin connection
    val addr = slave.io("addr") match {
      case addr: Bits => addr
    }
    val slaveIn = slave.io("in") match {
      case dio: DecoupledIO[_] => dio
    }
    val slaveInBits = slaveIn.bits match {
      case bits: Bits => bits
    }
    val slaveOut = slave.io("out") match {
      case dio: DecoupledIO[_] => dio
    }
    val slaveOutBits = slaveOut.bits match {
      case bits: Bits => bits
    }
    val topIn = top.io("in") match {
      case dio: DecoupledIO[_] => dio
    }
    /*
    val topInBits = topIn.bits match {
      case bits: Bits => bits
    }
    */
    val topOut = top.io("out") match {
      case dio: ValidIO[_] => dio
    }
    val topOutBits = topOut.bits match {
      case bits: Bits => bits
    }
    val topStall = top.io("stall") match {
      case bool: Bool => bool
    }
    val topNotStop = top.io("not_stop") match {
      case bool: Bool => bool
    }
    val topInit = top.io("init") match {
      case bool: Bool => bool
    }
    val daisyControl = top.io("daisy_control") match {
      case addr: Bits => addr
    }
    val daisyIn = top.io("daisy_in") match {
      case vio: ValidIO[_] => vio
    }
    val daisyInBits = daisyIn.bits match {
      case bits: Bits => bits
    }
    val daisyOut = top.io("daisy_out") match {
      case dio: DecoupledIO[_] => dio
    }
    val daisyOutBits = daisyOut.bits match {
      case bits: Bits => bits
    }
    
    val outBits = Vec(Range(0, pow(2, data_addr_width).toInt) map { 
      case 2 => daisyOutBits
      case 3 => topOut.valid
      case 4 => topOutBits
      case _ => Bits(0)
    })

    val dataAddr  = addr(addr_width-1, daisy_ctrl_width) 
    val daisyAddr = addr(daisy_ctrl_width-1, 0)
    val outValid = Vec(Range(0, pow(2, data_addr_width).toInt) map { 
      case 2 => daisyOut.valid
      case 3 => Bool(true)
      case 4 => topOut.valid
      case _ => Bool(false)
    })
    val daisyReady = dataAddr === UInt(2) && slaveOut.ready
    val inputReady = Vec(Range(0, pow(2, data_addr_width).toInt) map { 
      case 0 => topIn.ready
      case 1 => Bool(true)
      case _ => Bool(false)
    })
    def inValid(i: Int) = slaveIn.valid && stall && dataAddr === UInt(i)

    val (a, b) = topIn.bits match {
      case bundle: Bundle => (bundle.elements(0)._2, bundle.elements(1)._2) match {
        case (bitsA: Bits, bitsB: Bits) => (bitsA, bitsB)
      }
    }

    topStall.updates       += ((Bool(true), stall))
    topNotStop.updates     += ((Bool(true), notStop))
    topInit.updates        += ((Bool(true), init))
    daisyControl.updates   += ((Bool(true), daisyAddr))
    a.updates              += ((Bool(true), topInReg(31, 16)))
    b.updates              += ((Bool(true), topInReg(15, 0)))
    daisyOut.ready.updates += ((Bool(true), daisyReady))
    daisyIn.valid.updates  += ((Bool(true), daisyReady))
    daisyInBits.updates    += ((Bool(true), Bits(0)))
    slaveOut.valid.updates += ((Bool(true), outValid(dataAddr)))
    slaveOutBits.updates   += ((Bool(true), outBits(dataAddr)))
    slaveIn.ready.updates  += ((Bool(true), inputRdy || inputReady(dataAddr)))
    topIn.valid.updates    += ((Bool(true), inputRdy || inValid(0)))

    topInReg.comp.updates   += ((Bool(true), topInReg))
    topInReg.comp.updates   += ((inValid(0), slaveInBits))
    init.comp.updates       += ((Bool(true), init))
    init.comp.updates       += ((inValid(0), Bool(false)))
    clkCounter.comp.updates += ((Bool(true), clkCounter))
    clkCounter.comp.updates += ((notStall, clkCounterDecr))
    clkCounter.comp.updates += ((inValid(1), slaveInBits))
    stop.comp.updates       += ((Bool(true), stop))
    stop.comp.updates       += ((clkCounterIsOne, Bool(true)))
    stop.comp.updates       += ((inValid(1), Bool(false)))
    inputRdy.comp.updates   += ((Bool(true), inputRdy))
    inputRdy.comp.updates   += ((topOut.valid, Bool(true)))
    inputRdy.comp.updates   += ((inValid(1), Bool(false)))

    // emulation clock
    val enable = (notStall && notStop || init) 
    enable.getNode.component = slave
    emulClock setName "emul_clk"
    emulClock enabledBy (daisyClock, enable)
    top.clock = emulClock

    slave.genAllMuxes
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

  def initTester(m: Module) {
    m match {
      case s: Slave if Module.isTesting => Module.tester = new SlaveTester(s)
      case _ =>
    }
  }

  def reportCounters (m: Module) {
    val rptdir  = ensureDir(targetdir)
    val rptfile = new java.io.FileWriter(rptdir+"%s_signal.rpt".format(m.name))
    val report = new StringBuilder();

    ChiselError.info("Counter Backend: report annotated signals")
    
    report append "\t\t+-------------------------------------+\n"
    report append "\t\t|          Counter Report             |\n"
    report append "\t\t|                     by Donggyu Kim  |\n"
    report append "\t\t+-------------------------------------+\n\n"

    report append "Weigts\t\t\tCross\t\t\tSingals\n"
    for ((coeff, cross) <- crosses) {
      report append "%.5e".format(coeff)
      report append "\t\t"
      if (!cross.isEmpty) {
        report append emitRef(signalCounterMap(cross.head).comp)
        for (term <- cross.tail) {
          report append "*"
          report append emitRef(signalCounterMap(term).comp)
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
