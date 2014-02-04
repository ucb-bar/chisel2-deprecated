package Chisel

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack

class AXISlave extends Module {
  val addr_width = 5
  val data_width = 32
  val io = new Bundle {
    val in = Decoupled(Bits(width = data_width)).flip
    val out = Decoupled(Bits(width = data_width))
    val addr = Bits(INPUT, addr_width)
  }
}

class CounterBackend extends FPGABackend with SignalBackannotation {
  val shadows = new ArrayBuffer[Bits]
  val signalCounterMap = new HashMap[Node, Bits]
  val ops = new HashMap[Module, (Bool, Bool)]

  var counterIdx = -1
  var shadowIdx = -1

  // Define two different clock domains for the daisy chain
  val daisyClock = Module.implicitClock
  val emulClock = new Clock
  emulClock setName "emul_clk"

  preElaborateTransforms += ((c: Module) => generateCounters)
  preElaborateTransforms += ((c: Module) => generateDaisyChain)
  preElaborateTransforms += ((c: Module) => generateSlave(c))

  preElaborateTransforms += ((c: Module) => Module.topComponent.genAllMuxes)
  /*
  transforms += ((c: Module) => Module.topComponent.inferAll)
  transforms += ((c: Module) => Module.topComponent.forceMatchingWidths)
  transforms += ((c: Module) => Module.topComponent.removeTypeNodes)
  transforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  */
  
  analyses += ((c: Module) => copyResource("user_logic.v", targetdir))
  analyses += ((c: Module) => copyResource("wrapper.v", targetdir))

  def getTypeNode(node: Node) = {
    val typeNode = new UInt
    typeNode.isTypeNode = true
    typeNode.inputs += node
    for (consumer <- node.consumers) {
      consumer.inputs -= node
      consumer.inputs += typeNode
    }
    typeNode
  }

  def addPin(m: Module, pin: Data, name: String = "") {
    pin.component = m
    pin.isIo = true
    pin setName (name)

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

  def addShadow(m: Module, shadow: Bits) = {
    val reg = shadow.comp
    shadowIdx += 1
    reg setName "shadow%d".format(shadowIdx)
    reg.component = m
    shadow.component = m
    shadows += shadow
    shadow
  }

  def generateCounters {
    ChiselError.info("Counter Backend: generate counters")

    Module.sortedComps map (_.nodes map (_.addConsumers))

    for (m <- Module.sortedComps ; signal <- m.nodes ; if signals contains signal) {
      ChiselError.info(signal.name + " ==> " + nodeToString(signal))
      val signalType = getTypeNode(signal)
      val signalWidth = signal.getWidth
      val counter = addCounter(signal, Reg(outType = Bits(width =  32), clock = daisyClock))
      if (signalWidth == 1) {
        counter.comp match {
          case r: Reg => r.updates += ((signalType === Bool(true), counter + Bits(1, 32)))
          case _ =>
        }
      }
      // This is a bus
      else {
        val buffer = Reg(outType = Bits(width = signalWidth), clock = daisyClock)
        val xor = signalType ^ buffer
        val hd = PopCount(xor)
        buffer.comp setName "buffer%d".format(counterIdx)
        buffer.comp.component = m
        buffer.comp match {
          case r: Reg => r.updates += ((Bool(true), signalType))
          case _ =>
        }
        counter.comp match {
          case r: Reg => r.updates += ((Bool(true), counter + hd))
        }
      }
    }
  }  
  

  def generateDaisyChain {
    ChiselError.info("Counter Backend: generate daisy chains")
 
    // Create pins
    for (m <- Module.sortedComps) {
      val stall = Bool(INPUT)
      val daisy_control = Bits(INPUT, 3)
      val daisy_out = Decoupled(Bits(width = 32))
      val copy = stall && daisy_control === Bits(1, daisy_control.getWidth)
      val read = daisy_out.ready && stall && (daisy_control === Bits(2, daisy_control.getWidth))

      m.clock = emulClock
      m.clocks.clear
      m.clocks += daisyClock

      copy.getNode setName "copy"
      read.getNode setName "read"

      ops(m) = (copy, read)

      addPin(m, stall, "stall")
      addPin(m, daisy_control, "daisy_control")
      addPin(m, daisy_out, "daisy_out")

      daisy_out.valid := read
    }

    // Couple counters with shadows
    for (m <- Module.sortedComps) {
      for (signal <- m.nodes ; if (signals contains signal) && (signalCounterMap contains signal)) {
        val shadow = addShadow(m, Reg(outType = Bits(width = 32), clock = daisyClock))
        val copy = ops(m)._1
        val counter = signalCounterMap(signal)

        shadow.comp match {
          case r: Reg => r.updates += ((copy, counter))
          case _ =>
        }
      }
    }  
 
    // Daisy chain
    // Connect shodows
    if (!shadows.isEmpty) {
      val shadow = shadows.head
      val daisy_out = shadow.component.io("daisy_out")
      daisy_out match {
        case dio: DecoupledIO[_] => dio.bits := shadow
        case _ =>
      }
    }
    for (i <- 1 until shadows.size) {
      val shadow = shadows(i-1)
      val nextShadow = shadows(i)
      val read = ops(shadow.component)._2

      shadow.comp match {
        case r: Reg => r.updates += ((read, nextShadow))
        case _ =>
      }
    }
    if (!shadows.isEmpty) {
      val shadow = shadows.last
      val read = ops(shadow.component)._2

      shadow.comp match {
        case r: Reg => r.updates += ((read, Bits(0, 32)))
        case _ =>
      }
    }
  }

  def generateSlave(top: Module) {
    ChiselError.info("Counter Backend: generate the slave")

    // initialize Slave module
    val slave = Module(new AXISlave)
    slave.markComponent
    top.parent = slave
    slave.children += top
    Module.setAsTopComponent(slave)
    slave.level = 0
    Module.sortedComps map { _.level += 1}
    Module.sortedComps prepend slave

    // connect pins
    val slaveAddr = slave.io("addr")
    val slaveIn = slave.io("in")
    val slaveOut = slave.io("out")
    val clkCounter = Reg(outType = Bits(width = 32), clock = daisyClock)
    val clkReg = clkCounter.comp
    val stall = !clkCounter.andR
    clkReg.component = slave
    clkReg setName "clk_counter"

    top.io("stall") := stall
    (slaveAddr, slaveIn) match {
      case (bits: Bits, dio: DecoupledIO[_]) => {
        clkReg.updates += ((!stall, clkCounter - Bits(1, 32)))
        clkReg.updates += ((bits(4,3) === UInt(2, slaveAddr.getWidth), dio.bits))
      }
      case _ =>
    }

    (slaveAddr, slaveOut, top.io("daisy_out"), top.io("out")) match {
      case (bits: Bits, 
            dio1: DecoupledIO[_], 
            dio2: DecoupledIO[_], 
            vio: ValidIO[_]) => {
        top.io("daisy_control") := bits(2,0)
        dio1.ready <> dio2.ready
        dio1.bits := Mux(bits(4,3) === UInt(0), dio2.bits, vio.bits)
        dio1.valid := Mux(bits(4,3) === UInt(0), dio2.valid, vio.valid)

        /*
        dio1.valid match {
          case b: Bits => {
            b.updates += ((bits(4,3) === UInt(0), dio2.valid))
            b.updates += ((bits(4,3) === UInt(1), vio.valid))
          }
          case _ =>
        }
        */
      }
      case _ =>
    }

    (slaveIn, top.io("in")) match {
      case (dio1: DecoupledIO[_], dio2: DecoupledIO[_]) => {
        dio1.ready <> dio2.ready
        dio1.valid <> dio2.valid
        (dio1.bits, dio2.bits)  match {
          case (bits: Bits, bundle: Bundle) => {
            val (name0, a) = bundle.elements(0)
            val (name1, b) = bundle.elements(1)
            a := bits(31, 16)
            b := bits(15, 0)
          }
          case _ =>
        }
      }
      case _ =>
    }

    val clockType = getTypeNode(daisyClock)
    val notStall = clkCounter.andR
    val enabledClk = clockType.toBool && notStall
    enabledClk.getNode setName "emul_clk"
   
    val stack = new Stack[Node]
    val walked = new HashSet[Node]
    stack push enabledClk
    while(!stack.isEmpty) {
      val node = stack.pop
      node.infer
      walked += node
      if (!node.isTypeNode && !node.isReg) {
        slave.mods += node
      } 
      for (i <- 0 until node.inputs.size) {
        if (node.inputs(i) != null) {
          if (node.inputs(i).isTypeNode) {
            node.inputs(i) = node.inputs(i).getNode
          } 
          if (!(walked contains node.inputs(i))) {
            stack push node.inputs(i)
            walked += node.inputs(i)
          }
        }
      }
    }
  }
}
