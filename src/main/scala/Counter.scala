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
}

trait CounterBackend extends Backannotation {
  val addr_width = 5
  val data_width = 32
  val daisy_ctrl_width = 2
  val data_addr_width = addr_width - daisy_ctrl_width

  val signals = new HashSet[Node]
  val shadows = new ArrayBuffer[Bits]
  val signalCounterMap = new HashMap[Node, Bits]
  val shadowCounterMap = new HashMap[Bits, Bits]
  val ops = new HashMap[Module, (Bool, Bool)]

  var counterIdx = -1
  var shadowIdx = -1

  // Define two different clock domains for the daisy chain
  val daisyClock = Module.implicitClock
  val stopClock = -daisyClock
  val emulClock = new Clock
  emulClock setName "emul_clk"

  preElaborateTransforms += ((c: Module) => annotateSignals(c))
  preElaborateTransforms += ((c: Module) => generateCtrls)
  preElaborateTransforms += ((c: Module) => generateCounters(c))
  preElaborateTransforms += ((c: Module) => generateDaisyChain(c))
  preElaborateTransforms += ((c: Module) => generateSlave(c))
  // transforms += ((c: Module) => Module.topComponent.clocks -= emulClock) //Todo: remove it
  analyses += ((c: Module) => findClockCrossingPoints(c))

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

  private def annotateSignals(m: Module) {
    ChiselError.info("Backannotation: annotate signals")

    // Read the signal list file
    // TODO: generalize the signal file format
    val lines = Source.fromFile(Module.signalFilename).getLines
    val TermRegex = """\s*([\w\._\:]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)""".r
    val signalNames = new HashSet[String]

    collectNodesIntoComp(initializeDFS)

    for (line <- lines) {
      line match {
        case TermRegex(exp, coeff, se, tstat, pvalue) => {
          val vars = exp split ":"
          if (tstat != "NaN" && pvalue != "NaN") {
            signalNames ++= vars
          }
        }
        case _ =>
      }
    }

    // Find correspoinding nodes
    m dfs { node =>
      if (!node.isTypeNode) {
        val signalName = getSignalPathName(node, ".")
        if (signalNames contains signalName) {
          signals += node
          if (!(node.component.debugs contains node))
            node.component.debugs += node
        }
      }
    }

    // For resets
    for (m <- Module.components) {
      val reset = m.reset
      val resetName = getSignalPathName(reset, ".")
      if (signalNames contains resetName) {
        signals += reset
        if (!(m.debugs contains reset))
          m.debugs += reset
      }
    }
  }

  def generateCtrls {
    ChiselError.info("Counter Backend: generate daisy controls")

    // Create pins
    for (m <- Module.sortedComps) {
      val stall = Bool(INPUT)
      val daisy_control = Bits(INPUT, 3)
      val daisy_out = Decoupled(Bits(width = 32))
      val in_valid = m.io("in") match { 
        case in: DecoupledIO[_] => in.ready 
        case _ => null 
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

      m.clock = emulClock
      m.clocks.clear
      m.clocks += daisyClock

      ops(m) = (ready(1), ready(2))

      addPin(m, stall, "stall")
      addPin(m, daisy_control, "daisy_control")
      addPin(m, daisy_out, "daisy_out")

      /* daisy_out.valid := ready(2) */
      daisy_out.valid.updates += ((Bool(true), stall))
    }
  }

  def generateCounters (m: Module) {
    ChiselError.info("Counter Backend: generate counters")

    m bfs (_.addConsumers)

    val stack = new Stack[Module]
    val walked = new HashSet[Module]
    
    m dfs { signal =>
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
              if (! (signal.component.debugs contains counterValue.getNode))
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
              if (! (signal.component.debugs contains counterValue.getNode))
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
    m bfs { signal =>
      if (signals contains signal) {
        val shadow = addShadow(signal.component, Reg(init = Bits(0, 32), clock = daisyClock))
        val counter = signalCounterMap(signal)
 
        shadowCounterMap(shadow) = counter
      }
    }  
 
    // Daisy chain
    // Connect shodows
    // todo: hierarchical modules
    if (!shadows.isEmpty) {
      val shadow = shadows.head
      // val read = ops(shadow.component)._1
      val stall = shadow.component.io("stall")
      val daisyOut = shadow.component.io("daisy_out")
      (daisyOut, stall) match {
        case (dio: DecoupledIO[_], isStall: Bool) => {
          // dio.bits := shadow
          dio.bits match {
            case bits: Bits => bits.updates += ((Bool(true), shadow))
            case _ =>
          }
        }
        case _ =>
      }
    }
    for (i <- 1 until shadows.size) {
      val shadow = shadows(i-1)
      val nextShadow = shadows(i)
      val counter = shadowCounterMap(shadow)
      val copy = ops(shadow.component)._1
      val read = ops(shadow.component)._2

      /*
      when (copy) {
        shadow := counter
      }.elsewhen (read) {
        shadow := nextShadow
      }
      */
      shadow.comp.updates += ((Bool(true), shadow))
      shadow.comp.updates += ((copy, counter))
      shadow.comp.updates += ((read, nextShadow))
    }
    if (!shadows.isEmpty) {
      val shadow = shadows.last
      val counter = shadowCounterMap(shadow)
      val copy = ops(shadow.component)._1

      /*
      when (copy) {
        shadow := counter
      }
      */
      shadow.comp.updates += ((Bool(true), shadow))
      shadow.comp.updates += ((copy, counter))
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

    // connect pins
    val slaveAddr = slave.io("addr")
    val slaveIn = slave.io("in")
    val slaveOut = slave.io("out")
    val topInBits = Reg(init = Bits(0, 32), clock = daisyClock)
    val clkCounter = Reg(init = Bits(0, 32), clock = daisyClock)
    val inputRdy = Reg(init = Bool(true), clock = daisyClock)
    val init = Reg(init = Bool(true), clock = daisyClock)
    val stop = Reg(init = Bool(false), clock = stopClock) // works at negative edges
    val notStall = clkCounter.orR
    val stall = !notStall

    topInBits.comp.component = slave
    topInBits.comp setName "top_in_bits"
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
    if (!(slave.debugs contains notStall.getNode))
      slave.debugs contains notStall.getNode
    if (!(slave.debugs contains stall.getNode))
      slave.debugs contains stall.getNode

    // for top's output ports
    (slaveAddr, slaveOut, top.io("daisy_out"), top.io("out")) match {
      case (addr: Bits, 
            slaveIO: DecoupledIO[_], 
            daisyIO: DecoupledIO[_], 
            topIO: ValidIO[_]) => {
        val readAddr = addr(addr_width-1, daisy_ctrl_width)
        val bits = Vec(Range(0, pow(2, data_addr_width).toInt) map { 
          case 2 => daisyIO.bits
          case 3 => topIO.valid
          case 4 => topIO.bits
          case _ => Bits(0)
        })
        val valids = Vec(Range(0, pow(2, data_addr_width).toInt) map { 
          case 2 => daisyIO.valid
          case 3 => Bool(true)
          case 4 => topIO.valid
          case _ => Bool(false)
        })
        val daisyRdy = readAddr === UInt(2) && slaveIO.ready
        daisyRdy.getNode.component = slave
        daisyRdy.getNode setName "daisy_rdy"
        if (!(slave.debugs contains daisyRdy.getNode))
          slave.debugs += daisyRdy.getNode

        // address == 2 => Read from daisy chains
        // address == 3 => Check whether or not the target finished
        // address == 4 => Read from the device result
        
        /*
        daisyIO.ready := (readAddr === UInt(2, data_addr_width)) && slaveIO.ready
        slaveIO.valid := valids(readAddr)
        slaveIO.bits  := bits(readAddr)
  
        // ready to get inputs for the target
        when(topIO.valid) {
          inputRdy := Bool(true)
        }
        */

        inputRdy.comp.updates += ((Bool(true), inputRdy))
        inputRdy.comp.updates += ((topIO.valid, Bool(true)))
        daisyIO.ready.updates += ((Bool(true), daisyRdy))
        slaveIO.valid.updates += ((Bool(true), valids(readAddr)))
        slaveIO.bits match {
          case outBits: Bits => outBits.updates  += ((Bool(true), bits(readAddr)))
          case _ => // Error?
        }
      }
      case _ => // Error?
    }

    // for the top's input ports
    (slaveAddr, slaveIn, top.io("in"), top.io("stall"), top.io("daisy_control")) match {
      case (addr: Bits, 
            slaveIO: DecoupledIO[_], 
            topIO: DecoupledIO[_], 
            topStall: Bits, 
            daisyControl: Bits) => {
        /*
        top.io("stall") := stall 
        top.io("daisy_control") := addr(daisy_ctrl_width-1, 0)
        */
        val daisyAddr = addr(daisy_ctrl_width-1, 0)
        daisyAddr.getNode.component = slave
        daisyAddr.getNode setName "daisy_addr"
        if (!(slave.debugs contains daisyAddr.getNode))
          slave.debugs += daisyAddr.getNode
        topStall.updates     += ((Bool(true), stall))
        daisyControl.updates += ((Bool(true), daisyAddr))

        val ready = Vec(Range(0, pow(2, data_addr_width).toInt) map { 
          case 0 => topIO.ready
          case 1 => Bool(true)
          case _ => Bool(false)
        })
        val writeAddr = addr(addr_width-1, daisy_ctrl_width) 
        val topIn = slaveIO.valid && stall && writeAddr === UInt(0)
        val clkIn = slaveIO.valid && stall && writeAddr === UInt(1)
        val topIn2 = slaveIO.valid && stall && writeAddr === UInt(2)
        val slaveRdy = inputRdy || ready(writeAddr)
        val topVal = inputRdy || topIn2

        topIn.getNode.component = slave
        clkIn.getNode.component = slave
        slaveRdy.getNode.component = slave
        topVal.getNode.component = slave
        topIn.getNode setName ("top_in")
        clkIn.getNode setName ("clk_in")
        slaveRdy.getNode setName ("slave_in_rdy")
        topVal.getNode setName ("top_in_val")

        if (!(slave.debugs contains clkIn.getNode))
          slave.debugs += clkIn.getNode
        if (!(slave.debugs contains topIn.getNode))
          slave.debugs += topIn.getNode
        if (!(slave.debugs contains slaveRdy.getNode))
          slave.debugs += slaveRdy.getNode
        if (!(slave.debugs contains topVal.getNode))
          slave.debugs += topVal.getNode


        // address = 0 => Set inputs to the device
        // address = 1 => Write to the clock counter
        /*
        slaveIO.ready := inputRdy || ready(writeAddr)
        topIO.valid := inputRdy || writeValid(2)
        */
        slaveIO.ready.updates += ((Bool(true), slaveRdy))
        topIO.valid.updates   += ((Bool(true), topVal))
       
        (slaveIO.bits, topIO.bits) match {
          case (bits: Bits, bundle: Bundle) => {
            val (a, b) = (bundle.elements(0)._2, bundle.elements(1)._2) match {
              case (bitsA: Bits, bitsB: Bits) => (bitsA, bitsB)
              case _ => (Bits(0), Bits(0)) // error?
            }
            // val (name0, a) = bundle.elements(0)
            // val (name1, b) = bundle.elements(1)
            val clkCounterIsOne = clkCounter === Bits(1)
            val clkCounterDecr  = clkCounter - Bits(1, 32)
            clkCounterIsOne.getNode.component = slave
            clkCounterDecr.getNode.component = slave
            clkCounterIsOne.getNode setName "clk_cnt_is_one"
            clkCounterDecr.getNode setName "clk_cnt_decr"
            if (slave.debugs contains clkCounterIsOne.getNode)
              slave.debugs += clkCounterIsOne.getNode
            if (slave.debugs contains clkCounterDecr.getNode)
              slave.debugs += clkCounterDecr.getNode

            // todo: stop with new inputs?
            /*
            when (writeValid(0)) {
              topInBits := bits
              init := Bool(false)
            }.elsewhen (writeValid(1)) {
              clkCounter := slaveIO.bits
              inputRdy := Bool(false)
              stop := Bool(false)
            }.elsewhen (clkCounter === Bits(1, 32)) {
              clkCounter := clkCounter - Bits(1, 32)
              stop := Bool(true)
            }.elsewhen (!stall) {
              clkCounter := clkCounter - Bits(1, 32)
            }

            a := topInBits(31, 16)
            b := topInBits(15, 0)
            */
            stop.comp.updates       += ((Bool(true), stop))
            stop.comp.updates       += ((clkCounterIsOne, Bool(true)))
            stop.comp.updates       += ((clkIn, Bool(false)))
            init.comp.updates       += ((Bool(true), init))
            init.comp.updates       += ((topIn, Bool(false)))
            topInBits.comp.updates  += ((Bool(true), topInBits))
            topInBits.comp.updates  += ((topIn, bits))
            clkCounter.comp.updates += ((Bool(true), clkCounter))
            clkCounter.comp.updates += ((notStall, clkCounterDecr))
            clkCounter.comp.updates += ((clkIn, slaveIO.bits))
            inputRdy.comp.updates   += ((clkIn, Bool(false)))
            a.updates               += ((Bool(true), topInBits(31, 16)))
            b.updates               += ((Bool(true), topInBits(15, 0)))
          }
        }
      }
      case _ => // Error?
    }

    // emulation clock
    // val clockType = getTypeNode(daisyClock)
    // val clockBool = clockType.toBool
    val notStop = !stop
    val enable = (notStall && notStop || init) /* && clockBool */
    // clockBool.isTypeNode = true
    enable.getNode.component = slave
    enable.getNode setName "emul_clk"
    emulClock enabledBy (daisyClock, enable)

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

  private def reportSignals(m: Module) {
    val rptdir  = ensureDir(targetdir+"report")
    val rptfile = new java.io.FileWriter(rptdir+"%s_signal.rpt".format(m.name))
    val report = new StringBuilder();

    ChiselError.info("Backannotation: report annotated signals")

    report append "\t\t+-------------------------------------+\n"
    report append "\t\t|     Signal and Conter Report        |\n"
    report append "\t\t|                     by Donggyu Kim  |\n"
    report append "\t\t+-------------------------------------+\n\n"

    Module.sortedComps map { module =>
      report append "Module: %s\n".format(module.getPathName)
      module.nodes map { node =>
       if (signals contains node) {
          val counter = node.counter
          report append "  %s: %s => %s ==> %s\n".format(
            getSignalPathName(node), 
            nodeToString(node), 
            getSignalPathName(counter), 
            nodeToString(counter.inputs(0))
          )
       }
      }
    } 

    ChiselError.info(report.result)

    try {
      rptfile.write(report.result)
    } finally {
      rptfile.close()
    }
  }
}
