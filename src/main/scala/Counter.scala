package Chisel

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.math.pow
import scala.io.Source

case class CounterConfiguration(
  addrWidth: Int = 5,
  dataWidth: Int = 32,
  daisyCtrlWidth: Int = 1,
  counterWidth: Int = 32) 
{
  val n = pow(2, addrWidth - daisyCtrlWidth).toInt
}

class CounterWrapperIO(conf: CounterConfiguration) extends Bundle {
  val in = Decoupled(Bits(width = conf.dataWidth)).flip
  val out = Decoupled(Bits(width = conf.dataWidth))
  val addr = Bits(INPUT, conf.addrWidth)
}

abstract class CounterWrapper(val conf: CounterConfiguration) extends Module {
  val io = new CounterWrapperIO(conf)
  def top: Module

  def wen(i: Int) = io.in.valid && io.addr(log2Up(conf.n)-1, 0) === UInt(i)
  def ren(i: Int) = io.out.ready && io.addr(log2Up(conf.n)-1, 0) === UInt(i)
  val rdata = Vec.fill(conf.n){Bits(width = conf.dataWidth)}
  val rvalid = Vec.fill(conf.n){Bool()}
  val wready = Vec.fill(conf.n){Bool()}

  val clks = Reg(init = UInt(0))
  val fire = clks != UInt(0)

  clks.comp setName "clks"
  fire.getNode setName "fire"

  // debug(fire)
 
  io.in.ready := wready(io.addr)
  io.out.valid := rvalid(io.addr)
  io.out.bits := rdata(io.addr)

  // write(aar = 4) -> clks
  when(wen(4) && !fire) {
    clks := io.in.bits
  }.elsewhen(fire) {
    clks := clks - UInt(1)
  }
  wready(4) := !fire
}

abstract class CounterTester[+T <: CounterWrapper](c: T, val clks: Int = 1) extends Tester(c) {
  val prevPeeks = new HashMap[Node, BigInt]
  val counts = new HashMap[Node, BigInt]

  def calcHD(a: BigInt, b: BigInt) = {
    var xor = a ^ b
    var hd: BigInt = 0
    while (xor > 0) {
      hd = hd + (xor & 1)
      xor = xor >> 1
    }
    hd
  }

  def pokeClear {
    poke(c.io.addr, 0)
    poke(c.io.in.valid, 0)
    poke(c.io.in.bits, 0)
    poke(c.io.out.ready, 0)
    step(1)
  }

  def pokeReset {
    println("------------------------")
    println("|  Reset: write(31, 0) |")
    println("------------------------")
    // write (31, 0)
    while (peek(c.io.in.ready) == 0) {
      step(1)
    }
    poke(c.io.addr, 31)
    poke(c.io.in.valid, 1)
    poke(c.io.in.bits, 0)
    poke(c.io.out.ready, 0)
    step(1)
  }

  def pokeClks {
    println("------------------------------")
    println("| Poke clks: write(4, %4d) |".format(clks))
    println("------------------------------")
    // write (4, clks)
    while (peek(c.io.in.ready) == 0) {
      step(1)
    }
    poke(c.io.addr, 4)
    poke(c.io.in.bits, clks)
    poke(c.io.in.valid, 1)
    poke(c.io.out.ready, 0)
    for (signal <- Module.signals ; if signal.width > 1) {
      prevPeeks(signal) = peekBits(signal)
    }
    step(1)
  }

  def peekDaisyCopy = {
    println("--------------------------------")
    println("| Daisy copy: read(4 | 0 << 4) |")
    println("--------------------------------")
    // read (4 | 0 << 4)
    do {
      poke(c.io.addr, 4 | 0 << 4)
      poke(c.io.in.bits, 0)
      poke(c.io.in.valid, 0)
      poke(c.io.out.ready, 1)
      step(1)
    } while (peek(c.io.out.valid) == 0)

    peek(c.io.out.bits)
  }

  def peekDaisyRead = {
    println("--------------------------------")
    println("| Daisy read: read(4 | 1 << 4) |")
    println("--------------------------------")
    // read (4 | 1 << 4)
    do {
      poke(c.io.addr, 4 | 1 << 4)
      poke(c.io.in.bits, 0)
      poke(c.io.in.valid, 0)
      poke(c.io.out.ready, 1)
      step(1)
    } while (peek(c.io.out.valid) == 0)

    peek(c.io.out.bits)
  }

  def peekSignals {
    println("----------------------")
    println("|    Read signals    |")
    println("----------------------")
    step(1)
    for ((signal, i) <- Module.signals.zipWithIndex) {
      val curPeek = peekBits(signal)
      val cntrPeek = peek(signal.counter)
      if (signal.width == 1) {
        counts(signal) += curPeek
      } else {
        counts(signal) += calcHD(curPeek, prevPeeks(signal))
        prevPeeks(signal) = curPeek
      }
      println("  counts of counter_%d = %x".format(i, counts(signal)))
    }
  }

  var good = true
  var cycles = 0

  def loop {
    for (signal <- Module.signals) {
      counts(signal) = 0
    }

    pokeClks
    pokeClear

    while (peek(c.fire) == 1) {
      cycles += 1
      peekSignals
    }
    pokeClear
    pokeClear

    var ready: BigInt = 0
    var bits: BigInt = 0

    val read = peekDaisyCopy
    pokeClear
    pokeClear
    for (signal <- Module.signals) {
      peek(signal.shadow)
    }
    for ((signal, i) <- Module.signals.zipWithIndex) {
      val read = peekDaisyRead
      for (s <- Module.signals) {
        peek(s.shadow)
      }
      good &= expect(read == counts(signal), "Counter" + i)
      println("out bits: %x\tfrom signal: %x".format(
               read, counts(signal)))
    }
  }

  // initialization
  pokeReset
  for (signal <- Module.signals) {
    counts(signal) = 0
    prevPeeks(signal) = 0
  }
}

trait CounterBackend extends Backannotation {
  val crosses = new ArrayBuffer[(Double, Array[Node])]

  val fires = new HashMap[Module, Bool]
  val firedRegs = new HashMap[Module, Bool]
  val daisyIns = new HashMap[Module, ValidIO[UInt]]
  val daisyOuts = new HashMap[Module, DecoupledIO[UInt]]
  val daisyCtrls = new HashMap[Module, Bits]
  val counterCopy = new HashMap[Module, Bool]
  val counterRead = new HashMap[Module, Bool] 
  val counterRegs = new ArrayBuffer[proc]

  var counterIdx = -1

  override def backannotationTransforms {
    transforms += ((c: Module) => c bfs (_.addConsumers))

    transforms += ((c: Module) => annotateSignals(c))
    transforms += ((c: Module) => decoupleTarget(c))
    transforms += ((c: Module) => connectDaisyPins(c))
    transforms += ((c: Module) => generateCounters(c))
    transforms += ((c: Module) => generateDaisyChains)

    transforms += ((c: Module) => genCounterMuxes(c))
    transforms += ((c: Module) => c.inferAll)
    transforms += ((c: Module) => c.forceMatchingWidths)
    transforms += ((c: Module) => c.removeTypeNodes)
    transforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  }

  // analyses += ((c: Module) => reportCounters(c))

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

  override def checkBackannotation(c: Module) {
    c match {
      case m: CounterWrapper => super.checkBackannotation(m.top)
    }
  }

  def emitCounterIdx = {
    counterIdx = counterIdx + 1
    counterIdx
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

  private def annotateSignals(c: Module) {
    ChiselError.info("[Backannotation] annotate signals")

    // Read the signal list file
    // TODO: generalize the signal file format
    val lines = Source.fromFile(Module.model).getLines
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
    for (m <- Module.sortedComps ; if m != c) {
      // TODO: include resets
      /*
      val reset = m.reset
      val resetName = getSignalPathName(reset, ".")
      if (signalNames contains resetName) {
        m.signals += reset
        signalNameMap(resetName) = reset
        if (!(m.debugs contains reset))
          m.debugs += reset
      }
      */
      for (node <- m.nodes) {
        val signalName = getSignalPathName(node, ".")
        if ((signalNames contains signalName) && signalName != "reset"){
          m.signals += node
          signalNameMap(signalName) = node
          // Backannotated signals should be accessible by names
          // in emulators, so their names are given here
          // if (!this.isInstanceOf[VerilogBackend] && node.name == "")
          //   node setName node.pName
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

  def decoupleTarget(c: Module) {
    ChiselError.info("[CounterBackend] target decoupling")

    var counterConf: CounterConfiguration = null 
    c match {
      case m: CounterWrapper => {
        counterConf = m.conf
        val fired = Reg(init = Bool(false), next = m.fire, clock = m.clock)
        fired.comp.component = m
        fired.comp setName "fired"
        if (!(counterRegs contains fired.comp))
          counterRegs += fired.comp
        if (!(m.debugs contains fired.comp))
          m.debugs += fired.comp
        firedRegs(m) = fired
      }
    }


    for (m <- Module.sortedComps; if m != c) {
      // Add pins
      val fire = Bool(INPUT)
      val daisyIn = Valid(Bits(width = counterConf.dataWidth)).flip
      val daisyOut = Decoupled(Bits(width = counterConf.dataWidth))
      val daisyCtrl = Bits(INPUT, width = counterConf.daisyCtrlWidth)
      val fired = Reg(init = Bool(false), next = fire, clock = m.clock)
      fired.comp.component = m
      fired.comp setName "fired"
      if (!(counterRegs contains fired.comp))
        counterRegs += fired.comp
      if (!(m.debugs contains fired.comp))
        m.debugs += fired.comp
      
      def daisyReady(i: Int) = {
        val dready = daisyOut.ready && daisyCtrl === Bits(i)
        dready.getNode.component = m
        if (i == 0) { dready.getNode setName "copy" }
        if (i == 1) { dready.getNode setName "read" }
        /*
        if (!(m.debugs contains dready.getNode))
          m.debugs += dready.getNode
        */
        dready
      }

      addPin(m, fire,      "fire")
      addPin(m, daisyIn,   "daisy_in")
      addPin(m, daisyOut,  "daisy_out")
      addPin(m, daisyCtrl, "daisy_ctrl")

      fires(m)         = fire
      firedRegs(m)     = fired
      daisyIns(m)      = daisyIn
      daisyOuts(m)     = daisyOut
      daisyCtrls(m)    = daisyCtrl
      counterCopy(m)   = daisyReady(0)
      counterRead(m)   = daisyReady(1)

      // FAME1 Transforms
      for (node <- m.nodes) {
        node match {
          case reg: Reg if this.isInstanceOf[VerilogBackend] => {
            val enable = fire && reg.enable
            enable.getNode setName (reg.enable.getNode.pName + "_fire")
            reg.inputs(reg.enableIndex) = enable.getNode
          }
          case reg: Reg => {
            reg.inputs(0) = Multiplex(fire, reg.next, reg)
          }
          case mem: Mem[_] => {
            for (write <- mem.writeAccesses) {
              write.inputs(1) match {
                case en: Bool => { 
                  val newEn = fire && en
                  newEn.getNode setName (en.getNode.pName + "_fire")
                  write.inputs(1) = newEn
                }
              }
            }
          }
          case _ =>
        }
      }
      
      val parentFire      = fires      getOrElse (m.parent, null)
      val parentDaisyOut  = daisyOuts  getOrElse (m.parent, null)
      val parentDaisyCtrl = daisyCtrls getOrElse (m.parent, null)
      if (!(parentFire == null)) {
        fire.updates           += ((Bool(true), parentFire))
      }
      if (!(parentDaisyCtrl == null)) {
        daisyCtrl.updates      += ((Bool(true), parentDaisyCtrl))
      }
      if (!(parentDaisyOut == null)) {
        daisyOut.ready.updates += ((Bool(true), parentDaisyOut.ready))
      }
    }
  }

  // Connect daisy pins to support hierarchical modules
  def connectDaisyPins(c: Module) {
    ChiselError.info("[CounterBackend] connect daisy pins")

    val top = c match {
      case m: CounterWrapper => {
        val daisyReady = m.ren(4) && !firedRegs(m)
        m.rdata(4).updates             += ((Bool(true), daisyOuts(m.top).bits))
        m.rvalid(4).updates            += ((Bool(true), daisyOuts(m.top).valid))
        fires(m.top).updates           += ((Bool(true), m.fire))
        daisyIns(m.top).bits.updates   += ((Bool(true), UInt(0)))
        daisyIns(m.top).valid.updates  += ((Bool(true), daisyReady))
        daisyOuts(m.top).ready.updates += ((Bool(true), daisyReady))
    
        val daisyCtrl = m.io("addr") match {
          case addr: Bits => {
            if (m.conf.daisyCtrlWidth == 1) {
              addr(m.conf.addrWidth - 1)
            } else {
              addr(m.conf.addrWidth - 1, m.conf.addrWidth - m.conf.daisyCtrlWidth)
            }
          }
        }
        daisyCtrl.getNode.component = m
        daisyCtrls(m.top).updates += ((Bool(true), daisyCtrl))

        m.top
      }
    }

    val stack = new Stack[Module]
    val walked = new HashSet[Module]
    stack push top

    while (!stack.isEmpty) {
      val m = stack.pop

      if (!m.children.isEmpty) {
        val head = m.children.head
        daisyIns(head).bits.updates  += ((Bool(true), daisyIns(m).bits))
        daisyIns(head).valid.updates += ((Bool(true), daisyIns(m).valid))
      }

      for (i <- 0 until m.children.size - 1) {
        val cur = m.children(i)
        val next = m.children(i+1)
        daisyOuts(next).bits.updates  += ((Bool(true), daisyIns(cur).bits))
        daisyOuts(next).valid.updates += ((Bool(true), daisyIns(cur).valid))
      }

      if (!m.children.isEmpty) {
        val last = m.children.last
        daisyOuts(m).bits.updates  += ((Bool(true), daisyOuts(last).bits))
        daisyOuts(m).valid.updates += ((Bool(true), daisyOuts(last).valid))
      }

      for (child <- m.children) {
        stack push child
      }
    }
  }

  def generateCounters(c: Module) {
    ChiselError.info("[CounterBackend] generate counters")

    val stack = new Stack[Module]
    val walked = new HashSet[Module]
    val counterWidth = c match {
      case m: CounterWrapper => m.conf.counterWidth
    }

    for (m <- Module.sortedComps; if m != c) {
      val counterEnable = firedRegs(m) || counterCopy(m)
      val shadowEnable = counterCopy(m) || counterRead(m)

      for (signal <- m.signals) {
        val signalValue = UInt(signal)
        val signalWidth = signal.getWidth
        val counter = Reg(init = Bits(0, counterWidth), clock = m.clock)
        val shadow  = Reg(init = Bits(0, counterWidth), clock = m.clock)

        signal.counter = counter
        signal.shadow = shadow
        counterRegs += counter.comp
        counterRegs += shadow.comp 

        counter.comp match {
          case reg: Reg => {
            reg.component = m
            reg.enable   = counterEnable
            reg.isEnable = true
            reg setName "counter_%d".format(emitCounterIdx)
          }
        }
        shadow.comp match {
          case reg: Reg => {
            reg.component = m
            reg.enable   = shadowEnable
            reg.isEnable = true
            reg setName "shadow_%d".format(counterIdx)
          }
        }
      
        // Signal
        if (signalWidth == 1) {
          val counterValue = counter + signalValue
          counterValue.getNode.component = m
          counterValue.getNode setName "c_value_%d".format(counterIdx)
          /*
          if ( !(m.debugs contains counterValue.getNode))
            m.debugs += counterValue.getNode
          */
          counter.comp.updates += ((firedRegs(m), counterValue))
          counter.comp.updates += ((counterCopy(m), Bits(0)))
        // Bus
        } else {
          val buffer  = Reg(init = Bits(0, signalWidth), 
                            next = signalValue, 
                            clock = m.clock)
          val xor = signalValue ^ buffer
          val hd = PopCount(xor)
          val counterValue = counter + hd
          counterValue.getNode.component = m
          counterValue.getNode setName "c_value_%d".format(counterIdx)
          /*
          if ( !(m.debugs contains counterValue.getNode))
            m.debugs += counterValue.getNode
          */
          counterRegs += buffer.comp
          buffer.comp setName "buffer_%d".format(counterIdx)
          buffer.comp.component = m
          /*
          buffer.comp match {
            case reg: Reg => {
              reg.enable   = Bool(true)
              reg.isEnable = true
            }
          }
          buffer.comp.updates  += ((Bool(true), signalValue))
          */
          counter.comp.updates += ((firedRegs(m), counterValue))
          counter.comp.updates += ((counterCopy(m), Bits(0)))
        }

        // for debugging
        // if (this.isInstanceOf[VerilogBackend])
        signal setName "signal_%d_%s".format(counterIdx, signal.pName)
      }
    }
  }  
 
  def generateDaisyChains {
    ChiselError.info("[CounterBackend] generate daisy chains")
 
    // Daisy chaining
    for (m <- Module.sortedComps ; if !m.isInstanceOf[CounterWrapper]) {
      val copy = counterCopy(m)
      val read = counterRead(m)

      // Copy logic
      if (!m.signals.isEmpty) {
        val counter = m.signals.last.counter
        val head = m.signals.head.shadow
        val last = m.signals.last.shadow
        daisyOuts(m).bits.updates  += ((Bool(true), head))
        daisyOuts(m).valid.updates += ((Bool(true), !fires(m)))
        last.comp.updates += ((copy, counter))
        last.comp.updates += ((read, daisyIns(m).bits))
      } else if (m.children.isEmpty){
        daisyOuts(m).bits.updates  += ((Bool(true), daisyIns(m).bits))
        daisyOuts(m).valid.updates += ((Bool(true), !fires(m)))
      }

      for (i <- 0 until m.signals.size - 1) {
        val counter = m.signals(i).counter
        val cur = m.signals(i).shadow
        val next = m.signals(i+1).shadow
      
        cur.comp.updates += ((copy, counter))
        cur.comp.updates += ((read, next))

        // Signals are collected in order so that
        // counter values are verified and
        // power numbers are calculated
        Module.signals += m.signals(i) 
      }
      Module.signals += m.signals.last
    }
  }

  // Here, we should not generate muxes using genAllMuxes
  // Only genMuxes of new 'proc's are invoked
  // Otherwise, there will be side effects
  def genCounterMuxes (c: Module) {
    ChiselError.info("[CounterBackend] generate muxes for counter structure")

    c match {
      case m: CounterWrapper => {
        if (!m.rdata(4).updates.isEmpty)
          m.rdata(4).genMuxes(m.rdata(4).default)
        if (!m.rvalid(4).updates.isEmpty)
          m.rvalid(4).genMuxes(m.rvalid(4).default)
      }
    }

    for ((m, fire) <- fires ; if !fire.updates.isEmpty) {
      fire.genMuxes(fire.default)
    }
    for ((m, daisyIn) <- daisyIns) {
      val bits = daisyIn.bits
      val valid = daisyIn.valid
      if (!bits.updates.isEmpty) {
        bits.genMuxes(bits.default)
      }
      if (!valid.updates.isEmpty) {
        valid.genMuxes(valid.default)
      }
    }
    for ((m, daisyOut) <- daisyOuts) {
      val bits = daisyOut.bits
      val valid = daisyOut.valid
      val ready = daisyOut.ready
      if (!bits.updates.isEmpty) {
        bits.genMuxes(bits.default)
      }
      if (!valid.updates.isEmpty) {
        valid.genMuxes(valid.default)
      }
      if (!ready.updates.isEmpty) {
        ready.genMuxes(ready.default)
      }
    }
    for ((m, daisyCtrl) <- daisyCtrls ; if !daisyCtrl.updates.isEmpty) {
      daisyCtrl.genMuxes(daisyCtrl.default)
    } 
   
    for (reg <- counterRegs) {
      reg.genMuxes(reg)
      reg.inputs += reg.component.reset
    }
  }

  def reportCounters (m: Module) {
    val rptdir  = ensureDir(targetdir)
    val rptfile = new java.io.FileWriter(rptdir+"%s_signal.rpt".format(m.name))
    val report = new StringBuilder();

    ChiselError.info("[CounterBackend] report annotated signals")
    
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
