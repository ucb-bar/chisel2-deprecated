package Chisel

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.math.pow
import scala.io.Source

trait CounterBackend extends Backend {
  val crosses = new ArrayBuffer[(Double, Array[Node])]

  val firedPins = new HashMap[Module, Bool]
  val firedRegs = new HashMap[Module, Bool]
  val daisyIns = new HashMap[Module, UInt]
  val daisyOuts = new HashMap[Module, DecoupledIO[UInt]]
  val daisyCtrls = new HashMap[Module, Bits]
  val counterCopy = new HashMap[Module, Bool]
  val counterRead = new HashMap[Module, Bool]
  val decoupledPins = new HashMap[Node, Bits]

  var counterIdx = -1

  override def backannotationTransforms {
    super.backannotationTransforms

    transforms += ((c: Module) => c bfs (_.addConsumers))

    if (Module.isBackannotating) {
      transforms += ((c: Module) => annotateSignals(c))
    }

    transforms += ((c: Module) => decoupleTarget(c))
    transforms += ((c: Module) => connectDaisyPins(c))
    transforms += ((c: Module) => generateCounters)
    transforms += ((c: Module) => generateDaisyChains)

    transforms += ((c: Module) => c.inferAll)
    transforms += ((c: Module) => c.forceMatchingWidths)
    transforms += ((c: Module) => c.removeTypeNodes)
    transforms += ((c: Module) => collectNodesIntoComp(initializeDFS))
  }

  /*
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
  */

  def emitCounterIdx = {
    counterIdx = counterIdx + 1
    counterIdx
  }

  def addPin(m: Module, pin: Data, name: String = "") {
    pin.component = m
    pin.isIo = true
    pin setName name
    (m.io) match {
      case io: Bundle => io += pin
    }
    pin setName ("io_" + name)

    pin match {
      case dio: DecoupledIO[_] => {
        dio.ready.component = m
        dio.valid.component = m
        dio.bits.component = m
        dio.ready.isIo = true
        dio.valid.isIo = true
        dio.bits.isIo = true
        dio.ready setName ("io_" + name + "_ready")
        dio.valid setName ("io_" + name + "_valid")
        dio.bits setName ("io_" + name + "_bits")
      }
      case vio: ValidIO[_] => {
        vio.valid.component = m
        vio.bits.component = m
        vio.valid.isIo = true
        vio.bits.isIo = true
        vio.valid setName ("io_" + name + "_valid")
        vio.bits setName ("io_" + name + "_bits")
      }
      case _ =>
    } 
  }

  private def annotateSignals(c: Module) {
    ChiselError.info("[Backannotation] annotate signals")

    try {
      // Read the signal list file
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
        for (node <- m.nodes) {
          val signalName = getSignalPathName(node, ".")
          if (signalNames contains signalName){
            m.signals += node
            signalNameMap(signalName) = node
            if (!(m.debugs contains node))
              m.debugs += node
          }
        }
        for ((reset, pin) <- m.resets) {
          val resetPinName = getSignalPathName(pin, ".")
          if (signalNames contains resetPinName) {
            m.signals += pin
            signalNameMap(resetPinName) = pin
            if (!(m.debugs contains pin))
              m.debugs += pin
          }
        }
      }
   
      for ((coeff, vars) <- coeffs) {
        val cross = vars map { x => signalNameMap getOrElse (x, null) }
        if (!(cross contains null)) {
          crosses += ((coeff, cross))
        }
      } 
    } catch {
      case ex: java.io.FileNotFoundException => 
        ChiselError.warning("[Backannotation] no signal file, no backannotation")
    }
  }

  def decoupleTarget(c: Module) {
    ChiselError.info("[CounterBackend] target decoupling")

    val clksPin = Decoupled(UInt(width = 32)).flip
    val clksReg = Reg(init = UInt(0, 32), clock = c.clock)
    val fired = clksReg.orR
    val firedPin = Bool(OUTPUT)
    val firedReg = Reg(init = Bool(false), next = fired, clock = c.clock)

    firedPin.inputs += fired
    firedReg.comp.component = c
    firedReg.comp setName "fired"
    firedReg.comp genMuxes firedReg.comp
    firedReg.comp.inputs += c.reset

    c.debug(clksReg)
    c.debug(firedReg)

    clksPin.ready.inputs += !fired
    clksReg.comp match {
      case reg: Reg => {
        reg.component = c
        reg setName "clks"
        reg.enable    = (clksPin.valid && !fired) || fired
        reg.isEnable  = true
        reg.updates   += ((clksPin.valid && !fired, clksPin.bits))
        reg.updates   += ((fired, clksReg - UInt(1)))
        reg genMuxes reg
        reg.inputs += c.reset
      }
    }

    for ((n, pin) <- c.io.asInstanceOf[Bundle].elements) {
      pin match {
        // wires -> insert buffer
        case in: Bits if in.dir == INPUT => {
          val in_reg = Reg(next = in, clock = c.clock)
          in_reg.comp.component = c
          in_reg.comp setName (in.pName + "_buffer") 
          in_reg.comp genMuxes in_reg.comp
          in_reg.comp.inputs += c.reset 
          for (consumer <- in.consumers) {
            val idx = consumer.inputs indexOf in
            consumer.inputs(idx) = in_reg
          }
          decoupledPins(in) = in_reg
        }
        case out: Bits if out.dir == OUTPUT => {
          val out_bits = out.inputs.head match {
            case bits: Bits => bits
            case any => UInt(any)
          }
          val out_reg = Reg(init = Bits(0, out.width), clock = c.clock)
          out_reg.comp match {
            case reg: Reg => {
              reg.component = c
              reg setName (out.pName + "_buffer")
              reg.enable   = fired
              reg.isEnable = true
              reg.updates += ((fired, out_bits))
              reg genMuxes reg
              reg.inputs += c.reset 
            }
          }
          out.inputs(0) = out_reg
          decoupledPins(out) = out_reg
        }
        // TODO: valid ios -> insert enabled buffer
        case vio: ValidIO[_] if vio.valid.dir == INPUT => {
          val in_bits = vio.bits match {
            case data: Data => data
            case any => UInt(any)
          }
          val in_reg = Reg(outType = in_bits, clock = c.clock)
          in_reg.comp.component = c
          in_reg.comp setName (vio.pName + "_buffer")
          in_reg.comp.updates += ((vio.valid, vio.bits))
          in_reg.comp genMuxes in_reg.comp
          in_reg.comp.inputs += c.reset
          for (consumer <- vio.valid.consumers) {
            val idx = consumer.inputs indexOf vio.valid
            consumer.inputs(idx) = fired
          }
          for (consumer <- vio.bits.consumers) {
            val idx = consumer.inputs indexOf vio.bits
            consumer.inputs(idx) = in_reg
          }
        }
        case vio: ValidIO[_] if vio.valid.dir == OUTPUT => {
          val out_bits = vio.bits match {
            case data: Data => data
            case any => UInt(any)
          }
          val out_valid = vio.valid.inputs.head match {
            case bool: Bool => bool
            case any => UInt(any).toBool
          }
          val out_reg = Reg(outType = out_bits, clock = c.clock)
          out_reg.comp.component = c
          out_reg.comp setName (vio.pName + "_buffer") 
          out_reg.comp.updates += ((out_valid, out_bits.inputs.head))
          out_reg.comp genMuxes out_reg.comp
          out_reg.comp.inputs += c.reset 
          vio.bits.inputs(0) = out_reg
          vio.valid.inputs(0) = fired
        }
        // decoupled ios => insert queue
        /*
        case dio: DecoupledIO[_] if dio.valid.dir == INPUT => {
          val in_q = Queue(dio)
          for (consumer <- dio.valid.consumers) {
            val idx = consumer.inputs indexOf dio.valid
            consumer.inputs(idx) = in_q.valid
          }
          for (consumer <- dio.bits.consumers) {
            val idx = consumer.inputs indexOf dio.bits
            consumer.inputs(idx) = in_q.bits
          }
          dio.ready.inputs(0) = in_q.ready && fired
        }
        case dio: DecoupledIO[_] if dio.valid.dir == OUTPUT => {
          val out_q = Queue(dio)
          for (consumer <- dio.ready.consumers) {
            val idx = consumer.inputs indexOf dio.ready
            consumer.inputs(idx) = out_q.ready && fired
          }
          dio.valid.inputs(0) = out_q.valid
          dio.bits.inputs(0) = out_q.bits
        } */
        case _ =>
      }
    }

    addPin(c, clksPin, "clks")
    addPin(c, firedPin, "fire")

    firedPins(c) = fired
    firedRegs(c) = firedReg

    val stack = new Stack[Module]
    stack push c

    while (!stack.isEmpty) {
      val top = stack.pop

      for (node <- top.nodes) {
        node match {
          case reg: Reg if this.isInstanceOf[VerilogBackend] && !Module.isBackannotating => {
            val enable = firedPins(top) && reg.enable
            reg.inputs(reg.enableIndex) = enable.getNode
          }
          case reg: Reg => {
            reg.inputs(0) = Multiplex(firedPins(top), reg.next, reg)
          }
          case mem: Mem[_] => {
            for (write <- mem.writeAccesses) {
              val en = write.inputs(1) match {
                case bool: Bool => bool
                case _ => UInt(write.inputs(1)).toBool
              }
              val newEn = firedPins(top) && en
              if (Module.isBackannotating)
                newEn.getNode setName (en.getNode.pName + "_fire")
              write.inputs(1) = newEn
            }
          }
          case _ =>
        }
      }

      for (child <- top.children) {
        val firedPin = Bool(INPUT)
        val firedReg = Reg(init = Bool(false), next = firedPin, clock = top.clock)

        addPin(child, firedPin, "fire")
        firedPin.inputs += firedPins(top)
        firedReg.comp setName "fired"
        firedReg.comp genMuxes firedReg.comp
        firedReg.comp.inputs += child.reset

        firedPins(child) = firedPin
        firedRegs(child) = firedReg
  
        stack push child
      }
    }
  }

  // Connect daisy pins of hierarchical modules
  def connectDaisyPins(c: Module) {
    ChiselError.info("[CounterBackend] connect daisy pins")

    val daisyOut = Decoupled(UInt(width = 32))
    val daisyCtrl = UInt(INPUT, 1)
    val daisyFire = daisyOut.ready && !firedPins(c)
    val copy = daisyFire && daisyCtrl === Bits(0)
    val read = daisyFire && daisyCtrl === Bits(1)

    addPin(c, daisyOut,  "daisy_out")
    addPin(c, daisyCtrl, "daisy_ctrl")
    copy.getNode setName "copy"
    read.getNode setName "read"

    daisyIns(c)    = UInt(0)
    daisyOuts(c)   = daisyOut
    daisyCtrls(c)  = daisyCtrl
    counterCopy(c) = copy
    counterRead(c) = read
    daisyOut.valid.inputs += daisyFire 

    val stack = new Stack[Module]
    stack push c

    while (!stack.isEmpty) {
      val m = stack.pop

      for (child <- m.children) {
        val daisyIn = UInt(INPUT, 32)
        val daisyOut = Decoupled(UInt(width = 32))
        val daisyCtrl = UInt(INPUT, 1)
        val daisyFire = daisyOut.ready && !firedPins(child)
        val copy = daisyFire && daisyCtrl === Bits(0)
        val read = daisyFire && daisyCtrl === Bits(1)

        addPin(child, daisyIn,   "daisy_in")
        addPin(child, daisyOut,  "daisy_out")
        addPin(child, daisyCtrl, "daisy_ctrl")
        daisyIns(child)      = daisyIn
        daisyOuts(child)     = daisyOut
        daisyCtrls(child)    = daisyCtrl

        daisyOut.valid.inputs += daisyFire
        daisyOut.ready.inputs += daisyOuts(m).ready
        daisyCtrl.inputs      += daisyCtrls(m)
        stack push child
      }

      if (!m.children.isEmpty && m != c) {
        val head = m.children.head
        daisyIns(head).inputs += daisyIns(m)
      }

      for (i <- 0 until m.children.size - 1) {
        val cur = m.children(i)
        val next = m.children(i+1)
        daisyOuts(next).bits.inputs += daisyIns(cur)
      }

      if (!m.children.isEmpty) {
        val last = m.children.last
        daisyOuts(m).bits.inputs += daisyOuts(last).bits
      }
    }
  }

  def generateCounters {
    ChiselError.info("[CounterBackend] generate counters")

    val stack = new Stack[Module]

    for (m <- Module.components) {
      val counterEnable = firedPins(m) || counterCopy(m)
      val shadowEnable = counterCopy(m) || counterRead(m)

      for (signal <- m.signals) {
        val signalWidth = signal.width
        val signalValue = 
          if (decoupledPins contains signal) decoupledPins(signal) 
          else UInt(signal)
        val counter = Reg(init = Bits(0, 32), clock = m.clock)
        val shadow  = Reg(init = Bits(0, 32), clock = m.clock)

        signal.counter = counter
        signal.shadow = shadow
        emitCounterIdx

        val counterValue = {
          // Signal
          if (signalWidth == 1) {
            counter + signalValue
          // Bus
          } else {
            val buffer = Reg(init = Bits(0, signalWidth), clock = m.clock)
            val xor = signalValue ^ buffer
            xor.inferWidth = (x: Node) => signalWidth
            val hd = PopCount(xor)
            buffer.comp match {
              case reg: Reg => {
                reg.component = m
                reg setName "buffer_%d".format(counterIdx)
                reg.enable   = firedPins(m)
                reg.isEnable = true
                reg.updates += ((firedPins(m), signalValue))
                reg genMuxes reg
                reg.inputs += m.reset
              }
            }
            counter + hd
          }
        }
        counterValue.getNode.component = m
        counterValue.getNode setName "c_value_%d".format(counterIdx)

        counter.comp match {
          case reg: Reg => {
            reg.component = m
            reg setName "counter_%d".format(counterIdx)
            reg.enable   = counterEnable
            reg.isEnable = true
            reg.updates += ((firedPins(m), counterValue))
            reg.updates += ((counterCopy(m), Bits(0)))
            reg genMuxes reg
            reg.inputs += m.reset
          }
        }

        shadow.comp match {
          case reg: Reg => {
            reg.component = m
            reg setName "shadow_%d".format(counterIdx)
            reg.isEnable = true
            reg.enable   = shadowEnable
          }
        }

        // for debugging
        if (Module.isBackannotating) {
          signal setName "signal_%d_%s".format(counterIdx, signal.pName)
        }
      }
    }
  }  
 
  def generateDaisyChains {
    ChiselError.info("[CounterBackend] daisy chaining")
 
    // Daisy chaining
    for (m <- Module.sortedComps) {
      val copy = counterCopy(m)
      val read = counterRead(m)

      // Copy logic
      // daisyOuts(m).valid.inputs += !firedPins(m)
      if (!m.signals.isEmpty) {
        val counter = m.signals.last.counter
        val head = m.signals.head.shadow
        val last = m.signals.last.shadow
        /* if (m == Module.topComponent) {
          val buf = Reg(next = head, clock = m.clock)
          buf.comp genMuxes buf.comp
          daisyOuts(m).bits.inputs += buf
        } else { */
          daisyOuts(m).bits.inputs += head
        // }
        last.comp.updates += ((copy, counter))
        last.comp.updates += ((read, daisyIns(m)))
        last.comp genMuxes last.comp
        last.comp.inputs += m.reset
      } else if (m.children.isEmpty) {
        daisyOuts(m).bits.inputs  += daisyIns(m)
      }

      for (i <- 0 until m.signals.size - 1) {
        val counter = m.signals(i).counter
        val cur = m.signals(i).shadow
        val next = m.signals(i+1).shadow
      
        cur.comp.updates += ((copy, counter))
        cur.comp.updates += ((read, next))
        cur.comp genMuxes cur.comp
        cur.comp.inputs += m.reset

        // Signals are collected in order
        // so that counter values are verified 
        // and power numbers are calculated
        Module.signals += m.signals(i) 
      }
      if (!m.signals.isEmpty) {
        Module.signals += m.signals.last
      }
    }
  }
}

class CounterCppBackend extends CppBackend with CounterBackend
class CounterVBackend extends VerilogBackend with CounterBackend

abstract class CounterTester[+T <: Module](c: T) extends Tester(c) {
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

  val clks = c.io("clks") match {
    case dio: DecoupledIO[_] => dio
  }
  val fire = c.io("fire") match {
    case bool: Bool => bool
  }
  val daisyCtrl = c.io("daisy_ctrl") match {
    case bits: Bits => bits
  }
  val daisyOut = c.io("daisy_out") match {
    case dio: DecoupledIO[_] => dio
  }
  val daisyOutBits = daisyOut.bits match {
    case bits: Bits => bits
  }

  def clock (n: Int) {
    val clk = emulatorCmd("step %d".format(n))
    println("  CLOCK %d".format(n))
  }

  override def reset(n: Int = 1) {
    super.reset(n)
    if (t >= 1) {
      for (signal <- Module.signals ; if signal.width > 1) {
        prevPeeks(signal) = 0
      }
    }
  }

  override def step (n: Int = 1) { 
    println("-------------------------")
    println("| Counter Strcture Step |")
    println("-------------------------")

    for (signal <- Module.signals) {
      counts(signal) = 0
    }

    // set clock register
    while(peek(clks.ready) == 0) {
      clock(1)
    }
  
    pokeBits(clks.bits, n)
    pokeBits(clks.valid, 1)
    clock(1)
    pokeBits(clks.valid, 0)

    // run the target until it is stalled
    println("*** RUN THE TAREGT / READ SIGNAL VALUES ***")
    for (i <- 0 until n) {
      for (signal <- Module.signals) {
        val curPeek = peekBits(signal)
        if (signal.width == 1) {
          counts(signal) += curPeek
        } else {
          counts(signal) += calcHD(curPeek, prevPeeks(signal))
          prevPeeks(signal) = curPeek
        }
      }
      clock(1)
    }
  
    println("*** CHECK COUNTER VALUES ***")
    for (signal <- Module.signals) {
      expect(signal.counter, counts(signal))
    }

    // daisy copy
    do {
      println("*** Daisy Copy ***")
      poke(daisyCtrl, 0)
      poke(daisyOut.ready, 1)
      clock(1)
    } while (peek(fire) == 1)

    println("--- CURRENT CHAIN ---")
    for (s <- Module.signals) {
      peek(s.shadow)
    }

    // daisy read
    for ((signal, i) <- Module.signals.zipWithIndex) {
      println("*** Daisy Output ***")
      do {
        poke(daisyCtrl, 1)
        poke(daisyOut.ready, 1)
        clock(1)
      } while (peek(fire) == 1)

      println("--- CURRENT CHAIN ---")
      for (s <- Module.signals) {
        peek(s.shadow)
      }
      println("---------------------")

      expect(daisyOutBits, counts(signal))
    }

    t += n
  }

  // initialization
  for (signal <- Module.signals ; if signal.width > 1) {
    prevPeeks(signal) = 0
  }
}

/*
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
*/

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
