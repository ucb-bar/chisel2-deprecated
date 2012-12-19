package Chisel
import ChiselError._
import Node._
import scala.collection.mutable.ArrayBuffer
import Component._

object Mem {
  def apply[T <: Data](n: Int, seqRead: Boolean = false)(gen: => T): Mem[T] = {

    // check valid gen
    val testGen = gen
    for((n, i) <- gen.flatten) {
      if (i.inputs.length > 0 || i.updates.length > 0) {
        throwException("Invalid Type Specifier for Reg")
      }
    }

    new Mem(n, seqRead, () => gen)
  }

  val sequentialReads = collection.mutable.HashSet[Node]()
  def retime(c: Component) = {
    if (Component.isInlineMem && !sequentialReads.isEmpty)
      for (n <- Component.nodes)
        for (i <- 0 until n.inputs.length)
          if (sequentialReads.contains(n.inputs(i)))
            n.inputs(i) = n.inputs(i).asInstanceOf[MemRead[_]].outputVal
  }
}

abstract class AccessTracker extends Delay {
  def writeAccesses: ArrayBuffer[MemAccess]
  def readAccesses: ArrayBuffer[MemAccess]
}

class Mem[T <: Data](val n: Int, val seqRead: Boolean, gen: () => T) extends AccessTracker {
  def writeAccesses = writes.map((x: MemAccess) => x)
  def readAccesses = reads.map((x: MemAccess) => x)
  val ports = ArrayBuffer[MemAccess]()
  val writes = ArrayBuffer[MemWrite[T]]()
  val reads = ArrayBuffer[MemRead[T]]()
  val data = gen().toNode

  inferWidth = fixWidth(data.getWidth)

  def doRead(addr: Bits, cond: Bool) = {
    val rd = new MemRead(this, cond, addr)
    ports += rd
    reads += rd
    val data = gen().fromNode(rd).asInstanceOf[T]
    data.setIsTypeNode
    (data, rd)
  }

  def doWrite(addr: Bits, condIn: Bool, data: T, wmask: Bits) = {
    val cond = // add bounds check if depth is not a power of 2
      if (isPow2(n)) condIn
      else condIn && addr(log2Up(n)-1,0) < UFix(n)
    val wr = new MemWrite(this, cond, addr, data, wmask)
    ports += wr
    writes += wr
    inputs += wr
    wr
  }

  def read(addr: Bits): T = doRead(addr, conds.top)._1

  def write(addr: Bits, data: T) = doWrite(addr, conds.top, data, null.asInstanceOf[Bits])

  def write(addr: Bits, data: T, wmask: Bits) = doWrite(addr, conds.top, data, wmask)

  def apply(addr: Bits) = {
    val (rdata, rport) = doRead(addr, conds.top)
    if (Component.isEmittingComponents && seqRead)
      rdata.memSource = rport
    rdata.comp = doWrite(addr, conds.top, null.asInstanceOf[T], null.asInstanceOf[Bits])
    rdata
  }

  override def isInVCD = false

  override def toString: String = "TMEM(" + ")"

  override def clone = new Mem(n, seqRead, gen)

  override def genSubNodes: Unit = {
    for (i <- 0 until backend.words(this)) {
      val w = backend.thisWordBits(this, i)
      val m = new Mem(n, seqRead, () => Bits(width = w))
      m.width_ = w
      // println("MEM SUBNODE " + i + " " + m + " WIDTH = " + m.width)
      setSubNode(i, m)
    }
  }
}

abstract class MemAccess(val mem: Mem[_], val condi: Bool, val addri: Bits) extends Node {
  def cond = inputs(0)
  def addr = inputs(1)
  inputs ++= Array(condi, addri)

  var referenced = false
  def used = referenced
  def getPortType: String

  override def forceMatchingWidths =
    if (addr.width != log2Up(mem.n)) inputs(1) = addr.matchWidth(log2Up(mem.n))
}

object RawMemRead {
  def apply(mem: Node, condi: Node, addri: Node) = {
    val m  = mem.asInstanceOf[Mem[Bits]]
    val bc = Bool();
    val ba = Bits();
    val (d, mr) = m.doRead(ba, bc)
    mr.width_   = m.width
    // println("MEM READ " + mr + " CONDI = " + condi)
    mr.inputs(0) = condi
    mr.inputs(1) = addri
    mr
  }
}

class MemRead[T <: Data](mem: Mem[T], condi: Bool, addri: Bits) extends MemAccess(mem, condi, addri) {
  inputs += mem
  inferWidth = fixWidth(mem.data.getWidth)

  var outputReg: Reg = null
  def outputVal = if (Component.isInlineMem && isSequential) inputs.last else this
  def setOutputReg(x: Reg) = {
    if (Component.isInlineMem) {
      Mem.sequentialReads += this
      // retime the read across the output register
      val r = Reg() { Bits() }
      r := addr.asInstanceOf[Bits]
      inputs += mem.read(r)
    }
    outputReg = x
  }
  def isSequential = outputReg != null && outputReg.isMemOutput
  override def toString: String = mem + "[" + addr + "]"
  override def getPortType: String = if (isSequential) "read" else "cread"

  override def genSubNodes: Unit = {
    for (i <- 0 until backend.words(mem)) {
      val c = if (cond == null) condi else cond.getSubNode(0)
      val m = mem.getSubNode(i)
      val r = RawMemRead(m, c, addr.getSubNode(0)) // TODO: FILL OUT
      setSubNode(i, r)
      // println("MEM READ " + mem.name + " MEM-WIDTH " + m.width + " MEM-READ-WIDTH " + r.width + " " + i + " ADDR " + addr + " IS TYPE " + addr.isTypeNode)
    }
  }
}

object RawMemWrite {
  def apply(mem: Node, condi: Node, addri: Node, datai: Node, wmaski: Node) = {
    // FAKE OUT MemWrite WITH DUMMY INPUTS
    val m  = mem.asInstanceOf[Mem[Bits]]
    val bc = Bool()
    val ba = Bits()
    val bd = Bits()
    val bm = if (wmaski == null) null else Bits()
    val mw = m.doWrite(ba, bc, bd, bm)
    // REPLACE WITH REAL INPUTS
    mw.inputs(0) = condi
    mw.inputs(1) = addri
    mw.inputs(2) = datai
    if (wmaski != null)
      mw.inputs(3) = wmaski
    condi.consumers += mw
    addri.consumers += mw
    datai.consumers += mw
    mw
  }
}

class MemWrite[T <: Data](mem: Mem[T], condi: Bool, addri: Bits, datai: T, wmaski: Bits) extends MemAccess(mem, condi, addri) with proc {
  def wrap(x: Node) = {
    if (Component.backend.isInstanceOf[VerilogBackend]) {
      // prevent verilog syntax error when indexing a literal (e.g. 8'hff[1])
      val b = Bits()
      b.inputs += x
      b
    } else
      x
  }
  if (datai != null)
    inputs += wrap(datai.toBits)
  if (wmaski != null) {
    require(datai != null)
    inputs += wrap(wmaski)
  }

  override def forceMatchingWidths = {
    val w = mem.width
    super.forceMatchingWidths
    if(inputs.length >= 3 && inputs(2).width != w) inputs(2) = inputs(2).matchWidth(w)
    if(inputs.length >= 4 && inputs(3).width != w) inputs(3) = inputs(3).matchWidth(w)
  }

  var pairedRead: MemRead[T] = null
  def emitRWEnable(r: MemRead[T]) = {
    def getProducts(x: Node): List[Node] = {
      if (x.isInstanceOf[Op]) {
        val op = x.asInstanceOf[Op]
        if (op.op == "&&")
          return List(x) ++ getProducts(op.inputs(0)) ++ getProducts(op.inputs(1))
      }
      List(x)
    }
    def isNegOf(x: Node, y: Node) = x.isInstanceOf[Op] && x.asInstanceOf[Op].op == "!" && x.inputs(0) == y

    val wp = getProducts(cond)
    val rp = getProducts(r.cond)
    wp.find(wc => rp.exists(rc => isNegOf(rc, wc) || isNegOf(wc, rc)))
  }
  def isPossibleRW(r: MemRead[T]) = mem.seqRead && !emitRWEnable(r).isEmpty && !isRW
  def isRW = pairedRead != null
  def setRW(r: MemRead[T]) = pairedRead = r
  def data = inputs(2)
  def wmask = inputs(3)
  def isMasked = inputs.length > 3
  override def procAssign(src: Node) = {
    require(inputs.length == 2)
    inputs += wrap(src)
  }
  override def toString: String = mem + "[" + addr + "] = " + (if (inputs.length > 2) data + " COND " + cond else "")
  override def getPortType: String = (if (isMasked) "m" else "") + (if (isRW) "rw" else "write")
  override def used = inputs.length > 2
  override def isRamWriteInput(n: Node) = inputs.contains(n)

  override def genSubNodes: Unit = {
    if (inputs.length > 2) {
      for (i <- 0 until backend.words(mem)) {
        val c = if (cond == null) cond else cond.getSubNode(i)
        val w = if (inputs.length == 4) wmask.getSubNode(i) else null
        // println("MEM WRITE SUBNODE " + mem.name + " " + i)
        setSubNode(i, RawMemWrite(mem.getSubNode(i), c, addr.getSubNode(0), data.getSubNode(i), w)) // TODO: FILL OUT
      }
    }
  }
}
