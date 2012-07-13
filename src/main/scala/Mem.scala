package Chisel
import ChiselError._
import Node._
import scala.collection.mutable.ArrayBuffer

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
}

abstract class AccessTracker extends Delay {
  def writeAccesses: ArrayBuffer[MemAccess]
  def readAccesses: ArrayBuffer[MemAccess]
}

class Mem[T <: Data](val n: Int, seqRead: Boolean, gen: () => T) extends AccessTracker {
  def writeAccesses = writes.map((x: MemAccess) => x)
  def readAccesses = reads.map((x: MemAccess) => x)
  val ports = ArrayBuffer[MemAccess]()
  val writes = ArrayBuffer[MemWrite[T]]()
  val reads = ArrayBuffer[MemRead[T]]()
  val data = gen().toNode
  val inferSeqRead = seqRead && Component.isEmittingComponents

  inferWidth = fixWidth(data.getWidth)

  def doRead(addr: Bits, cond: Bool) = {
    val rd = new MemRead(this, cond, addr)
    ports += rd
    reads += rd
    val data = gen().fromNode(rd).asInstanceOf[T]
    data.setIsTypeNode
    //data assign rd
    (data, rd)
  }

  def doWrite(addr: Bits, cond: Bool, data: T, wmask: Bits) = {
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
    if (inferSeqRead && !Component.isInlineMem)
      rdata.memSource = rport
    rdata.comp = doWrite(addr, conds.top, null.asInstanceOf[T], null.asInstanceOf[Bits])
    rdata
  }

  override def isInVCD = false

  override def toString: String = "TMEM(" + ")"

  override def clone = new Mem(n, seqRead, gen)
}

abstract class MemAccess(val condi: Bool, val addri: Bits) extends Node {
  def cond = inputs(0)
  def addr = inputs(1)
  inputs ++= Array(condi, addri)

  var referenced = false
  def used = referenced
  var outputReg = null.asInstanceOf[Reg]
  def setOutputReg(x: Reg) = outputReg = x
  def getPortType: String
}

class MemRead[T <: Data](val mem: Mem[T], condi: Bool, addri: Bits) extends MemAccess(condi, addri) {
  inputs += mem
  inferWidth = fixWidth(mem.data.getWidth)
  var reader = null.asInstanceOf[Node]

  override def toString: String = mem + "[" + addr + "]"
  override def getPortType: String = if (outputReg != null && outputReg.isMemOutput) "read" else "cread"
}

class MemWrite[T <: Data](val mem: Mem[T], condi: Bool, addri: Bits, datai: T, wmaski: Bits) extends MemAccess(condi, addri) with proc {
  def wrap(x: Bits) = {
    if (Component.backendName == "v") {
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
  def isPossibleRW(r: MemRead[T]) = mem.inferSeqRead && !emitRWEnable(r).isEmpty && !isRW
  def isRW = pairedRead != null
  def setRW(r: MemRead[T]) = pairedRead = r
  def data = inputs(2)
  def wmask = inputs(3)
  def isMasked = inputs.length > 3
  override def procAssign(src: Node) = {
    require(inputs.length == 2)
    inputs += wrap(src.asInstanceOf[Data].toBits)
  }
  override def toString: String = mem + "[" + addr + "] = " + data + " COND " + cond
  override def getPortType: String = (if (isMasked) "m" else "") + (if (isRW) "rw" else "write")
  override def used = super.used && inputs.length > 2
  override def isRamWriteInput(n: Node) = inputs.contains(n)
}
