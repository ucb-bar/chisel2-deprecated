package Chisel
import ChiselError._
import Node._
import scala.collection.mutable.ArrayBuffer

object Mem {
  def apply[T <: Data](n: Int, seqRead: Boolean = false)(gen: => T): Mem[T] = 
    new Mem(n, seqRead, () => gen)
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
    val data = gen().asOutput
    data.setIsTypeNode
    data assign rd
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

  override def toString: String = "TMEM(" + ")"

  private def getPathName = component.getPathName + "_" + emitRef

  override def emitDef: String = {
    if (Component.isInlineMem)
      return ""

    reads.filter(r => r.used && r.getPortType == "read").foreach { r =>
      val pairedWrite = writes.find(w => w.used && w.isPossibleRW(r))
      if (!pairedWrite.isEmpty) {
        pairedWrite.get.setRW(r)
        ports -= r
      }
    }

    val usedports = ports.filter(_.used)
    val portdefs = usedports.zipWithIndex.map { case (p, i) => p.emitPortDef(i) }

    Component.configStr +=
      "name " + getPathName +
      " depth " + n +
      " width " + width +
      " ports " + usedports.map(_.getPortType).reduceLeft(_ + "," + _) +
      "\n"

    val clkrst = Array("    .CLK(clk)", "    .RST(reset)")
    "  " + getPathName + " " + emitRef + " (\n" +
    (clkrst ++ portdefs).reduceLeft(_ + ",\n" + _) + "\n" +
    "  );\n"
  }

  override def emitDec: String = {
    if (Component.isInlineMem)
      "  reg [" + (width-1) + ":0] " + emitRef + " [" + (n-1) + ":0];\n"
    else
      ""
  }

  override def emitDecC: String = 
    "  mem_t<" + width + "," + n + "> " + emitRef + ";\n"

  override def emitInitC: String = "  if (random_initialization) "+emitRef+".randomize();\n"
}

abstract class MemAccess(val condi: Bool, val addri: Bits) extends Node {
  def cond = inputs(0)
  def addr = inputs(1)
  inputs ++= Array(condi, addri)

  def emitPortDef(idx: Int): String
  def getPortType: String

  var referenced = false
  override def emitDec: String = {
    referenced = true
    super.emitDec
  }
  def used = referenced

  var outputReg = null.asInstanceOf[Reg]
  def setOutputReg(x: Reg) = outputReg = x
}

class MemRead[T <: Data](val mem: Mem[T], condi: Bool, addri: Bits) extends MemAccess(condi, addri) {
  inputs += mem
  inferWidth = fixWidth(mem.data.getWidth)
  var reader = null.asInstanceOf[Node]

  override def toString: String = mem + "[" + addr + "]"
  override def emitDefLoC: String = 
    "  " + emitTmp + " = " + mem.emitRef + ".get(" + addr.emitRef + ");\n"
  override def emitDef: String = {
    if (Component.isInlineMem)
      "  assign " + emitTmp + " = " + mem.emitRef + "[" + addr.emitRef + "];\n"
    else
      ""
  }
  override def emitPortDef(idx: Int): String = {
    "    .A" + idx + "(" + addr.emitRef + "),\n" +
    "    .CS" + idx + "(" + cond.emitRef + "),\n" +
    "    .O" + idx + "(" + emitTmp + ")"
  }
  override def getPortType: String = if (outputReg != null && outputReg.isMemOutput) "read" else "cread"
}

class MemWrite[T <: Data](val mem: Mem[T], condi: Bool, addri: Bits, datai: T, wmaski: Bits) extends MemAccess(condi, addri) with proc {
  if (datai != null)
    inputs += datai
  if (wmaski != null) {
    require(datai != null)
    inputs += wmaski
  }

  var pairedRead: MemRead[T] = null
  private def emitRWEnable(r: MemRead[T]) = {
    def getProducts(x: Node): List[Node] = {
      if (x.isInstanceOf[Op]) {
        val op = x.asInstanceOf[Op]
        if (op.op == "&&")
          return getProducts(op.inputs(0)) ++ getProducts(op.inputs(1))
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
    inputs += src
  }
  override def toString: String = mem + "[" + addr + "] = " + data + " COND " + cond
  override def emitDefHiC: String = {
    if (inputs.length == 2)
      return ""
    isHiC = true
    var res = "  if (" + cond.emitRef + ".to_bool()) {\n"
    if (isMasked)
      res += "    " + mem.emitRef + ".put(" + addr.emitRef + ", (" + data.emitRef + " & " + wmask.emitRef + ") | (" + mem.emitRef + ".get(" + addr.emitRef + ") & ~" + wmask.emitRef + "));\n"
    else
      res += "    " + mem.emitRef + ".put(" + addr.emitRef + ", " + data.emitRef + ");\n"
    res += "  }\n"
    isHiC = false
    res
  }
  override def emitDef: String = {
    if (!used || !Component.isInlineMem)
      return ""

    val i = "i" + emitTmp
    if (isMasked)
      "  generate\n" +
      "    genvar " + i + ";\n" +
      "    for (" + i + " = 0; " + i + " < " + mem.width + "; " + i + " = " + i + " + 1) begin: f" + emitTmp + "\n" +
      "      always @(posedge clk)\n" +
      "        if (" + cond.emitRef + " && " + wmask.emitRef + "[" + i + "])\n" +
      "          " + mem.emitRef + "[" + addr.emitRef + "][" + i + "] <= " + data.emitRef + "[" + i + "];\n" +
      "    end\n" +
      "  endgenerate\n"
    else
      "  always @(posedge clk)\n" +
      "    if (" + cond.emitRef + ")\n" +
      "      " + mem.emitRef + "[" + addr.emitRef + "] <= " + data.emitRef + ";\n"
  }
  override def emitPortDef(idx: Int): String = {
    var we = "1'b1"
    var a = addr.emitRef
    var cs = cond.emitRef
    var res = ""

    if (isRW) {
      we = emitRWEnable(pairedRead).get.emitRef
      cs = cs + " || " + pairedRead.cond.emitRef
      if (addr != pairedRead.addr)
        a = we + " ? " + a + " : " + pairedRead.addr.emitRef
      res += "    .O" + idx + "(" + pairedRead.emitTmp + "),\n"
    }
    if (isMasked)
      res += "    .WBM" + idx + "(" + wmask.emitRef + "),\n"

    res +
    "    .A" + idx + "(" + a + "),\n" +
    "    .WE" + idx + "(" + we + "),\n" +
    "    .CS" + idx + "(" + cs + "),\n" +
    "    .I" + idx + "(" + data.emitRef + ")"
  }
  override def getPortType: String = (if (isMasked) "m" else "") + (if (isRW) "rw" else "write")
  override def used = super.used && inputs.length > 2
  override def isRamWriteInput(n: Node) = inputs.contains(n)
}
