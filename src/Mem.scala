// authors: jonathan bachrach, andrew waterman
package Chisel

import ChiselError._
import Node._
import scala.collection.mutable.ArrayBuffer

object Mem {
  def apply[T <: Data](n: Int)(gen: => T): Mem[T] = 
    new Mem(n, () => gen)
}

abstract class AccessTracker extends Delay {
  def writeAccesses: ArrayBuffer[MemAccess]
  def readAccesses: ArrayBuffer[MemAccess]
}

class Mem[T <: Data](val n: Int, gen: () => T) extends AccessTracker {
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
    rdata.memSource = rport
    rdata.comp = doWrite(addr, conds.top, null.asInstanceOf[T], null.asInstanceOf[Bits])
    rdata
  }

  override def toString: String = "TMEM(" + ")"

  private def getPathName = component.getPathName + "_" + emitRef

  override def emitDef: String = {
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

  override def emitDec: String = ""

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

  def data = inputs(2)
  def wmask = inputs(3)
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
    if (inputs.length > 3)
      res += "    " + mem.emitRef + ".put(" + addr.emitRef + ", (" + data.emitRef + " & " + wmask.emitRef + ") | (" + mem.emitRef + ".get(" + addr.emitRef + ") & ~" + wmask.emitRef + "));\n"
    else
      res += "    " + mem.emitRef + ".put(" + addr.emitRef + ", " + data.emitRef + ");\n"
    res += "  }\n"
    isHiC = false
    res
  }
  override def emitPortDef(idx: Int): String = {
    "    .A" + idx + "(" + addr.emitRef + "),\n" +
    "    .CS" + idx + "(" + cond.emitRef + "),\n" +
    "    .WE" + idx + "(" + cond.emitRef + "),\n" +
    (if (inputs.length > 3) "    .WBM" + idx + "(" + wmask.emitRef + "),\n" else "") +
    "    .I" + idx + "(" + data.emitRef + ")"
  }
  override def getPortType: String = if (inputs.length > 3) "mwrite" else "write"
  override def used = super.used && inputs.length > 2
  override def isRamWriteInput(n: Node) = inputs.contains(n)
}
