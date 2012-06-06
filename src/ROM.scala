// authors: jonathan bachrach, andrew waterman
package Chisel

import ChiselError._
import Node._
import scala.collection.mutable.ArrayBuffer

object ROM {
  def apply[T <: Data](data: Array[T])(gen: => T): ROM[T] = {
    new ROM(data, () => gen)
  }
}

class ROM[T <: Data](datai: Array[T], gen: () => T) extends AccessTracker {
  def writeAccesses = ArrayBuffer[MemAccess]()
  def readAccesses = reads.map((x: MemAccess) => x)
  val reads = ArrayBuffer[ROMRead[T]]()
  val lits = datai.map(_.litOf)
  val data = gen().toNode

  if (lits.contains(null))
    ChiselErrors += ChiselError("ROM data is not constant", Thread.currentThread().getStackTrace)

  inferWidth = fixWidth(gen().getWidth)

  def apply(addr: Bits): T = {
    var port = new ROMRead(this, addr)
    reads += port
    val data = gen().asOutput
    data.setIsTypeNode
    data assign port
    data
  }

  override def emitDec: String =
    "  reg [" + (width-1) + ":0] " + emitRef + " [" + (lits.length-1) + ":0];\n"

  override def emitDef: String = {
    val inits = lits.zipWithIndex.map { case (lit, i) =>
      "    " + emitRef + "[" + i + "] = " + lit.emitRef + ";\n"
    }

    "  initial begin\n" +
    inits.reduceLeft(_ + _) +
    "  end\n"
  }

  override def emitDecC: String = 
    "  mem_t<" + width + "," + lits.length + "> " + emitRef + ";\n"

  override def emitInitC: String = {
    lits.zipWithIndex.map { case (lit, i) =>
      "  " + emitRef + "[" + i + "] = " + lit.emitRef + ";\n"
    }.reduceLeft(_ + _)
  }
}

class ROMRead[T <: Data](val rom: ROM[T], addri: Bits) extends MemAccess(Bool(true), addri) {
  inputs += rom
  inferWidth = fixWidth(rom.data.getWidth)

  override def toString: String = rom + "[" + addr + "]"
  override def emitDefLoC: String = 
    "  " + emitTmp + " = " + rom.emitRef + ".get(" + addr.emitRef + ");\n"
  override def emitDef: String =
    "  assign " + emitTmp + " = " + rom.emitRef + "[" + addr.emitRef + "];\n"
  override def emitPortDef(idx: Int): String = ""
  override def getPortType: String = "read"
}
