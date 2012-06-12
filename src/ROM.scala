package Chisel
import ChiselError._
import Node._
import scala.collection.mutable.ArrayBuffer

class ROM[T <: Data](lits: Seq[Literal], gen: () => T) extends Vec[T](gen) {
  val data = gen().toNode
  inferWidth = fixWidth(gen().getWidth)

  override def read(addr: UFix): T = {
    var port = new ROMRead(this, addr)
    val data = gen().asOutput
    data.setIsTypeNode
    data assign port
    data
  }

  override def write(addr: UFix, data: T) = {
    ChiselErrors += ChiselError("Can't write to ROM", Thread.currentThread().getStackTrace)
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
      "  " + emitRef + ".put(" + i + ", " + lit.emitRef + ");\n"
    }.reduceLeft(_ + _)
  }

  override def isReg = true
}

class ROMRead[T <: Data](rom: ROM[T], addri: Bits) extends Node {
  def addr = inputs(0)
  inputs += addri
  inputs += rom

  inferWidth = fixWidth(rom.data.getWidth)

  override def toString: String = rom + "[" + addr + "]"
  override def emitDefLoC: String = 
    "  " + emitTmp + " = " + rom.emitRef + ".get(" + addr.emitRef + ");\n"
  override def emitDef: String =
    "  assign " + emitTmp + " = " + rom.emitRef + "[" + addr.emitRef + "];\n"
}
