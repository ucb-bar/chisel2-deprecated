// author: jonathan bachrach
package Chisel {

import Node._;
import Component._;
import IOdir._;

object Multiplex{
  def apply (t: Node, c: Node, a: Node): Node = 
    new Mux().init("", maxWidth _, t, c, a);
}


class MuxCell[T <: dat_t](cI: T, aI: T) extends Cell {
  val io = new bundle_t(){val t = Input();
			val c: T = cI.clone().asInput();
			val a: T = aI.clone().asInput();
			val out: T = cI.clone().asOutput();
		      }
  io.setIsCellIO;
  val primitiveNode: Node = Multiplex(io.t, io.c.toBits, io.a.toBits);
  val fb = io.out.fromBits(primitiveNode).asInstanceOf[T];
  fb.setIsCellIO;
  fb ^^ io.out;
  primitiveNode.nameHolder = io.out;
}

object Mux {
  def apply[T <: dat_t](t: int_t, c: T, a: T): T = {
    val muxcell = new MuxCell(c, a);
    muxcell.io.t := t;
    muxcell.io.c <> c;
    muxcell.io.a <> a;
    muxcell.io.out
  }
}
class Mux extends Op {
  override def toString: String =
    inputs(0) + " ? " + inputs(1) + " : " + inputs(2)
  override def emitDef: String = 
    "  assign " + emitTmp + " = " + inputs(0).emitRef + " ? " + inputs(1).emitRef + " : " + inputs(2).emitRef + ";\n"
  override def emitDefLoC: String = 
    "  " + emitTmp + " = mux<" + width + ">(" + inputs(0).emitRef + ", " + inputs(1).emitRef + ", " + inputs(2).emitRef + ");\n"
  def ::(a: Node): Mux = { inputs(2) = a; this }
}


}
