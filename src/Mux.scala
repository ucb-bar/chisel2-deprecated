// author: jonathan bachrach
package Chisel {

import Node._;
import Component._;
import scala.math._

object Multiplex{
  def apply (t: Node, c: Node, a: Node): Node = {
    if (isFolding) {
      if (t.litOf != null) 
        return if (t.litOf.value == 0) a else c
      if (c.litOf != null && a.litOf != null) {
        if (c.litOf.value == a.litOf.value)
          return c
        if (c.litOf.value == 1 && a.litOf.value == 0){
          if(c.litOf.width == 1 && a.litOf.width == 1) return t
          val fill = NodeFill(max(c.litOf.width-1, a.litOf.width-1), Literal(0,1))
          fill.infer
          val bit = NodeExtract(t, 0)
          bit.infer
          val cat = Concatanate(fill, bit)
          cat.infer
          return cat
        }
      }
    }
    new Mux().init("", maxWidth _, t, c, a);
  }
}


object Mux {
  def apply[T <: Data](t: Bits, c: T, a: T): T = {
    val res = Multiplex(t, c.toNode, a.toNode)
    // make output
    val output = c.fromNode(res).asInstanceOf[T]
    output.setIsCellIO
    res.nameHolder = output
    output
  }
}
class Mux extends Op {
  muxes += this;
  stack = Thread.currentThread.getStackTrace;
  op = "Mux";
  override def toString: String =
    inputs(0) + " ? " + inputs(1) + " : " + inputs(2)
  override def emitDef: String = 
    "  assign " + emitTmp + " = " + inputs(0).emitRef + " ? " + inputs(1).emitRef + " : " + inputs(2).emitRef + ";\n"
  override def emitDefLoC: String = 
    "  " + emitTmp + " = mux<" + width + ">(" + inputs(0).emitRef + ", " + inputs(1).emitRef + ", " + inputs(2).emitRef + ");\n"
  def ::(a: Node): Mux = { inputs(2) = a; this }
}


}
