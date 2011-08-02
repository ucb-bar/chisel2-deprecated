// author: jonathan bachrach
package Chisel {

import Node._;

object MemRef {
  def apply (mem: Node, addr: Node): Node = {
    val res = new MemRef();
    res.init("", widthOf(0), mem, addr);
    res
  }
  def apply[T <: dat_t](mem: Node, addr:Node, data: T): T = {
    val memRes = new MemRef();
    memRes.init("", widthOf(0), mem, addr);
    val res = data.fromBits(memRes).asInstanceOf[T];
    res
  }
}
class MemRef extends Node {
  override def toString: String = inputs(0) + "[" + inputs(1) + "]";
  override def emitDef: String = 
    "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + inputs(1).emitRef + "];\n"
  override def emitDefLoC: String = 
    "  " + emitTmp + " = " + inputs(0).emitRef + ".get(" + inputs(1).emitRef + ");\n"
}


}
