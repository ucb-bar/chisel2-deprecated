// author: jonathan bachrach
package Chisel {


import Node._;
import Component._;


object Bits {
  def apply (mod: Node, bit: Int): Node = 
    apply(mod, Lit(bit));
  def apply (mod: Node, hi: Int, lo: Int): Node = {
    val res = new Bits();
    res.hi = Lit(hi);
    res.lo = Lit(lo);
    res.init("", fixWidth(hi-lo+1), mod, res.hi, res.lo);
    res
  }
  def apply (mod: Node, bit: Node): Node = {
    val res = new Bits();
    res.init("", fixWidth(1), mod, bit);
    res.hi = bit; 
    res.lo = bit;
    res
  }
  def apply (mod: Node, hi: Node, lo: Node): Node = {
    val res = new Bits();
    res.init("", widthOf(0), mod, hi, lo);
    res.hi = hi;
    res.lo = lo;
    res
  }
}
class Bits extends Node {
  var lo: Node = null;
  var hi: Node = null;
  override def toString: String =
    if (hi == lo)
      "BITS(" + inputs(0) + ", " + lo + ")";
    else
      "BITS(" + inputs(0) + ", " + hi + ", " + lo + ")";
  override def emitDef: String = 
    if (hi == lo)
      "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + hi.emitRef + "];\n"
    else
      "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + hi.emitRef + ":" + lo.emitRef + "];\n"
  override def emitDefLoC: String = 
    if (hi == lo)
      "  " + emitTmp + " = " + inputs(0).emitRef + ".bit(" + hi.emitRef + ");\n"
    else
      "  " + emitTmp + " = " + inputs(0).emitRef + ".extract<" + width + ">(" + hi.emitRef + "," + lo.emitRef + ");\n"
}

}
