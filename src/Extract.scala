// author: jonathan bachrach
package Chisel {

import Node._;
import Component._;
import Lit._;

object NodeExtract {
  
  // extract one bit
  def apply (mod: Node, bit: Node): Node = {
    val (bits_lit, off_lit) = (mod.litOf, bit.litOf);
    if (isFolding && bits_lit != null && off_lit != null) {
      Literal((bits_lit.value >> off_lit.value.toInt)&1, 1)
    } else {
      val res = new Extract();
      res.init("", fixWidth(1), mod, bit);
      res.hi = bit; 
      res.lo = bit;
      res
    }
  }

  def apply (mod: Node, bit: Int): Node = 
    apply(mod, Literal(bit));

  // extract bit range
  def apply (mod: Node, hi: Node, lo: Node): Node = {
    val (bits_lit, hi_lit, lo_lit) = (mod.litOf, hi.litOf, lo.litOf);
    if (isFolding && bits_lit != null && hi_lit != null && lo_lit != null) {
      val w = (hi_lit.value - lo_lit.value + 1).toInt
      Literal((bits_lit.value >> lo_lit.value.toInt) & ((BigInt(1) << w) - BigInt(1)), w)
    } else {
      val res = new Extract();
      res.init("", widthOf(0), mod, hi, lo);
      res.hi = hi;
      res.lo = lo;
      res
    }
  }

  def apply (mod: Node, hi: Int, lo: Int): Node = {
    val bits_lit = mod.litOf
    if (isFolding && bits_lit != null){
      val w = hi - lo + 1
      Literal((bits_lit.value >> lo) & ((BigInt(1) << w) - BigInt(1)), w)
    } else {
      val res = new Extract();
      res.hi = Literal(hi);
      res.lo = Literal(lo);
      res.init("", fixWidth(hi-lo+1), mod, res.hi, res.lo);
      res
    }
  }
}

object Extract {
  //extract 1 bit
  def apply[T <: Bits](mod: T, bit: UFix)(gen: => T): T = {
    val (bits_lit, off_lit) = (mod.litOf, bit.litOf);
    if (isFolding && bits_lit != null && off_lit != null) {
      // println("FOLDING EXTRACT " + bits_lit.value + "(" + off_lit.value + ")");
      makeLit(Literal((bits_lit.value >> off_lit.value.toInt)&1, 1)){ gen };
    } else {
      // initialize 
      val extract = new Extract()
      extract.init("", fixWidth(1), mod.toNode, bit)
      extract.hi = bit
      extract.lo = bit
      extract.setTypeNodeNoAssign(gen.fromNode(extract))
    }
  }

  def apply[T <: Bits](mod: T, bit: Int)(gen: => T): T = 
     apply(mod, UFix(bit))(gen);

  // extract bit range
  def apply[T <: Bits](mod: T, hi: UFix, lo: UFix, w: Int = -1)(gen: => T): T = {
    val (bits_lit, hi_lit, lo_lit) = (mod.litOf, hi.litOf, lo.litOf);
    if (isFolding && bits_lit != null && hi_lit != null && lo_lit != null) {
      val dw = if (w == -1) (hi_lit.value - lo_lit.value + 1).toInt else w;
      makeLit(Literal((bits_lit.value >> lo_lit.value.toInt)&((BigInt(1)<<dw) - BigInt(1)), dw)){ gen };
    } else {
      // initialize
      val extract = new Extract()
      extract.init("", if(w == -1) widthOf(0) else fixWidth(w), mod.toNode, hi, lo)
      extract.hi = hi
      extract.lo = lo
      extract.setTypeNodeNoAssign(gen.fromNode(extract))
    }
  }

  def apply[T <: Bits](mod: T, hi: Int, lo: Int)(gen: => T): T ={
    apply(mod, UFix(hi), UFix(lo), hi-lo+1)(gen);
  }
}

class Extract extends Node {
  var lo: Node = null;
  var hi: Node = null;

  override def toString: String =
    if (hi == lo)
      "BITS(" + inputs(0) + ", " + lo + ")";
    else
      "BITS(" + inputs(0) + ", " + hi + ", " + lo + ")";
  override def emitDef: String = {
    inputs.tail.foreach(validateIndex)
    if (inputs.length < 3) {
      if(inputs(0).width > 1) {
        "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + inputs(1).emitRef + "];\n"
      } else {
        "  assign " + emitTmp + " = " + inputs(0).emitRef + ";\n"
      }
    } else {
      if(inputs(0).width > 1) {
        "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + inputs(1).emitRef + ":" + inputs(2).emitRef + "];\n"
      } else {
        "  assign " + emitTmp + " = " + inputs(0).emitRef + ";\n"
      }
    }
  }
  override def emitDefLoC: String = {
    inputs.tail.foreach(validateIndex)
    if (inputs.length < 3 )
      "  " + emitTmp + " = " + inputs(0).emitRef + ".bit(" + inputs(1).emitRef + ");\n"
    else{
      "  " + emitTmp + " = " + inputs(0).emitRef + ".extract<" + width + ">(" + inputs(1).emitRef + "," + inputs(2).emitRef + ");\n"}
  }
  def validateIndex(x: Node) = {
    val lit = x.litOf
    assert(lit == null || lit.value >= 0 && lit.value < inputs(0).getWidth)
  }
}

}
