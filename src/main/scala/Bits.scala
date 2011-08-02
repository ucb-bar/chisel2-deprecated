// author: jonathan bachrach
package Chisel {

import Node._;
import Component._;
import IOdir._;

object Bits {
  // extract one bit
  def apply (mod: Node, bit: Node): Node = {
    val res = new Bits();
    res.init("", fixWidth(1), mod, bit);
    res.hi = bit; 
    res.lo = bit;
    res
  }
  def apply(mod: int_t, bit: int_t): int_t = {
    val bitcell = new BitExtractCell(mod);
    bitcell.io.in <> mod;
    bitcell.io.hi <> bit;
    bitcell.io.out;
  }
  def apply (mod: Node, bit: Int): Node = 
    apply(mod, Lit(bit));
  def apply(mod: int_t, bit: Int): int_t = 
    apply(mod, Lit(bit));
  // extract bit range
  def apply (mod: Node, hi: Node, lo: Node): Node = {
    val res = new Bits();
    res.init("", widthOf(0), mod, hi, lo);
    res.hi = hi;
    res.lo = lo;
    res
  }
  def apply(mod: int_t, hi: int_t, lo: int_t, w: Int = -1): int_t = {
    val bitcell = new RangeExtractCell(mod, w);
    bitcell.io.in <> mod;
    bitcell.io.hi := hi;
    bitcell.io.lo := lo;
    bitcell.io.out
  }
  def apply (mod: Node, hi: Int, lo: Int): Node = {
    val res = new Bits();
    if(mod.name == "foo") println(hi + " " + lo);
    res.hi = Lit(hi);
    res.lo = Lit(lo);
    res.init("", fixWidth(hi-lo+1), mod, res.hi, res.lo);
    res
  }
  def apply(mod: int_t, hi: Int, lo: Int): int_t ={
    apply(mod, Lit(hi), Lit(lo), hi-lo+1);
  }
}

abstract class ExtractCell(data: int_t){
  val io = new bundle_t(){val in = data.clone.asInput;
			val hi = int_t(INPUT);
			val lo = int_t(INPUT);
			val out = data.clone.asOutput;
		      }
  io.setIsCellIO;
  val primitiveNode = new Bits();
  val fb = io.out.fromBits(primitiveNode)
  fb.setIsCellIO;
  fb ^^ io.out;
  primitiveNode.nameHolder = io.out;
}

class BitExtractCell(data: int_t) extends ExtractCell(data) {
  primitiveNode.init("primitiveNode", fixWidth(1), io.in.toBits, io.hi.toBits);
  primitiveNode.hi = primitiveNode.inputs(1)
  primitiveNode.lo = primitiveNode.hi;
}

class RangeExtractCell(data: int_t, w: Int = -1) extends ExtractCell(data) {
  primitiveNode.init("primitiveNode", if(w == -1) widthOf(0) else fixWidth(w), io.in.toBits, io.hi, io.lo);
  primitiveNode.hi = primitiveNode.inputs(1);
  primitiveNode.lo = primitiveNode.inputs(2);
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
    if (inputs.length < 3)
      "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + inputs(1).emitRef + "];\n"
    else
      "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + inputs(1).emitRef + ":" + inputs(2).emitRef + "];\n"
  override def emitDefLoC: String = 
    if (inputs.length < 3 )
      "  " + emitTmp + " = " + inputs(0).emitRef + ".bit(" + inputs(1).emitRef + ");\n"
    else{
      "  " + emitTmp + " = " + inputs(0).emitRef + ".extract<" + width + ">(" + inputs(1).emitRef + "," + inputs(2).emitRef + ");\n"}
}

}
