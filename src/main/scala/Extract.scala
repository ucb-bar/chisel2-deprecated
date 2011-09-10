// author: jonathan bachrach
package Chisel {

import Node._;
import Component._;
import IOdir._;

object Extract {
  // extract one bit
  def apply (mod: Node, bit: Node): Node = {
    val res = new Extract();
    res.init("", fixWidth(1), mod, bit);
    res.hi = bit; 
    res.lo = bit;
    res
  }
  def apply[T <: Bits](mod: T, bit: Fix)(gen: => T): T = {
    val bitcell = new BitExtractCell(gen);
    bitcell.io.in <> mod;
    bitcell.io.hi <> bit;
    bitcell.io.out;
  }
  def apply (mod: Node, bit: Int): Node = 
    apply(mod, Fix(bit));
  def apply[T <: Bits](mod: T, bit: Int)(gen: => T): T = 
    apply(mod, Fix(bit))(gen);
  // extract bit range
  def apply (mod: Node, hi: Node, lo: Node): Node = {
    val res = new Extract();
    res.init("", widthOf(0), mod, hi, lo);
    res.hi = hi;
    res.lo = lo;
    res
  }
  def apply[T <: Bits](mod: T, hi: Fix, lo: Fix, w: Int = -1)(gen: => T): T = {
    val bitcell = new RangeExtractCell(w)(gen);
    bitcell.io.in <> mod;
    bitcell.io.hi := hi;
    bitcell.io.lo := lo;
    bitcell.io.out
  }
  def apply (mod: Node, hi: Int, lo: Int): Node = {
    val res = new Extract();
    if(mod.name == "foo") println(hi + " " + lo);
    res.hi = Fix(hi);
    res.lo = Fix(lo);
    res.init("", fixWidth(hi-lo+1), mod, res.hi, res.lo);
    res
  }
  def apply[T <: Bits](mod: T, hi: Int, lo: Int)(gen: => T): T ={
    apply(mod, Fix(hi), Fix(lo), hi-lo+1)(gen);
  }
}

abstract class ExtractCell[T <: Bits](gen: => T){
  val io = new Bundle(){
    val in = gen.asInput;
    val hi = Fix('input);
    val lo = Fix('input);
    val out = gen.asOutput;
  }
  io.setIsCellIO;
  val primitiveNode = new Extract();
  val fb = io.out.fromNode(primitiveNode)
  fb.setIsCellIO;
  fb ^^ io.out;
  primitiveNode.nameHolder = io.out;
}

class BitExtractCell[T <: Bits](gen: => T) extends ExtractCell[T](gen) {
  primitiveNode.init("primitiveNode", fixWidth(1), io.in.toNode, io.hi.toNode);
  primitiveNode.hi = primitiveNode.inputs(1)
  primitiveNode.lo = primitiveNode.hi;
}

class RangeExtractCell[T <: Bits](w: Int = -1)(gen: => T) extends ExtractCell[T](gen) {
  primitiveNode.init("primitiveNode", if(w == -1) widthOf(0) else fixWidth(w), io.in.toNode, io.hi, io.lo);
  primitiveNode.hi = primitiveNode.inputs(1);
  primitiveNode.lo = primitiveNode.inputs(2);
}

class Extract extends Node {
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
