// author: jonathan bachrach
package Chisel {

import Node._;
import Component._;
import IOdir._;

object NodeExtract {
  
  // extract one bit
  def apply (mod: Node, bit: Node): Node = {
    val res = new Extract();
    res.init("", fixWidth(1), mod, bit);
    res.hi = bit; 
    res.lo = bit;
    res
  }

  def apply (mod: Node, bit: Int): Node = 
    apply(mod, UFix(bit));

  // extract bit range
  def apply (mod: Node, hi: Node, lo: Node): Node = {
    val res = new Extract();
    res.init("", widthOf(0), mod, hi, lo);
    res.hi = hi;
    res.lo = lo;
    res
  }

  def apply (mod: Node, hi: Int, lo: Int): Node = {
    val res = new Extract();
    if(mod.name == "foo") println(hi + " " + lo);
    res.hi = Fix(hi);
    res.lo = Fix(lo);
    res.init("", fixWidth(hi-lo+1), mod, res.hi, res.lo);
    res
  }
}

object Extract {
  //extract 1 bit
  def apply[T <: Bits](mod: T, bit: UFix)(gen: => T): T = {
    val bitcell = new BitExtractCell(gen);
    bitcell.io.in <> mod;
    bitcell.io.hi <> bit;
    bitcell.io.out;
  }

  def apply[T <: Bits](mod: T, bit: Int)(gen: => T): T = 
     apply(mod, UFix(bit))(gen);

  // extract bit range
  def apply[T <: Bits](mod: T, hi: UFix, lo: UFix, w: Int = -1)(gen: => T): T = {
    val bitcell = new RangeExtractCell(w)(gen);
    bitcell.io.in <> mod;
    bitcell.io.hi assign hi;
    bitcell.io.lo assign lo;
    bitcell.io.out
  }

  def apply[T <: Bits](mod: T, hi: Int, lo: Int)(gen: => T): T ={
    apply(mod, UFix(hi), UFix(lo), hi-lo+1)(gen);
  }
}

abstract class ExtractCell[T <: Bits](gen: => T){
  val io = new Bundle(){
    val in = gen.asInput;
    val hi = UFix('input);
    val lo = UFix('input);
    val out = gen.asOutput;
  }
  io.setIsCellIO;
  val primitiveNode = new Extract();
  val fb = io.out.fromNode(primitiveNode)
  fb.setIsCellIO;
  fb ^^ io.out;
  io.out.comp = primitiveNode;
  primitiveNode.nameHolder = io.out;

  def := (src: Bits) = {
    println("ColonEqual in Extract");
  }
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

class Extract extends Node with proc {
  var lo: Node = null;
  var hi: Node = null;

  // Define proc trait methods.
  override def genMuxes(default: Node) = {
  }

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

  def procAssign(src: Node) = {
    assign(src);
  }
  override def assign(src: Node): Unit = {
    // If assigning to an extract output, search forward to an Assign node.
    val assign_node = findAssignNode(8);
    if (assign_node == null) {
      println("[error] Unable to determine assignment destination from extract.");
      return Unit;
    }
    // println("[info] Found an Assign node from an Extract");
    assign_node match {
      case a: Assign[_] => {
        a.assign_from_extract(this, src.asInstanceOf[Data].toBits);
      }
      case any => {
        println("[error] Assignment to Extract: Unable to find associated Assign block.");
      }
    }
  }
}

}
