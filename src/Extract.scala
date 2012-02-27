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
    // initialize 
    val extract = new Extract()
    extract.init("", fixWidth(1), mod.toNode, bit)
    extract.hi = bit
    extract.lo = bit

    // make output
    val output = gen.fromNode(extract)
    output.setIsCellIO
    extract.nameHolder = output
    output.comp = extract
    output
  }

  def apply[T <: Bits](mod: T, bit: Int)(gen: => T): T = 
     apply(mod, UFix(bit))(gen);

  // extract bit range
  def apply[T <: Bits](mod: T, hi: UFix, lo: UFix, w: Int = -1)(gen: => T): T = {
    // initialize
    val extract = new Extract()
    extract.init("", if(w == -1) widthOf(0) else fixWidth(w), mod.toNode, hi, lo)
    extract.hi = hi
    extract.lo = lo

    // make output
    val output = gen.fromNode(extract)
    output.setIsCellIO
    extract.nameHolder = output
    output.comp = extract
    output
  }

  def apply[T <: Bits](mod: T, hi: Int, lo: Int)(gen: => T): T ={
    apply(mod, UFix(hi), UFix(lo), hi-lo+1)(gen);
  }
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
      // stack = Thread.currentThread.getStackTrace;
      // for (e <- stack)
      //   println(e);
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
