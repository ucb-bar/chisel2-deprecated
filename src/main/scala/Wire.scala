// author: jonathan bachrach
package Chisel {

import Node._;
import ChiselError._;

object Wire {
  def apply[T <: Data]()(gen: => T): T = {
    val junctioncell = new WireCell[T](gen, -1)(gen);
    junctioncell.io.out
  }
  def apply[T <: Data](width: Int)(gen: =>T): T = {
    val junctioncell = new WireCell[T](gen, width)(gen);
    junctioncell.io.out
  }
  def apply[T <: Data](default: T): T = {
    val junctioncell = new WireCell[T](default, -1, true)(default.clone.asInstanceOf[T]);
    junctioncell.io.in assign default;
    junctioncell.io.out
  }
}


class WireCell[T <: Data](data: T, width: Int, hasDefault: Boolean = false)(gen: => T){
  val io = new Bundle(){
    val in = gen.asInput();
    val out = gen.asOutput();
  }
  io.setIsCellIO;
  val primitiveNode = new Wire();
  if(width > -1)
    primitiveNode.init("primitiveNode", width, null)
  else if(hasDefault) {
    primitiveNode.init("primitiveNode", widthOf(0), io.in)
  } else
    primitiveNode.init("primitiveNode", widthOf(0), null);
  val fb = io.out.fromNode(primitiveNode).asInstanceOf[T] 
  fb.setIsCellIO;
  fb ^^ io.out;
  io.out.comp = primitiveNode.asInstanceOf[Wire];
  primitiveNode.nameHolder = io.out;
}

class Wire extends Data with proc{
  // override def toString: String = "W(" + name + ")"
  var assigned = false;
  def default: Node = if (inputs.length < 1 || inputs(0) == null) null else inputs(0);
  override def toNode = this;
  override def fromNode(src: Node) = {
    val res = new Wire().asInstanceOf[this.type];
    res assign src;
    res
  }
  def procAssign(src: Node) = {
    if (assigned) {
      ChiselErrors += IllegalState("reassignment to Node", 3);
    } else {
      var res = Lit(true);
      for (i <- 0 until conds.length)
        res = conds(i) && res;
      updates.push((res, src));
    }
  }
  override def toString: String = name
  override def emitDef: String = { 
    if (inputs.length == 0) {
      println("// UNCONNECTED " + this + " IN " + component); ""
    } else if (inputs(0) == null) {
      println("// UNCONNECTED WIRE " + this + " IN " + component); ""
    } else
      "  assign " + emitTmp + " = " + inputs(0).emitRef + ";\n" }
  override def emitDefLoC: String = 
    // TODO: NEED THIS TO BE A CHECK
    if (inputs.length == 1)
      "  " + emitTmp + " = " + inputs(0).emitRef + ";\n"
    else
      ""
    // "  " + emitTmp + " = " + inputs(0).emitRef + ";\n"

  override def assign(src: Node) = {
    if(assigned || inputs(0) != null)
      ChiselErrors += IllegalState("reassignment to Wire", 3);
    else { assigned = true; super.assign(src)}
  }
}

}
