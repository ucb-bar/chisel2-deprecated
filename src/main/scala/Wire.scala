// author: jonathan bachrach
package Chisel {

import Node._;
import ChiselError._;

object PWire {

  def apply[T <: dat_t : Manifest](): T = {
    val junctioncell = new WireCell[T](Fab[T](), -1);
    junctioncell.io.out
  }
  def apply[T <: dat_t : Manifest](width: Int): T = {
    val junctioncell = new WireCell[T](Fab[T](), width);
    junctioncell.io.out
  }
}

object Wire {
  def apply[T <: dat_t: Manifest](): T = {
    val junctioncell = new WireCell[T](Fab[T](), -1);
    junctioncell.io.out
  }
  def apply[T <: dat_t: Manifest](width: Int): T = {
    val junctioncell = new WireCell[T](Fab[T](), width);
    junctioncell.io.out
  }
  def apply[T <: dat_t](default: T): T = {
    val junctioncell = new WireCell[T](default, -1, true);
    junctioncell.io.in := default;
    junctioncell.io.out
  }
}


class WireCell[T <: dat_t](data: T, width: Int, hasDefault: Boolean = false){
  val io = new bundle_t(){val in = data.clone.asInput();
			  val out = data.clone.asOutput();
		      }
  io.setIsCellIO;
  val primitiveNode = new Wire();
  if(width > -1)
    primitiveNode.init("primitiveNode", width, null)
  else if(hasDefault)
    primitiveNode.init("primitiveNode", widthOf(0), io.in)
  else
    primitiveNode.init("primitiveNode", widthOf(0), null);
  val fb = data.fromNode(primitiveNode).asInstanceOf[T] 
  fb.setIsCellIO;
  fb ^^ io.out;
  io.out.comp = primitiveNode.asInstanceOf[Wire];
  primitiveNode.nameHolder = io.out;
}

class Wire extends dat_t with proc{
  // override def toString: String = "W(" + name + ")"
  var assigned = false;
  override def toNode = this;
  override def fromNode(src: Node) = {
    val res = new Wire().asInstanceOf[this.type];
    res := src;
    res
  }
  def <==(src: Node) = {
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

  override def :=(src: Node) = {
    if(assigned || inputs(0) != null)
      ChiselErrors += IllegalState("reassignment to Node", 3);
    else { assigned = true; super.:=(src)}
  }
}

}
