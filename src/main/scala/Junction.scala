// author: jonathan bachrach
package Chisel {

import Node._;

object PJunction {

  def apply[T <: dat_t : Manifest](): T = {
    val junctioncell = new JunctionCell[T](Fab[T](), -1);
    junctioncell.io.out
  }
  def apply[T <: dat_t : Manifest](width: Int): T = {
    val junctioncell = new JunctionCell[T](Fab[T](), width);
    junctioncell.io.out
  }
}

object Junction {
  def apply(): int_t = {
    val junctioncell = new JunctionCell[int_t](Fab[int_t](), -1);
    junctioncell.io.out
  }
  def apply(width: Int): int_t = {
    val junctioncell = new JunctionCell[int_t](Fab[int_t](), width);
    junctioncell.io.out
  }
}


class JunctionCell[T <: dat_t](data: T, width: Int){
  val io = new bundle_t(){val in = data.clone.asInput();
			  val out = data.clone.asOutput();
		      }
  io.setIsCellIO;
  val primitiveNode = new Junction();
  if(width > -1)
    primitiveNode.init("primitiveNode", width, null)
  else
    primitiveNode.init("primitiveNode", widthOf(0), null);
  val fb = data.fromBits(primitiveNode).asInstanceOf[T] 
  fb.setIsCellIO;
  fb ^^ io.out;
  io.out.comp = primitiveNode.asInstanceOf[Junction];
  primitiveNode.nameHolder = io.out;
}

class Junction extends dat_t with proc{
  // override def toString: String = "W(" + name + ")"
  override def toBits = this;
  override def fromBits(src: Node) = {
    val res = new Junction().asInstanceOf[this.type];
    res := src;
    res
  }
  def <==(src: Node): this.type = {
    if (cond.length == 0){
      inputs(0) = src;
    }
    else {
      var res = cond(0);
      for (i <- 1 until cond.length)
        res = cond(i) && res;
      if(inputs(0) != null)
	inputs(0) = Multiplex(res, src, inputs(0))
      else 
	inputs(0) = Multiplex(res, src, this);
    }
    this
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
}

}
