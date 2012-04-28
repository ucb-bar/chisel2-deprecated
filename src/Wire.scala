// author: jonathan bachrach
package Chisel {

import Node._;
import ChiselError._;

/*
object Wire {
  def apply[T <: Data](width: Int = -1)(gen: =>T): T = {
    val wire = new Wire()

    // initialize
    val genWidth = gen.getWidth
    if(width > -1)
      wire.init("", width, null)
    else if(genWidth > -1)
      wire.init("", genWidth, null)
    else
      wire.init("", widthOf(0), null)

    // make output
    val output = gen.fromNode(wire)
    output.setIsCellIO
    output.comp = wire
    wire.nameHolder = output
    output
  }

  def apply[T <: Data](default: T): T = {
    if(default.inputs.length == 0)
      ChiselErrors += ChiselError("incorrect wire syntax", Thread.currentThread().getStackTrace);
    val wire = new Wire()

    //initialize
    wire.init("", widthOf(0), default)

    // make output
    val output = default.fromNode(wire)
    output.setIsCellIO
    output.comp = wire
    wire.nameHolder = output
    output
  }
}
* */

object Wire {
  def apply[T <: Data](width: Int = -1)(gen: =>T): T = {
    val res = gen.asOutput
    res.setIsCellIO

    for((n, i) <- res.flatten) {
      // initialize wire
      val w = i.getWidth
      val wFun = if(w < 0) widthOf(0) else fixWidth(w)
      val wire = new Wire()
      wire.init("", wFun, null)

      // make output
      i.inputs += wire
      i.comp = wire
    }

    res
  }

  def apply[T <: Data](default: T): T = {
    if(default.inputs.length == 0)
      ChiselErrors += ChiselError("incorrect wire syntax", Thread.currentThread().getStackTrace);
    val wire = new Wire()

    //initialize
    wire.init("", widthOf(0), default)

    // make output
    val output = default.fromNode(wire)
    output.setIsCellIO
    output.comp = wire
    wire.nameHolder = output
    output
  }
}

class Wire extends Data with proc {
  // override def toString: String = "W(" + name + ")"
  var assigned = false;
  def default: Node = if (inputs.length < 1 || inputs(0) == null) null else inputs(0);
  override def toNode = this;
  override def fromNode(src: Node) = {
    val res = new Wire().asInstanceOf[this.type];
    res assign src;
    res
  }
  override def toString: String = name
  override def dotName = { 
    // val names = name.split("__"); 
    // val dname = (if (names.size > 1) names(1) else names(0));
    super.dotName + "(" + name + ")";
  }
  override def emitRefC: String = if (!isInObject && inputs.length == 1) inputs(0).emitRefC else super.emitRefC
  override def emitDef: String = { 
    if (inputs.length == 0) {
      println("// UNCONNECTED " + this + " IN " + component); ""
    } else if (inputs(0) == null) {
      println("// UNCONNECTED WIRE " + this + " IN " + component); ""
    } else
      "  assign " + emitTmp + " = " + inputs(0).emitRef + ";\n" }
  override def emitDefLoC: String = 
    // TODO: NEED THIS TO BE A CHECK
    if (isInObject && inputs.length == 1)
      "  " + emitTmp + " = " + inputs(0).emitRef + ";\n"
    else if (inputs.length == 0 && !isInObject) {
      "  " + emitTmp + ";\n"
    } else
      ""
    // "  " + emitTmp + " = " + inputs(0).emitRef + ";\n"

  def procAssign(src: Node) = {
    if (assigned) {
      ChiselErrors += ChiselError("reassignment to Node", Thread.currentThread().getStackTrace);
    } else {
      updates.enqueue((genCond(), src));
    }
  }
  override def assign(src: Node) = {
    if(assigned || inputs(0) != null) {
      ChiselErrors += ChiselError("reassignment to Wire", Thread.currentThread().getStackTrace);
    } else { 
      assigned = true; super.assign(src)
    }
  }
}

}
