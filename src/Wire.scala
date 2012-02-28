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
      assert(w > 0, {println("Negative width to wire " + i)})
      val wire = new Wire()
      wire.init("", w, null)

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
