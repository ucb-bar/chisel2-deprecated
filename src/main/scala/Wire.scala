// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import java.lang.reflect.Modifier._;
import java.io.File;

import scala.math.log;
import scala.math.abs;
import scala.math.ceil;
import scala.math.max;
import scala.math.min;
import Node._;
import Wire._;
import Lit._;
import Op._;
import Reg._;
import Component._;
import Bundle._;
import IOdir._;

object Wire {
  def apply(): Wire = apply("")
  def apply(name: String, x: Node): Wire = {
    val res = new Wire();
    res.init(name, widthOf(0), x);
    res
  }
  def apply(x: Node): Wire = 
    apply("", x);
  def apply(name: String): Wire = 
    apply(name, null);
  def apply(name: String, width: Int): Wire = {
    val res = new Wire();
    res.init(name, width, null);
    res
  }
  def apply(width: Int): Wire = apply("", width);
}
class Wire extends Interface {
  // override def toString: String = "W(" + name + ")"
  def <==(src: Node): Wire = {
    if (cond.length == 0)
      inputs(0) = src;
    else {
      var res = cond(0);
      for (i <- 1 until cond.length)
        res = cond(i) && res;
      inputs(0) = Mux(res, src, inputs(0))
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
