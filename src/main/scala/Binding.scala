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

// used for component to component connections
object Binding {
  def apply(m: Node, c: Component): Node = {
    // println("BINDING " + m);
    if (isEmittingComponents) {
      val res = c.findBinding(m);
      if (res == null) {
        val res = new Binding();
        res.component = c;
        res.init("", widthOf(0), m);
        res.name = c.genName(m.name); // TODO: NAME
        // println("ADDING NEW BINDING " + m);
        // println("ADDING BINDING " + res + " TO " + res.component.name);
        // res.component.bindings += res;
        c.bindings += res;
        res
      } else {
        // println("FOUND BINDING " + res);
        res;
      }
    } else
      m
  }
}
class Binding extends Node {
  // override def emitDec: String = "";
  override def emitDef: String = "";
  override def toString: String = "BINDING(" + inputs(0) + ")";
  override def emitDecC: String = 
    if (isEmittingComponents) "  dat_t " + emitRef + ";\n"; else "";
  override def emitDefLoC: String = ""
  override def emitDefHiC: String = ""
  override def emitRefC: String = 
    if (isEmittingComponents) emitRefV; else inputs(0).emitRefC;
}
}
