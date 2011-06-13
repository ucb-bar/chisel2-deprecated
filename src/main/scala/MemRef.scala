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

object MemRef {
  def apply (mem: Node, addr: Node): Node = {
    val res = new MemRef();
    res.init("", widthOf(0), mem, addr);
    res
  }
}
class MemRef extends Node {
  override def toString: String = inputs(0) + "[" + inputs(1) + "]";
  override def emitDef: String = 
    "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + inputs(1).emitRef + "];\n"
  override def emitDefLoC: String = 
    "  " + emitTmp + " = " + inputs(0).emitRef + ".get(" + inputs(1).emitRef + ");\n"
}

}
