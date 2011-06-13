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

object Lookup {
  def apply (addr: Node, default: Node, mapping: Seq[(Lit, Node)]): Lookup = {
    val res = new Lookup();
    res.init("", widthOf(1), addr, default); 
    for ((addr, data) <- mapping)
      res.inputs += data;
    res.map = mapping;
    res
  }
}

class Lookup extends Delay {
  var map: Seq[(Lit, Node)] = null;
  override def toString: String = "LOOKUP(" + inputs(0) + ")";
  override def emitDef: String = {
    var res = 
      "  always @(*) begin\n" +
      "    " + emitRef + " = " + inputs(1).emitRef + ";\n" +
      "    casez (" + inputs(0).emitRef + ")" + "\n";
    
    for ((addr, data) <- map) 
      res = res +
        "      " + addr.emitRef + " : " + emitRef + " = " + data.emitRef + ";\n";
    res = res + 
      "    endcase\n" +
      "  end\n";
    res
  }
  override def emitDefLoC: String = {
    var res = "";
    for ((addr, data) <- map) 
      res = res +
        "  if ((" + addr.emitRef + " == " + inputs(0).emitRef + ").to_bool()) " + emitRef + " = " + data.emitRef + ";\n";
    res
  }
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";
}

}
