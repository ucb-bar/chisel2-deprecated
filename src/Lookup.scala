// author: jonathan bachrach
package Chisel {

import Node._;
import Literal._;
import scala.collection.mutable.ArrayBuffer;

object Lookup {
  def apply[T <: Data](addr: Bits, default: T, mapping: Seq[(Bits, T)]): T = {
    val lookup = new Lookup()
    val mappingNode = mapping.map(x => LookupMap(x))
    lookup.initOf("", widthOf(1), List(addr, default) ++ mappingNode)
    lookup.setTypeNodeNoAssign(default.fromNode(lookup).asInstanceOf[T])
  }
}

object LookupMap {
  def apply[T <: Data](map: (Bits, T)): LookupMap = {
    val res = new LookupMap()
    res.init("", widthOf(0), map._1, map._2)
    res
  }
}

class LookupMap extends Node {
  override def emitDefLoC: String = ""
  override def emitDef: String = ""
  override def emitDec: String = ""
  override def emitDecC: String = ""

  def addr = inputs(0)
  def data = inputs(1)
}

class Lookup extends Node {
  override def isInObject = true;

  def map = inputs.slice(2, inputs.length).map(x => x.asInstanceOf[LookupMap])

  override def toString: String = "LOOKUP(" + inputs(0) + ")";
  override def emitDef: String = {
    var res = 
      "  always @(*) begin\n" +
      "    " + emitRef + " = " + inputs(1).emitRef + ";\n" +
      "    casez (" + inputs(0).emitRef + ")" + "\n";
    
    for (node <- map) 
      res = res +
        "      " + node.addr.emitRef + " : " + emitRef + " = " + node.data.emitRef + ";\n";
    res = res + 
      "    endcase\n" +
      "  end\n";
    res
  }
  override def emitDefLoC: String = {
    var res = "";
    for (node <- map) 
      res = res +
        "  if ((" + node.addr.emitRef + " == " + inputs(0).emitRef + ").to_bool()) " + emitRef + " = " + node.data.emitRef + ";\n";
    res
  }
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";
}

}
