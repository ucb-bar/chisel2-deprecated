// author: jonathan bachrach
package Chisel {

import Node._;
import Literal._;
import scala.collection.mutable.ArrayBuffer;

object Lookup {
  def apply[T <: Data](addr: Fix, default: T, mapping: Seq[(Fix, T)]): T = {
    val lookupCell = new LookupCell(default, mapping);
    lookupCell.io.default <> default;
    lookupCell.io.addr := addr;
    lookupCell.io.out
  }

}


class LookupCell[T <: Data](data: T, mapping: Seq[(Fix, T)]){
  val io = new Bundle(){
    val default = data.clone.asInput;
    val addr = Input();
    val out = data.clone.asOutput;
  }
  io.setIsCellIO;
  val primitiveNode = new Lookup()
  primitiveNode.init("primitiveNode", widthOf(1), io.addr, io.default.toNode);
  for((addr, data) <- mapping){
    data.setIsCellIO;
    primitiveNode.inputs += data.toNode;
  }
  primitiveNode.asInstanceOf[Lookup].map = mapping.map{case(addr, data) => (addr, data.toNode)}.toArray;
  val fb = io.out.fromNode(primitiveNode).asInstanceOf[T] 
  fb.setIsCellIO;
  fb ^^ io.out;
}


class Lookup extends Node {
  override def isInObject = true;
  var map: Array[(Node, Node)] = null;

  override def removeCellIOs() = {
    super.removeCellIOs();
    for(((i, n), m) <- map zip map.indices)
      map(m) = ((i.getNode, n.getNode));
    this
  }

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
