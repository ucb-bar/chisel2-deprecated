// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer

import Node._;
import Component._;
import IOdir._;

object cppComp {
  def apply(fctn: String, outWidths: List[Int], input: Node*): cppComp = {
    val res = new cppComp(fctn, outWidths);
    res.initOf("", fixWidth(1), input.toList);
    for((w, node) <- outWidths zip res.outputNodes)
      node.init("", fixWidth(w), res);
    res
  }
  def apply(fctnName: String, fctnIO: Bundle): cppComp = {
    val ioList = fctnIO.flatten;
    val input = new ArrayBuffer[Node]();
    val output = new ArrayBuffer[Node]();
    for ((s, node) <- ioList)
      if (node.dir == INPUT) input += node else output += node
    val res = new cppComp(fctnName, output.map(a => a.width).toList);
    res.initOf("", fixWidth(1), input.toList);
    for((ioNode, node) <- output zip res.outputNodes){
      node.init("", ioNode.width, res); ioNode assign node;
    }
    res
  }
}

class cppComp(fctnName: String, outWidths: List[Int]) extends Node {
  var outputNodes: List[Node] = (outWidths.indices zip outWidths).map{case (i, w) => new cppCompOutput(this, w, i)}.toList;
  override def toString: String = inputs.toString;
  override def isInObject = true;
  override def emitDefLoC: String = {
    var res = "";
    res = res + "  " + emitRef + " = " + fctnName + "(";
    var first = true;
    for(node <- inputs) 
      res = res + (if(first) {first = false; ""} else ", ") + node.emitRef + ".lo_word()";
    res = res + ");\n";
    res
  }
  override def emitDecC: String = "  int* " + emitRef + ";\n";
  def apply(): List[Node] = outputNodes;
}

class cppCompOutput(fctn: cppComp, width: Int, i: Int) extends Node {
  override def isInObject = true;
  override def emitDefLoC: String = 
    "  " + emitRef + " = DAT<" + width + ">(" + fctn.emitRef + "[" + i + "]);\n";
  
}

}
