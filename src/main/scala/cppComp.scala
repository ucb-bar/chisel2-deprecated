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

object cppComp {
  def apply(fctn: String, outWidths: List[Int], input: Node*): cppComp = {
    val res = new cppComp(fctn, outWidths);
    res.initOf("", fixWidth(1), input.toList);
    for((w, node) <- outWidths zip res.outputNodes)
      node.init("", fixWidth(w), res);
    res
  }
  def apply(fctn: String, fctnIO: Bundle): cppComp = {
    val ioList = fctnIO.flatten;
    val input = new ArrayBuffer[Node]();
    val output = new ArrayBuffer[Node]();
    for ((s, node) <- ioList)
      if (node.dir == INPUT) input += node else output += node
    val res = new cppComp(fctn, output.map(a => a.width).toList);
    res.initOf("", fixWidth(1), input.toList);
    for((ioNode, node) <- output zip res.outputNodes){
      node.init("", ioNode.width, res); ioNode := node;
    }
    res
  }
}

class cppComp(fctn: String, outWidths: List[Int]) extends Node {
  val fctnName = fctn;
  var outputNodes: List[Node] = (outWidths.indices zip outWidths).map{case (i, w) => new cppCompOutput(this, w, i)}.toList;
  override def toString: String = inputs.toString;
  override def isInObject = true;
  override def emitDefLoC: String = {
    var res = "";
    res = res + "  " + emitRef + " = " + fctn + "(";
    var first = true;
    for(node <- inputs) 
      res = res + (if(first) {first = false; ""} else ", ") + node.emitRef + ".lo_word()";
    res = res + ");\n";
    res
  }
  override def emitDecC: String = 
    "  int* " + emitRef + ";\n";
  

  def apply(): List[Node] = outputNodes;
}

class cppCompOutput(fctn: cppComp, width: Int, i: Int) extends Node {
  override def isInObject = true;
  override def emitDefLoC: String = 
    "  " + emitRef + " = DAT<" + width + ">(" + fctn.emitRef + "[" + i + "]);\n";
  
}

}
