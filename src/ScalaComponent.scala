// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import Node._;

object ScalaComponent {

  def apply(file: String, outputs: Int, inputs: Node*): List[Node] = {
    val sc = new ScalaComponent(file, outputs);
    sc.init("", fixWidth(64), ScalaComponentInput(inputs.toList));
    for(sco <- sc.outputNodes)
      sco.init("", fixWidth(64), sc);
    sc.outputNodes
  }
}

class ScalaComponent(file: String, numOutputs: Int) extends Node {
  var outputNodes: List[Node] = Range(0, numOutputs, 1).map(a => new ScalaComponentOutput(this)).toList;
  override def toString: String = inputs.toString;
  override def emitDefLoC: String = {
    var res = "";
    res = res + "  callout_t<" + outputNodes.length + "> " + emitRef + " = " + "callout_t<" + outputNodes.length +">(\"" + file + "\", "; 
    res = res + inputs(0).emitRef;
    res = res + ");\n";
    for((i, node) <- outputNodes.indices zip outputNodes) {
      res = res + "  " + node.emitRef + " = " + emitRef + ".get(" + i + ");\n";
    }
    res = res + "  " + inputs(0).emitRef + ".clear();\n";
    res
  }

}

object ScalaComponentInput {
  def apply(inputs: List[Node]): ScalaComponentInput = {
    val res = new ScalaComponentInput();
    res.initOf("", fixWidth(64), inputs);
    res
  }
}

class ScalaComponentInput() extends Node {
  override def isInObject = true;
  override def emitDecC: String = {
    var res = "";
    res = res + "  std::vector<dat_t<64>*> " + emitRef + ";\n";
    res
  }
  override def emitDefLoC: String = {
    var res = "";
    for(node <- inputs){
      res = res + "  " + emitRef + ".push_back(&" + node.emitRef + ");\n";
    }
    res
  }
}

class ScalaComponentOutput(sc: ScalaComponent) extends Node {
  var scalacomp = sc;
  override def isInObject = true; 
  inferWidth = (m: Node) => 64;
}

}
