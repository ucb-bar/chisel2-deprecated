// author: jonathan bachrach
package Chisel {

import Node._;

object Probe {
  def apply(x: Node, name: String): Probe = { val res = new Probe(); res.init(name, widthOf(0), x); res }
  def apply(x: Node): Probe = apply(x, "");
}
class Probe extends Node {
  def my_name: String = if (name == "") inputs(0).emitRefV else name
  override def isProbe = true;
  override def toString: String =
    "Probe(" + inputs(0) + ", " + my_name + ")"
  override def emitDef: String = 
    "  assign " + emitTmp + " = " + inputs(0).emitRef + ";\n"
  override def emitDefLoC: String = 
    "  " + "printf(\"DBG " + my_name + ": %s\\n\", " + inputs(0).emitRef + ".to_str().c_str());\n" +
    "  " + emitTmp + " = " + inputs(0).emitRef + ";\n";
}

}
