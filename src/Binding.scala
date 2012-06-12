package Chisel
import Component._
import Node._

// used for component to component connections
object Binding {
  def apply(m: Node, c: Component, ioComp: Component): Node = {
    if (isEmittingComponents) {
      val res = c.findBinding(m);
      if (res == null) {
        val res = new Binding();
        res.component = c;
        res.init("", widthOf(0), m);
	res.infer;
	var genName = ioComp.genName(m.name);
	if(c.nameSpace.contains(genName)) genName += ("_" + res.emitIndex);
	res.name = genName;
	res.named = true;
        c.bindings += res;
        res
      } else {
        res;
      }
    } else
      m
  }
}
class Binding extends Node {
  override def emitDef: String = "";
  override def toString: String = "BINDING(" + inputs(0) + ")";
  override def emitDecC: String = 
    if (isEmittingComponents) "  Data " + emitRef + ";\n"; else "";
  override def emitDefLoC: String = ""
  override def emitDefHiC: String = ""
  override def emitRefC: String = 
    if (isEmittingComponents) emitRefV; else inputs(0).emitRefC;
}
