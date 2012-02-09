// author: jonathan bachrach
package Chisel {

import Component._;
import Node._;

// used for component to component connections
object Binding {
  def apply(m: Node, c: Component, ioComp: Component): Node = {
    // println("BINDING " + m);
    if (isEmittingComponents) {
      val res = c.findBinding(m);
      if (res == null) {
        val res = new Binding();
        res.component = c;
        res.init("", widthOf(0), m);
	var genName = ioComp.genName(m.name);
	if(c.nameSpace.contains(genName)) genName += ("_" + res.emitIndex);
        //res.name = ioComp.genName(m.name); 
	// TODO: NAME
	res.name = genName;
	res.named = true;
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
    if (isEmittingComponents) "  Data " + emitRef + ";\n"; else "";
  override def emitDefLoC: String = ""
  override def emitDefHiC: String = ""
  override def emitRefC: String = 
    if (isEmittingComponents) emitRefV; else inputs(0).emitRefC;
}
}
