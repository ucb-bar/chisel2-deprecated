package Chisel {
import Component._;
import IOdir._;
import Node._;

object Cat {
  def apply[T <: Data](mod: T, mods: T*): Fix = {
    val res = new CatCell(mod, mods.toList);
    res.io.out
  }
}


class CatCell[T <: Data](mod: T, mods: List[T]){
  val io = new Bundle(){val out = Fix('output);
			}
  io.setIsCellIO;
  val primitiveNode = 
    if(isEmittingComponents){
      val res = new Cat();
      res.initOf("primitiveNode", sumWidth _, mod.toNode :: mods.map(x => x.toNode));
      res
    } else
      mods.foldLeft(mod.toNode){(a, b) => a ## b.toNode}
  io.out := primitiveNode;
  primitiveNode.nameHolder = io.out
}


object Concatanate {
  def apply (mod: Node, mods: Node*): Node = 
    if(isEmittingComponents) {
      val res = new Cat();
      res.initOf("", sumWidth _, mod :: mods.toList);
      res
    } else
      mods.foldLeft(mod){(a, b) => a ## b};
}
class Cat extends Node {
  override def emitDef: String = {
    var res = "  assign " + emitTmp + " = {";
    var first = true;
    for(node <- inputs)
      res += (if(first) {first = false; ""} else ", ") + node.emitRef;
    res += "};\n";
    res
  }
}

}
