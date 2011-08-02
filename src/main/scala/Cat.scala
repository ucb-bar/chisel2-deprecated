package Chisel {
import Component._;
import IOdir._;
import Node._;

object Cat {
  def apply[T <: dat_t](mod: T, mods: T*): int_t = {
    val res = new CatCell(mod, mods.toList);
    res.io.out
  }
}


class CatCell[T <: dat_t](mod: T, mods: List[T]){
  val io = new bundle_t(){val out = int_t(OUTPUT);
			}
  io.setIsCellIO;
  val primitiveNode = 
    if(isEmittingComponents){
      val res = new Cat();
      res.initOf("primitiveNode", sumWidth _, mod.toBits :: mods.map(x => x.toBits));
      res
    } else
      mods.foldLeft(mod.toBits){(a, b) => a ## b.toBits}
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
