package Chisel {
import Component._;
import Node._;

object Cat {
  def apply[T <: Data](mod: T, mods: T*): Bits = {
    val modsList = mods.toList;
    if(modsList.length == 1 && modsList(0) == null){
      val res = Bits(OUTPUT);
      res.setIsCellIO;
      res assign mod.toNode;
      res
    } else {
      val isLit = mods.foldLeft(mod.litOf != null){(a,b) => a && (b.litOf != null)}
      if (isFolding && isLit) {
        var res = mod.litOf.value;
        var tw  = mod.litOf.getWidth();
        for (n <- mods) {
          val w = n.litOf.getWidth();
          res   = (res << w)|n.litOf.value;
          tw   += w;
        }
        Lit(res, tw){ Bits() };
      } else {
        // initialize
        val res = 
          if(isEmittingComponents){
            val res = new Cat();
            res.initOf("", sumWidth _, mod.toNode :: mods.toList.map(x => x.toNode))
          } else {
            mods.foldLeft(mod.toNode){(a,b) => a ## b.toNode}
          }

        // make output
        val output = Bits(OUTPUT)
        output.setIsCellIO
        output assign res
        res.nameHolder = output
        output
      }
    }
  }
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
