package Chisel
import Component._
import Node._

object Cat {
  def apply[T <: Data](mod: T, mods: T*): Bits = {
    val modsList = (mod :: mods.toList).filter(_ != null).map(_.toNode)
    val isLit = isFolding && modsList.forall(_.litOf != null)
    val res = if (!isLit && backend.isInstanceOf[VerilogBackend]) {
      val res = new Cat();
      res.initOf("", sumWidth _, modsList)
    } else
      modsList.reduceLeft((a, b) => a ## b)
    res.setTypeNode(Bits(OUTPUT))
  }
}

class Cat extends Node {
}

object Concatenate {
  def apply (mod: Node, mods: Node*): Node = 
    if(backend.isInstanceOf[VerilogBackend]) {
      val res = new Cat();
      res.initOf("", sumWidth _, mod :: mods.toList);
      res
    } else
      mods.foldLeft(mod){(a, b) => a ## b};
}

