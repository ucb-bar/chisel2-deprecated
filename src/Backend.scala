package Chisel
import Node._
import Reg._
import ChiselError._

object Backend {
}

abstract class Backend {
  def depthString(depth: Int): String = {
    var res = "";
    for (i <- 0 until depth)
      res += "  ";
    res
  }

 def emitTmp(node: Node): String

  def emitRef(node: Node): String = {
    if(node.name == "" || !node.named) 
      "T" + node.emitIndex 
    else if(!node.named)
      node.name + "_" + node.emitIndex
    else 
      node.name
  }

  def emitRef(c: Component): String =
    c.name

  def emitDec(node: Node): String = {
    node match {
      case m: MemAccess =>
        m.referenced = true
        ""
      case _ =>
        ""
    }
  }

  def emitDef(node: Node): String = ""

  def compile(c: Component): Unit = { }
}


