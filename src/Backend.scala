package Chisel
import Node._
import Reg._
import ChiselError._
import Component._

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
    node match {
      case r: Reg => 
        if(r.isMemOutput) emitRef(r.updateVal) else if(r.name == "") "R" + r.emitIndex else r.name
      case _ => 
        if(node.name == "" || !node.named) 
          "T" + node.emitIndex 
        else if(!node.named)
          node.name + "_" + node.emitIndex
        else 
          node.name
    }
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

  val transforms = collection.mutable.ArrayBuffer[(Component) => Unit]()

  def transform(c: Component): Unit = {
    for (t <- transforms)
      t(c)
  }

  def emitDef(node: Node): String = ""

  def elaborate(c: Component): Unit = { }

  def compile(c: Component, flags: String = null): Unit = { }
}


