package Chisel
import Node._
import Reg._
import ChiselError._
import Component._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet

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

  val transforms = ArrayBuffer[(Component) => Unit]()
  val postResolvedTransforms = ArrayBuffer[(Component) => Unit]()

  // DFS walk of graph to collect members of every component
  def resolveNodesComponent(c: Component) = {
    val dfsStack = new Stack[(Node, Component)]()
    val walked = new HashSet[Node]()

    def walk = {
      var nextComp = c

      while(!dfsStack.isEmpty) {
        val (node, curComp) = dfsStack.pop

        if(!walked.contains(node)) {
          walked += node
          // push and pop components as necessary
          node match {
            case io: Bits => {
              if (io.dir == OUTPUT) // push
                nextComp = io.component
              else if (io.dir == INPUT) // pop
                nextComp = io.component.parent
              else // do nothing
                nextComp = curComp
            }
            case any => // do nothing
              nextComp = curComp
          }

          // collect inputs into component
          for (input <- node.inputs) {
            if(!walked.contains(input)) {
              nextComp.nodes += input
              input.resolvedComp = nextComp
              dfsStack.push((input, nextComp))
            }
          }
        }
      }

    }

    println("resolving nodes to the components")
    // start DFS from top level inputs
    // dequeing from dfsStack => walked
    for ((name, io) <- c.io.flatten) {
      assert(io.isInstanceOf[Bits])
      if(io.asInstanceOf[Bits].dir == OUTPUT) {
        c.nodes += io
        io.resolvedComp = c
        dfsStack.push((io, c))
      }
    }

    walk;
    assert(dfsStack.isEmpty)
    println("finished resolving")
  }

  def transform(c: Component, transforms: ArrayBuffer[(Component) => Unit]): Unit = {
    for (t <- transforms)
      t(c)
  }


  def emitDef(node: Node): String = ""

  def elaborate(c: Component): Unit = { }

  def compile(c: Component, flags: String = null): Unit = { }
}


