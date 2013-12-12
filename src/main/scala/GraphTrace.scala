package Chisel

import Chisel._
import Module._
import Node._
import Backend._
import ChiselError._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack

object nodeToString {
  def apply(node: Node) = nodeString(node)

  def nodeString(node: Node): String = { 
    node match {
      case bits  : Chisel.Bits      => 
        if (!node.isTypeNode || node.inputs.length == 0) {
          if (bits.dir == OUTPUT) "OUTPUT(" + bits.name + ")"
          else if (bits.dir == INPUT) "INPUT(" +bits.name+ ")"
          else if (bits.name != null && !bits.name.isEmpty) bits.name 
          else "Bits(?)"
        }
        else nodeString(node.inputs(0).getNode)
      case bundle: Chisel.Bundle    => "Bundle(" + 
        { for { (n, i) <- bundle.elements } yield n + " => " + nodeString(i) + ", " } + ")"
      case vec   : Chisel.Vec[_]    => "Vec(" + vec.name + ")"
      case reg   : Chisel.Reg       => "Reg(" + reg.name + ")"
      case lit   : Chisel.Literal   => "Lit(" + lit.name + ")"
      case op    : Chisel.Op        => 
        if (op.inputs.length == 1) op.op + "(" + nodeString(op.inputs(0)) + ")"
        else if (op.op == "Mux") "[ " + nodeString(op.inputs(0)) + " ] ? [ " + nodeString(op.inputs(1)) + " ] : [ " + nodeString(op.inputs(2)) + " ]"
        else "[ " + nodeString(op.inputs(0)) + " ] " + op.op + " [ " + nodeString(op.inputs(1)) + " ]"
      case cat   : Chisel.Cat       => 
        { "{ " + ((nodeString(cat.inputs.head) /: cat.inputs.tail) (_ + ", " + nodeString(_))) + " }" }
      case ext   : Chisel.Extract   => 
        val hi: String = nodeString(ext.hi)
        val lo: String = nodeString(ext.lo) 
        nodeString(ext.inputs(0)) + "[" + { if (hi == lo) hi else hi + ":" + lo } + "]"  
      case bind  : Chisel.Binding   => "Binding(" + nodeString(bind.inputs(0)) + ")"
      case _ => node.toString
    }      
  }
}

trait GraphTrace extends Backend {
  // Print the graph before 'elaborate'
  // preElaborateTransforms += ((c: Module) => printGraph(c.name + "_preelabgraph.rpt")) 
  // Print the graph after 'elaborate'
  // analyses               += ((c: Module) => printGraph(c.name + "_postelabgraph.rpt")) 

  protected def printGraph(filename: String = "graph.rpt") = {
    val walked = new HashSet[Node]
    val basedir = ensureDir(Module.targetDir)
    val rptdir  = ensureDir(basedir+"report")
    val rptfile = new java.io.FileWriter(rptdir + filename)
    var report = new StringBuilder();

    def printNode(top: Node, level: Int) = {
      report.append(genIndent(level) + nodeToString(top) + 
                    "\t(arrival: %.4f, delay: %.4f)\n".format(top.arrival, top.delay))
    }
   
    def dfs(c: Module) = {
      // initialization
      val stack = new Stack[(Node, Int)]
      val walked = new HashSet[Node]()
      
      for (a <- c.debugs) {
        stack push ((a, 0))
        walked += a
      }
      for ((n, flat) <- c.io.flatten) {
        stack push ((flat, 0))
        walked += flat
      }

      // do DFS
      while(!stack.isEmpty) {
        val (node, level) = stack.pop

        node match {
          case bits: Bits if bits.dir == INPUT =>
          case _: Literal =>
          case _ => {
            printNode(node, level)
            for (input <- node.inputs) {
              if (walked contains input) {
                input match {
                  case _: Literal =>
                  case _ => printNode(input, level+1)
                }
              } else {
                walked += input
                stack push ((input, level + 1))
              }
            }
          }
        } 
      }    
    }

    report.append("\t\t+---------------------------+\n")
    report.append("\t\t|       Graph Traces        |\n")
    report.append("\t\t|             by Donggyu    |\n")
    report.append("\t\t+---------------------------+\n\n")

    for (c <- Module.components) {
      report.append("Module name : " + c.name + "\n")
      dfs(c)
    }

    report.append("\nCritical path delay = %.5f\n".format(Module.criticalPathDelay))

    // ChiselError.info(report) 
    // write files into report
    try {
      rptfile.write(report.toString)
    } finally {
      rptfile.close()
    }    
  }

  override def elaborate(c: Module) {
    super.elaborate(c)

    printGraph(c.name + "_graph.rpt") 
  }
}
