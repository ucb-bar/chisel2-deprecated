package Chisel

import Chisel._
import Module._
import Node._
import Backend._
import ChiselError._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack

trait GraphTrace extends Backend {
  // preElaborateTransforms += ((c: Module) => printGraph(c.name + "_graph.rpt"))
  // analyses += ((c: Module) => printGraph(c.name + "_graph.rpt"))

  protected def getParentNames(m: Module, delim: String = "/"): String = {
    if (m == Module.topComponent) ""
    else getParentNames(m.parent) + emitRef(m) + delim
  }

  protected def getSignalName(n: Node, delim: String = "/"): String = {
    if (n == null) "null" else getParentNames(n.componentOf, delim) + emitRef(n)
  }

  def nodeToString(node: Node): String = { 
    node match {
      case bits  : Bits      => 
        if (!bits.isTypeNode || bits.inputs.length == 0) {
          if (bits.dir == OUTPUT) "OUTPUT(" + bits.name + ")"
          else if (bits.dir == INPUT) "INPUT(" +bits.name+ ")"
          else if (bits.name != null && !bits.name.isEmpty) bits.name 
          else "Bits(?)"
        }
        else nodeToString(bits.inputs(0).getNode)
      case bundle: Bundle    => "Bundle(" + 
        { for { (n, i) <- bundle.elements } yield n + " => " + nodeToString(i) + ", " } + ")"
      case vec   : Vec[_]    => "Vec(" + vec.name + ")"
      case reg   : Reg       => "Reg(" + reg.name + ")"
      case lit   : Literal   => "Lit(" + lit.name + ")"
      case op    : Op        => 
        if (op.inputs.length == 1) op.op + "(" + nodeToString(op.inputs(0)) + ")"
        else if (op.op == "Mux") "[ " + nodeToString(op.inputs(0)) + " ] ? [ " + nodeToString(op.inputs(1)) + " ] : [ " + nodeToString(op.inputs(2)) + " ]"
        else "[ " + nodeToString(op.inputs(0)) + " ] " + op.op + " [ " + nodeToString(op.inputs(1)) + " ]"
      case cat   : Cat       => 
        { "{ " + ((nodeToString(cat.inputs.head) /: cat.inputs.tail) (_ + ", " + nodeToString(_))) + " }" }
      case fill  : Fill      =>
        { "Fill(" + nodeToString(fill.inputs(0)) + ", " + nodeToString(fill.n) + ")" }
      case ext   : Extract   => 
        val hi: String = nodeToString(ext.hi)
        val lo: String = nodeToString(ext.lo) 
        nodeToString(ext.inputs(0)) + "[" + { if (hi == lo) hi else hi + ":" + lo } + "]"  
      case bind  : Binding   => "Binding(" + nodeToString(bind.inputs(0)) + ")"
      case mem   : Mem[_]    => "Mem(" + mem.name + ")"
      case memacc: MemAccess => nodeToString(memacc.mem) + "[" + nodeToString(memacc.addr) + "]"
      case rom   : ROM[_]    => { "ROM(" + rom.name + ")" }
      case romread: ROMRead[_] => nodeToString(romread.rom) + "[" + nodeToString(romread.addr) + "]"
      case _ => if (node == null) "" else node.toString
    }      
  }

  def printGraph(c: Module, filename: String = "graph.rpt", isDelayBack: Boolean = true) = {
    val walked = new HashSet[Node]
    val basedir = ensureDir(Module.targetDir)
    val rptdir  = ensureDir(basedir+"report")
    val rptfile = new java.io.FileWriter(rptdir + filename)
    var report = new StringBuilder();

    def printNode(top: Node, level: Int) = {
      report.append(genIndent(level) + getSignalName(top) + ": " + nodeToString(top) + {
        if (isDelayBack) {
          "  (delay: %.4f, selection delay: %.4f)\n".format(top.delay, top.seldelay) 
        } else {
          "\n" 
        }
      })
    }
   
    def dfs(c: Module) = {
      // initialization
      val stack = new Stack[(Node, Int)]
      val walked = new HashSet[Node]()
      
      for (a <- c.debugs) {
        stack push ((a, 1))
        walked += a
      }
      for ((n, flat) <- c.io.flatten) {
        stack push ((flat, 1))
        walked += flat
      }
      for (reset <- c.resets.values) {
        stack push ((reset, 1))
        walked += reset
      }

      // do DFS
      while(!stack.isEmpty) {
        val (node, level) = stack.pop
          node match {
            // case bits: Bits if bits.dir == INPUT /* || (bits.dir == OUTPUT && bits.componentOf != c) */ =>
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

    report.append("Module name : " + c.name + "\n")
    dfs(c)

    if(isDelayBack) {
      report.append("\nCritical path = " + Module.criticalPath)
      report.append("\nCritical path delay = %.5f\n".format(Module.criticalPathDelay))
    }

    // ChiselError.info(report) 
    // write files into report
    try {
      rptfile.write(report.toString)
    } finally {
      rptfile.close()
    }    
  }
}
