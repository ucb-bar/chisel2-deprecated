package Chisel

// import GraphTrace._
import ChiselError._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.io.Source
import scala.sys.process._
import scala.util.Random

object nodeToString {
  def apply(node: Node): String = { 
    node match {
      case bits  : Bits      => 
        if (!bits.isTypeNode || bits.inputs.length == 0) {
          if (bits.dir == OUTPUT) "OUTPUT(" + bits.name + ")"
          else if (bits.dir == INPUT) "INPUT(" +bits.name+ ")"
          else if (bits.name != null && !bits.inputs.isEmpty) 
            "Bits(%s)".format(nodeToString(bits.inputs(0)))
          else "Bits(?)"
        }
        else nodeToString(bits.inputs(0).getNode)
      case bundle: Bundle    => "Bundle(" + 
        { for { (n, i) <- bundle.elements } yield n + " => " + nodeToString(i) + ", " } + ")"
      case vec   : Vec[_]    => "Vec(" + vec.name + ")"
      case reg   : Reg       => "Reg(%s)".format(reg.name)
      case lit   : Literal   => "Lit(%s)".format(lit.name)
      case op    : Op        => 
        if (op.inputs.length == 1) op.op + "(" + nodeToString(op.inputs(0)) + ")"
        else if (op.op == "Mux") "[ " + nodeToString(op.inputs(0)) + " ] ? [ " + nodeToString(op.inputs(1)) + " ] : [ " + nodeToString(op.inputs(2)) + " ]"
        else "[ " + nodeToString(op.inputs(0)) + " ] " + op.op + " [ " + nodeToString(op.inputs(1)) + " ]"
      case ext   : Extract   => 
        val hi: String = nodeToString(ext.hi)
        val lo: String = nodeToString(ext.lo) 
        nodeToString(ext.inputs(0)) + "[" + { if (hi == lo) hi else hi + ":" + lo } + "]"  
      case bind  : Binding   => "Binding(" + nodeToString(bind.targetNode) + ")"
      case mem   : Mem[_]    => "Mem(%s)".format(mem.name)
      case memacc: MemAccess => nodeToString(memacc.mem) + "[" + nodeToString(memacc.addr) + "]"
      // case rom   : ROM[_]    => "ROM(%s)".format(rom.name) 
      case romread: ROMRead[_] => nodeToString(romread.rom) + "[" + nodeToString(romread.addr) + "]"
      case clk   : Clock     => "Clock(%s)".format(clk.name)
      case _ => if (node == null) "" else node.toString
    }      
  }
}

trait Backannotation extends Backend {
  val targetdir = ensureDir(Module.targetDir)

  protected def getPseudoPath(m: Module, delim: String = "/"): String = {
    if (m.parent == null) m.pName else getPseudoPath(m.parent) + delim + m.pName
  }

  protected def getSignalPathName(n: Node, delim: String = "/"): String = {
    if (n == null || n.pName == "") "null" else getPseudoPath(n.component) + delim + n.pName
  }

  protected def copyResource(filename: String, toDir: String) {
    val resourceStream = getClass getResourceAsStream "/" + filename //Todo: understand it (Java?)
    if (resourceStream != null) {
      val file =  new java.io.FileWriter(toDir+filename)
      while(resourceStream.available > 0) {
        file write (resourceStream.read)
      }
      file.close
      resourceStream.close 
    } else {
      ChiselError.info("Critical Error: we should be able to access resource/" + filename)
    }
  }
}
