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
  def name(node: Node, isRealName: Boolean) =
    if (!isRealName) node.pName 
    else if (node.name != "") node.name
    else Module.backend.emitRef(node)

  def apply(node: Node, isRealName: Boolean = false): String = { 
    node match {
      case bits  : Bits      => 
        if (bits.dir == OUTPUT) "OUTPUT"
        else if (bits.dir == INPUT) "INPUT"
        else bits match {
          case bool  : Bool => "Bool(%s)".format(name(bool.getNode, isRealName))
          case uint  : UInt => "UInt(%s)".format(name(uint.getNode, isRealName))
          case _ => "Bits(%s)".format(name(bits.getNode, isRealName))
        }
      case reg   : Reg       => "Reg(%s)".format(name(reg, isRealName))
      case lit   : Literal   => "Lit(%s)".format(lit.name)
      case op    : Op        => 
        if (op.inputs.length == 1) op.op + "(%s)".format(name(op.inputs(0).getNode, isRealName))
        else if (op.op == "Mux") "[%s]?[%s]:[%s]".format(
          name(op.inputs(0).getNode, isRealName),
          name(op.inputs(1).getNode, isRealName),
          name(op.inputs(2).getNode, isRealName) )
        else "[%s]%s[%s]".format(
          name(op.inputs(0).getNode, isRealName), 
          op.op, 
          name(op.inputs(1).getNode, isRealName) )
      case ext   : Extract   => 
        val hi: String = nodeToString(ext.hi)
        val lo: String = nodeToString(ext.lo) 
        nodeToString(ext.inputs(0)) + "[" + { if (hi == lo) hi else hi + ":" + lo } + "]"  
      case bind  : Binding   => "Binding(" + nodeToString(bind.targetNode) + ")"
      case bundle: Bundle    => 
        if (!bundle.elements.isEmpty) {
          val head = bundle.elements.head._2
          val tail = bundle.elements.tail
          (tail foldLeft ("Bundle(%s){%s".format(
              name(bundle, isRealName), 
              name(head.getNode, isRealName)))) { 
            (res, elem) => res + "," + name(elem._2.getNode, isRealName)
          } + "}"
        } else {
          "Bundle(%s)".format(name(bundle, isRealName))
        }
      case rom   : ROM[_]    => 
        if (!rom.self.isEmpty) {
          val head = rom.self.head
          val tail = rom.self.tail
          (tail foldLeft ("ROM(%s){%s".format(
              name(rom, isRealName), 
              name(head.getNode, isRealName)))) { 
            (res, node) => res + "," + name(node.getNode, isRealName)
          } + "}"
        } else {
          "ROM(%s)".format(name(rom, isRealName))
        }
      case vec   : Vec[_]    =>
        if (!vec.self.isEmpty) {
          val head = vec.self.head
          val tail = vec.self.tail
          (tail foldLeft ("Vec(%s){%s".format(
              name(vec, isRealName),
              name(head.getNode, isRealName)))) { 
            (res, node) => res + "," + name(node.getNode, isRealName)
          } + "}"
        } else {
          "Vec(%s)".format(name(vec, isRealName))
        }
      case mem   : Mem[_]    => {
        "MEM(%s)[%d]".format(name(mem, isRealName), mem.n)
      }
      case memacc: MemAccess => "%s[%s]".format(
        name(memacc.mem, isRealName), nodeToString(memacc.addr))
      case romread: ROMRead[_] => "%s[%s]".format(
        name(romread.rom, isRealName), nodeToString(romread.addr))
      // case clk   : Clock     => "Clock(%s)".format(clk.pName)
      case _ => if (node == null) "" else node.toString
    }      
  }
}

trait Backannotation extends Backend {
  Module.isBackannotating = true

  val targetdir = ensureDir(Module.targetDir)
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

  override def checkBackannotation(c: Module) {
    ChiselError.info("[Backannotation] check backannotation")
    try {
      val lines = Source.fromFile("%s.dfs".format(targetdir + c.pName)).getLines.toArray
      val dfsTraversal = new HashSet[String]
      
      c dfs { node =>
        dfsTraversal += getSignalPathName(node) + ":" + nodeToString(node)
      }

      var ok = true

      if (ok) {
        for (line <- lines) {
          val contains = dfsTraversal contains line
          if (!(dfsTraversal contains line))
            ChiselError.warning("[Backannotation] %s does not appear in this graph".format(line))
          ok &= contains
        }
      }

      if (ok) {
        ChiselError.info("[Backannotation] no naming problems")
      } 
    } catch {
      case ex: java.io.FileNotFoundException => 
        ChiselError.warning("[Backannotation] no DFS file (%s.dfs): cannot verify backannotation".format(c.pName))
    } 
  }

  override def backannotationAnalyses { }
}
