package Chisel

// import GraphTrace._
import ChiselError._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.io.Source
import scala.sys.process._
import scala.util.Random

object nodeToString {
  def name(node: Node, isRealName: Boolean, consumer: Node = null): String = {
    def getName(node: Node) = {
      if (!isRealName) node.pName
      else if (node.name != "") node.name
      else Module.backend.emitRef(node)
    }

    node match {
      case b: Binding => 
        name(b.targetNode, isRealName)
      case io: Bits if io.isIo && io.dir == INPUT && !io.inputs.isEmpty && consumer != null => {
        if ((!consumer.isIo && 
             consumer.component == io.component.parent) ||
            (consumer.isInstanceOf[Bits] && 
             consumer.asInstanceOf[Bits].dir == OUTPUT && 
             consumer.component == io.component.parent)) {
          getName(io.inputs.head)
        } else if (io.component == consumer.component) {
          getName(io.getNode)
        } else ""
      }
      case _ => 
        getName(node.getNode)
    }
  }

  def apply(node: Node, isRealName: Boolean = false): String = { 
    node match {
      case bits  : Bits      => 
        if (bits.dir == OUTPUT && !bits.isTypeNode) "OUTPUT"
        else if (bits.dir == INPUT && !bits.isTypeNode) "INPUT"
        else bits match {
          case bool  : Bool => "Bool(%s)".format(name(bool.getNode, isRealName))
          case uint  : UInt => "UInt(%s)".format(name(uint.getNode, isRealName))
          case _ => "Bits(%s)".format(name(bits.getNode, isRealName))
        }
      case reg   : Reg       => "Reg(%s)".format(name(reg, isRealName))
      case lit   : Literal   => "Lit(%s)".format(lit.name)
      case op    : Op        => 
        if (op.inputs.length == 1) op.op + "[%s]".format(
          name(op.inputs.head.getNode, isRealName, op))
        else if (op.op == "Mux") "[%s]?[%s]:[%s]".format(
          name(op.inputs(0).getNode, isRealName, op),
          name(op.inputs(1).getNode, isRealName, op),
          op.inputs(2) match {
            case mux: Mux if (Module.pseudoMuxes contains mux.inputs(2).getNode) =>
              name(Module.pseudoMuxes(mux.inputs(2).getNode), isRealName, op)
            case _ =>
              name(op.inputs(2).getNode, isRealName, op)
          } 
        )
        else "[%s]%s[%s]".format(
          name(op.inputs(0).getNode, isRealName, op), 
          op.op, 
          name(op.inputs(1).getNode, isRealName, op) 
        )
      case ext   : Extract   => 
        val hi: String = name(ext.hi.getNode, isRealName, ext)
        val lo: String = name(ext.lo.getNode, isRealName, ext) 
        name(ext.inputs.head.getNode, isRealName, ext) + "[" + { if (hi == lo) hi else hi + ":" + lo } + "]"  
      case bind  : Binding   => "Binding(" + name(bind.targetNode.getNode, isRealName) + ")"
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
        name(memacc.mem, isRealName), nodeToString(memacc.addr, isRealName))
      case romread: ROMRead => "%s[%s]".format(
        name(romread.rom, isRealName), nodeToString(romread.addr, isRealName))
      // case clk   : Clock     => "Clock(%s)".format(clk.pName)
      case _ => if (node == null) "" else node.toString
    }      
  }
}

trait Backannotation extends Backend {
  Module.isBackannotating = true

  lazy val targetdir = ensureDir(Module.targetDir)
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

  def checkBackannotation(c: Module) {
    ChiselError.info("[Backannotation] check backannotation")
    try {
      val lines = Source.fromFile("%s.trace".format(targetdir + c.pName)).getLines.toArray
      val traversal = new LinkedHashSet[String]
      
      for (m <- Module.sortedComps) {
        m dfs { node =>
          node match {
            case _: Assert =>
            case _: PrintfBase =>
            case _: Binding =>
            case _: Literal =>
            case _ => if (!node.isTypeNode) {
              traversal += getSignalPathName(node) + ":" + nodeToString(node)
            }
          }
        }
      }

      var ok = true

      if (ok) {
        for (line <- lines) {
          val contains = traversal contains line
          if (!contains)
            ChiselError.warning("[Backannotation] %s does not appear in this graph".format(line))
          ok &= contains
        }
      }

      if (ok) {
        ChiselError.info("[Backannotation] no naming problems")
      } 
    } catch {
      case ex: java.io.FileNotFoundException => 
        ChiselError.warning("[Backannotation] no trace file (%s.trace), ".format(Module.targetDir + c.pName) + 
                            "no backannotation verification")
    } 
  }

  override def backannotationAnalyses { }
}
