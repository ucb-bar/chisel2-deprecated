/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

package Chisel

import ChiselError._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.io.Source

object nodeToString {
  def name(node: Node, consumer: Node = null): String = {
    node match {
      case b: Binding => 
        name(b.targetNode)
      case io: Bits if io.isIo && io.dir == INPUT && !io.inputs.isEmpty && consumer != null => {
        if ((!consumer.isIo && 
             consumer.component == io.component.parent) ||
            (consumer.isInstanceOf[Bits] && 
             consumer.asInstanceOf[Bits].dir == OUTPUT && 
             consumer.component == io.component.parent)) {
          Driver.backend.emitRef(io.inputs.head)
        } else if (io.component == consumer.component) {
          Driver.backend.emitRef(io.getNode)
        } else ""
      }
      case _ => 
        Driver.backend.emitRef(node.getNode)
    }
  }

  def apply(node: Node): String = { 
    node match {
      case bits  : Bits      => 
        if (bits.dir == OUTPUT && !bits.isTypeNode) 
          "OUTPUT(%s)".format(name(bits.getNode))
        else if (bits.dir == INPUT && !bits.isTypeNode) 
          "INPUT(%s)".format(name(bits.getNode))
        else bits match {
          case bool  : Bool => "Bool(%s)".format(name(bool.getNode))
          case uint  : UInt => "UInt(%s)".format(name(uint.getNode))
          case _ => "Bits(%s)".format(name(bits.getNode))
        }
      case reg   : Reg       => "Reg(%s)".format(name(reg))
      case lit   : Literal   => "Lit(%s)".format(lit.name)
      case op    : Op        => 
        if (op.inputs.length == 1) op.op + "[%s]".format(
          name(op.inputs.head.getNode, op))
        else if (op.op == "Mux") "[%s]?[%s]:[%s]".format(
          name(op.inputs(0).getNode, op),
          name(op.inputs(1).getNode, op),
          name(op.inputs(2).getNode, op)
        )
        else "[%s]%s[%s]".format(
          name(op.inputs(0).getNode, op), 
          op.op, 
          name(op.inputs(1).getNode, op) 
        )
      case ext   : Extract   => 
        val hi: String = name(ext.hi.getNode, ext)
        val lo: String = name(ext.lo.getNode, ext) 
        name(ext.inputs.head.getNode, ext) + "[" + { if (hi == lo) hi else hi + ":" + lo } + "]"  
      case bind  : Binding   => "Binding(" + name(bind.targetNode.getNode) + ")"
      case bundle: Bundle    => 
        if (!bundle.elements.isEmpty) {
          val head = bundle.elements.head._2
          val tail = bundle.elements.tail
          (tail foldLeft ("Bundle(%s){%s".format(name(bundle), name(head.getNode)))) { 
            (res, elem) => res + "," + name(elem._2.getNode)
          } + "}"
        } else {
          "Bundle(%s)".format(name(bundle))
        }
      case rom   : ROM[_]    => 
        if (!rom.self.isEmpty) {
          val head = rom.self.head
          val tail = rom.self.tail
          (tail foldLeft ("ROM(%s){%s".format(name(rom), name(head.getNode)))) { 
            (res, node) => res + "," + name(node.getNode)
          } + "}"
        } else {
          "ROM(%s)".format(name(rom))
        }
      case vec   : Vec[_]    =>
        if (!vec.self.isEmpty) {
          val head = vec.self.head
          val tail = vec.self.tail
          (tail foldLeft ("Vec(%s){%s".format(name(vec), name(head.getNode)))) { 
                (res, node) => res + "," + name(node.getNode)
              } + "}"
        } else {
          "Vec(%s)".format(name(vec))
        }
      case mem   : Mem[_]    => "MEM(%s)[%d]".format(name(mem), mem.n)
      case memacc: MemAccess => "%s[%s]".format(name(memacc.mem), nodeToString(memacc.addr))
      case romread: ROMRead => "%s[%s]".format(name(romread.rom), nodeToString(romread.addr))
      case romdata: ROMData => {
        if (!romdata.lits.isEmpty) {
          val head = romdata.lits.head
          val tail = romdata.lits.tail
          (tail foldLeft ("Vec(%s){%s".format(name(romdata), name(head.getNode)))) { 
                (res, node) => res + "," + name(node.getNode)
              } + "}"
        } else {
          "ROMData(%s)".format(name(romdata))
        }
      }
      case _ => if (node == null) "" else node.toString
    }      
  }
}

/*
object Backannotation extends Backend {
  Driver.isBackannotating = true

  lazy val targetdir = ensureDir(Driver.targetDir)
  def copyResource(filename: String, toDir: String) {
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
      val lines = Source.fromFile("%s.trace".format(targetdir + c.pName)).getLines.toSet
      var ok = true 
      for (m <- Driver.sortedComps) {
        m dfs { node =>
          node match {
            case _: Assert =>
            case _: PrintfBase =>
            case _: Binding =>
            case _: Literal =>
            case _ => if (!node.isTypeNode) {
              val l = getSignalPathName(node) + ":" + nodeToString(node)
              val contains = lines contains l
              if (!contains) 
                ChiselError.warning(
                  "[Backannotation] %s does not appear in the trace".format(l)
                )
              ok &= contains
            }
          }
        }
      }

      if (ok) {
        ChiselError.info("[Backannotation] no naming problems")
      } 
    } catch {
      case ex: java.io.FileNotFoundException => 
        ChiselError.warning("[Backannotation] no trace file (%s.trace), ".format(targetdir + c.pName) + 
                            "no backannotation verification")
    } 
  }

  def annotateSignals(c: Module) { }

  override def backannotationTransforms {
    super.backannotationTransforms
    transforms += { c => checkBackannotation(c) }
    transforms += { c => annotateSignals(c) }
  }

  override def backannotationAnalyses { }
}

trait CounterBackannotation extends Backannotation {
  val crosses = new ArrayBuffer[(Double, Array[Node])]

  override def annotateSignals(c: Module) {
    ChiselError.info("[Backannotation] annotate signals")

    try {
      // Read the signal list file
      val lines = Source.fromFile(Driver.model).getLines
      val TermRegex = """\s*([\w\._\:]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)\s+([\d\.\+-e]+)""".r
      val signalNames = new HashSet[String]
      val signalNameMap = new HashMap[String, Node]
      val coeffs = new HashSet[(Double, Array[String])]

      for (line <- lines) {
        line match {
          case TermRegex(exp, coeff, se, tstat, pvalue) => {
            val vars = exp split ":"
            if (tstat != "NaN" && pvalue != "NaN") {
              signalNames ++= vars
              coeffs += ((coeff.toDouble, vars))
            }
          }
          case _ =>
        }
      }

      // Find correspoinding nodes
      for (m <- Driver.sortedComps ; if m != c) {
        for (node <- m.nodes) {
          val signalName = getSignalPathName(node, ".")
          if (signalNames contains signalName){
            m.counter(node)
            m.debug(node)
            signalNameMap(signalName) = node
          }
        }
        for ((reset, pin) <- m.resets) {
          val resetPinName = getSignalPathName(pin, ".")
          if (signalNames contains resetPinName) {
            m.counter(pin)
            m.debug(pin)
            signalNameMap(resetPinName) = pin
          }
        }
      }
   
      for ((coeff, vars) <- coeffs) {
        val cross = vars map { x => signalNameMap getOrElse (x, null) }
        if (!(cross contains null)) {
          crosses += ((coeff, cross))
        }
      } 
    } catch {
      case ex: java.io.FileNotFoundException => 
        ChiselError.warning("[Backannotation] no signal file, no backannotation")
    }
  }
}
*/
