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
  def name(node: Node, isRealName: Boolean, consumer: Node = null): String = {
    def getName(node: Node) = {
      node match {
        case _: Literal => node.name
        case _ =>
          if (!isRealName) node.pName
          else if (node.name != "") node.name
          else Driver.backend.emitRef(node)
      }
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
          name(op.inputs(2).getNode, isRealName, op)
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
      case romdata: ROMData => {
        if (!romdata.lits.isEmpty) {
          val head = romdata.lits.head
          val tail = romdata.lits.tail
          (tail foldLeft ("Vec(%s){%s".format(
              name(romdata, isRealName),
              name(head.getNode, isRealName)))) { 
                (res, node) => res + "," + name(node.getNode, isRealName)
              } + "}"
        } else {
          "ROMData(%s)".format(name(romdata, isRealName))
        }
      }
      case _ => if (node == null) "" else node.toString
    }      
  }
}

trait Backannotation extends Backend {
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

  override def backannotationTransforms {
    super.backannotationTransforms
    transforms += { c => checkBackannotation(c) }
  }

  override def backannotationAnalyses { }
}

trait CounterBackannotation extends Backannotation {
  val crosses = new ArrayBuffer[(Double, Array[Node])]

  private def annotateSignals(c: Module) {
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
