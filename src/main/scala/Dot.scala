/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
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
import Node._
import Reg._
import ChiselError._
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import PartitionIslands._

class DotBackend extends Backend {
  val keywords = new HashSet[String]();
  val islandIds = ArrayBuffer[Int]()

  override def emitRef(node: Node): String = {
    node match {
      case r: Reg =>
        if (r.name == "") {
          r.name = "R" + r.emitIndex
        }
      case _ =>
        if(node.name == "") {
          node.name = "T" + node.emitIndex
        }
    }
    fullyQualifiedName(node)
  }

  private def isDottable (m: Node): Boolean = {
    if (m == m.component.defaultResetPin) {
      false
    } else {
      m match {
        case x: Literal  => false;
        case _           => true;
      }
    }
  }


  private def asValidLabel( node: Node ): String = {
    node match {
      case operator: Op => if (operator.op == "") "?" else operator.op;
      case _             => {
        val typeName = node.getClass.getName.substring(7)
        node.name + ":" + typeName
      }
    }
  }


  private def emitModuleText(top: Module, depth: Int ): (String, String) = {
    val res = new StringBuilder()
    val crossings = new StringBuilder()
    val indent = "  " * (depth + 1)
    for (child <- top.children) {
      /* Prefix by "cluster" for graphviz to draw a bounding box. */
      res.append(indent)
      res.append("subgraph cluster" + emitRef(child) + "{\n")
      res.append("  " + indent)
      res.append("label = \"" + child.name + "\"\n")
      val (innertext, innercrossings) = emitModuleText(child, depth + 1)
      res.append(innertext)
      res.append(indent)
      res.append("}\n")
      res.append(indent)
      res.append(innercrossings)
    }
    var EOL = "\n"
    for (islandId <- islandIds) {
      if (islandId != 0) {
        res.append("subgraph clusterIsland_" + islandId + " {\n")
      }
      for (m <- top.mods) {
        if (isDottable(m)) {
          if( m.component == top ) {
            /* We have to check the node's component agrees because output
             nodes are part of a component *mods* as well as its parent *mods*! */
            if (m.islandId == islandId) {
              res.append(indent)
              res.append(emitRef(m));
            }
            var label  = "label=\"" + asValidLabel(m)
            val anyLit = m.inputs.find(x => !isDottable(x));
            if (!anyLit.isEmpty) {
              var i = 0;
              label += "(";
              for (in <- m.inputs) {
                if (i != 0) label += ", ";
                label += (if (in.isLit) emitRef(in) else "_");
                i += 1;
              }
              label += ")";
            }
            label += "\""
            if (m.islandId == islandId) {
              m match {
                case reg: Delay => res.append("[shape=square," + label + "];" + EOL)
                case _ => res.append("[" + label + "];" + EOL)
              }
            }
          }
        }
      }
      for (m <- top.mods) {
        if( m.component == top ) {
          /* We have to check the node's component agrees because output
           nodes are part of a component *mods* as well as its parent *mods*! */
          for (in <- m.inputs) {
            // Draw the vertex after the nodes have been defined.
            val maxIslandId = math.max(m.islandId, in.islandId)
            if (isDottable(m) && isDottable(in) && maxIslandId == islandId) {
              val edge = (emitRef(in) + " -> " + emitRef(m)
                + "[label=\"" + in.width_ + "\"];"+ EOL)
              /* If the both ends of an edge are on either side of a component
               boundary, we must add it at the upper level otherwise graphviz
               will incorrectly draw the input node into the cluster. */
              if( in.component != top && !top.children.contains(in.component) ) {
                crossings.append(edge)
              } else {
                res.append(indent)
                res.append(edge);
              }
            }
          }
        }
      }
      if (islandId != 0) {
        res.append("label = \"Island_" + islandId + "\";\n")
        res.append("}\n")
      }
    }
    (res.toString, crossings.toString)
  }


  override def elaborate(c: Module): Unit = {
    super.elaborate(c)

    if (Driver.partitionIslands) {
      val islands = createIslands(c)
      for (chain <- islands) islandIds += chain.chainId
    } else {
      islandIds += 0
    }
    var gn = -1;
    val out_cd = createOutputFile(c.name + "_c.dot");
    out_cd.write("digraph TopTop {\n");
    out_cd.write("rankdir = LR;\n");
    def genNum: Int = { gn += 1; gn };
    def dumpComponent (c: Module): Unit = {
      out_cd.write("subgraph cluster" + c.name + "{\n");
      out_cd.write("label = \"" + c.name + "\";\n");
      def dumpIo (n: String, d: Data): Unit = {
        d match {
          case b: Bundle =>
            out_cd.write("subgraph cluster" + n + "__" + genNum + "{\n");
            out_cd.write("node [shape=box];\n");
            out_cd.write("label = \"" + n + "\";\n");
            for ((cn, cd) <- b.elements)
              dumpIo(cn, cd);
            out_cd.write("}\n");
          case o =>
            out_cd.write(emitRef(d) + "[label=\"" + n + "\"];\n");
            for (in <- d.inputs)
              if (isDottable(in)) {
                out_cd.write(emitRef(in) + " -> " + emitRef(d) + "[label=\"" + in.width_ + "\"];\n");
              }
        }
      }
      dumpIo("io", c.io);
      for (cc <- c.children)
        dumpComponent(cc);
      out_cd.write("}\n");
    }
    dumpComponent(c);
    out_cd.write("}");
    out_cd.close();

    val out_d = createOutputFile(c.name + ".dot");
    out_d.write("digraph " + c.name + "{\n");
    out_d.write("rankdir = LR;\n");
    val (innertext, innercrossings) = emitModuleText(c, 0)
    out_d.write(innertext);
    Predef.assert(innercrossings.length == 0,
      {println("length:" + innercrossings.length + ", " + innercrossings)})
    out_d.write("}");
    out_d.close();
  }
}
