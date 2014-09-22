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

import scala.collection.mutable.{ArrayBuffer, HashSet, Queue=>ScalaQueue, Stack}

class CppVertex {
  val inputs = new ArrayBuffer[Node]
  val nodes = new ArrayBuffer[Node]
  val outputs = new ArrayBuffer[Node]
  val sortedNodes = new ArrayBuffer[Node]
  val inputVertices = new ArrayBuffer[CppVertex]
  val outputVertices = new ArrayBuffer[CppVertex]

  def sort(): ArrayBuffer[Node] = {
    println(nodes.size)
    val walked = new ArrayBuffer[Node]
    val stack = new Stack[(Boolean, Node)]
    for (o <- outputs) stack.push((false, o))
    while (!stack.isEmpty) {
      val (done, node) = stack.pop()
      if (done) {
        sortedNodes += node
      } else {
        if (!walked.contains(node)) {
          walked += node
          stack.push((true, node))
          for (i <- node.inputs) {
            if (i != null && i.CppVertex == this) {
              if (!i.isInstanceOf[Delay]) {
                stack.push((false, i))
              }
            }
          }
        }
      }
    }
    sortedNodes
  }

}

object bfs {
  def apply(start: Array[Node], visit: Node => Unit, next: Node => Array[Node]): Unit = {
    val walked = new HashSet[Node]
    val bfsQueue = new ScalaQueue[Node]
    bfsQueue ++= start
    walked ++= start
    while (!bfsQueue.isEmpty) {
      val top = bfsQueue.dequeue
      walked += top
      visit(top)
      for (i <- next(top)) {
        if (!(i == null)) {
          if (!walked.contains(i)) {
            bfsQueue.enqueue(i)
            walked += i
          }
        }
      }
    }
  }
}

class ModularCppBackend extends CppBackend {

  var threshold = 500

  def createVertices(module: Module): ArrayBuffer[CppVertex] = {
    val res = new ArrayBuffer[CppVertex]
    val roots = new ArrayBuffer[Node]
    val walked = new ArrayBuffer[Node]
    for (node <- module.mods) {
      if (node.isIo && node.asInstanceOf[Bits].dir == OUTPUT && node.consumers.length == 0)
        roots += node
    }

    val queue = new ScalaQueue[Array[Node]]
    queue.enqueue(roots.toArray)
    walked ++= roots
    while(!queue.isEmpty) {
      val nodes = queue.dequeue()
      val vertex = createVertex(nodes)
      if (vertex.nodes.size > 0) {
        res += vertex
        val nexts = vertex.inputs.filter(x => !walked.contains(x) && x.inputs.length > 0).toArray
        queue.enqueue(nexts)
        walked ++= nexts
      }
    }
    res
  }

  def createVertex(nodes: Array[Node]): CppVertex = {
    val res = new CppVertex()
    res.outputs ++= nodes
    bfs(nodes, 
        (node: Node) => if(!res.nodes.contains(node) && node.CppVertex == null) {res.nodes += node; node.CppVertex = res},
        (node: Node) => 
          if (node.CppVertex != res) {
            if (!node.isInstanceOf[Delay]) {
              res.inputVertices += node.CppVertex
              node.CppVertex.outputVertices += res
            }
            Array[Node]()
          } else {
            val next = node.inputs.filter(!_.isInstanceOf[Delay])
            node.inputs.map(x => if(x.isInstanceOf[Delay]) res.inputs += x)
            next.toArray
          }
      )
    res
  }

  override def elaborate(c: Module): Unit = {
    backendElaborate(c)
    ChiselError.checkpoint()

    val vertices = createVertices(c)
    println("HUY: started sort")
    for (vertex <- vertices) {
      vertex.sort()
      renameNodes(c, vertex.sortedNodes)
    }
    println("HUY: finished sort")

    val out_h = createOutputFile(c.name + ".h")
    val out_c = createOutputFile(c.name + ".cpp")
    out_h.write("#ifndef __" + c.name + "__\n");
    out_h.write("#define __" + c.name + "__\n\n");
    out_h.write("#include \"emulator.h\"\n\n");
    out_h.write("class " + c.name + "_t : public mod_t {\n");
    out_h.write(" public:\n");
    val vcd = new VcdBackend(c)
    for (vertex <- vertices) {
      for (m <- vertex.sortedNodes) {
        if(m.name != "reset") {
          if (m.isInObject) {
            out_h.write(emitDec(m));
          }
          if (m.isInVCD) {
            out_h.write(vcd.emitDec(m));
          }
        }
      }
    }

    out_h.write("\n");
    out_h.write("  void init ( val_t rand_init = false );\n");
    for ((vertex, i) <- vertices.zipWithIndex) {
      out_h.write("  void clock_lo_" + i + " ( dat_t<1> reset );\n")
    }
    out_h.write("  void clock_lo ( dat_t<1> reset );\n")
    out_h.write("  void clock_hi ( dat_t<1> reset );\n")
    out_h.write("  void print ( FILE* f );\n");
    out_h.write("  void dump ( FILE* f, int t );\n");
    out_h.write("  void dump_init ( FILE* f );\n");
    out_h.write("};\n\n");
    out_h.write("#endif\n");
    out_h.close();

    out_c.write("#include \"" + c.name + ".h\"\n");
    for (str <- Driver.includeArgs) out_c.write("#include \"" + str + "\"\n")
    out_c.write("\n");
    out_c.write("void " + c.name + "_t::init ( val_t rand_init ) {\n");
    for (vertex <- vertices) {
      for (m <- vertex.sortedNodes) {
        out_c.write(emitInit(m));
      }
    }
    out_c.write("}\n");

    for ((vertex, i) <- vertices.zipWithIndex) {
      out_c.write("void " + c.name + "_t::clock_lo_" + i + " ( dat_t<1> reset ) {\n")
      for (m <- vertex.sortedNodes) {
        out_c.write(emitDefLo(m))
      }
      out_c.write("}\n")
    }

    out_c.write("void " + c.name + "_t::clock_lo ( dat_t<1> reset ) {\n")
    for ((vertex, i) <- vertices.zipWithIndex) {
      out_c.write("  clock_lo_" + i + "( reset );\n")
    }
    out_c.write("}\n")
    out_c.write("void " + c.name + "_t::clock_hi ( dat_t<1> reset ) {\n")
    for (vertex <- vertices) {
      for (m <- vertex.sortedNodes) {
        out_c.write(emitInitHi(m))
      }
    }
    for (vertex <- vertices) {
      for (m <- vertex.sortedNodes) {
        out_c.write(emitDefHi(m))
      }
    }
    out_c.write("}\n")

    out_c.write("void " + c.name + "_t::print ( FILE* f ) {\n");
    for (cc <- Driver.components; p <- cc.printfs)
      out_c.write("#if __cplusplus >= 201103L\n"
        + "  if (" + emitLoWordRef(p.cond)
        + ") dat_fprintf<" + p.needWidth() + ">(f, "
        + p.args.map(emitRef _).foldLeft(CString(p.format))(_ + ", " + _)
        + ");\n"
        + "#endif\n")
    out_c.write("}\n");
    vcd.dumpVCDInit(out_c.write);
    vcd.dumpVCD(out_c.write);
    out_c.close();

    /* Copy the emulator.h file into the targetDirectory. */
    val resourceStream = getClass().getResourceAsStream("/emulator.h")
    if( resourceStream != null ) {
      val classFile = createOutputFile("emulator.h")
      while(resourceStream.available > 0) {
        classFile.write(resourceStream.read())
      }
      classFile.close()
      resourceStream.close()
    }
  }

}
