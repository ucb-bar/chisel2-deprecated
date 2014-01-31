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
    c.findConsumers()
    c.verifyAllMuxes
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
    val vcd = new VcdBackend()
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
    out_h.write("  void init ( bool rand_init = false );\n");
    for ((vertex, i) <- vertices.zipWithIndex) {
      out_h.write("  void clock_lo_" + i + " ( dat_t<1> reset );\n")
    }
    out_h.write("  void clock_lo ( dat_t<1> reset );\n")
    out_h.write("  void clock_hi ( dat_t<1> reset );\n")
    out_h.write("  void print ( FILE* f );\n");
    out_h.write("  bool scan ( FILE* f );\n");
    out_h.write("  void dump ( FILE* f, int t );\n");
    out_h.write("};\n\n");
    out_h.write("#endif\n");
    out_h.close();

    out_c.write("#include \"" + c.name + ".h\"\n");
    for(str <- Module.includeArgs) out_c.write("#include \"" + str + "\"\n");
    out_c.write("\n");
    out_c.write("void " + c.name + "_t::init ( bool rand_init ) {\n");
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

    def splitFormat(s: String): Seq[String] = {
      var off = 0;
      var res: List[String] = Nil;
      for (i <- 0 until s.length) {
        if (s(i) == '%') {
          if (off < i) {
            res = s.substring(off, i) :: res;
          }
          res = "%" :: res;
          if (i == (s.length-1)) {
            ChiselError.error("Badly formed format argument kind: %");
          } else if (s(i + 1) != 'x') {
            ChiselError.error("Unsupported format argument kind: %" + s(i + 1));
          }
          off = i + 2;
        }
      }
      if (off < (s.length-1)) {
        res = s.substring(off, s.length) :: res;
      }
      res.reverse
    }
    out_c.write("void " + c.name + "_t::print ( FILE* f ) {\n");
    for (p <- Module.printfs)
      out_c.write("#if __cplusplus >= 201103L\n"
        + "  if (" + emitLoWordRef(p.cond)
        + ") dat_fprintf<" + p.width + ">(f, "
        + p.args.map(emitRef _).foldLeft(CString(p.format))(_ + ", " + _)
        + ");\n"
        + "#endif\n")
    if (Module.printArgs.length > 0) {
      val format =
        if (Module.printFormat == "") {
          Module.printArgs.map(a => "%x").reduceLeft((y,z) => z + " " + y)
        } else {
          Module.printFormat;
        }
      val toks = splitFormat(format);
      var i = 0;
      for (tok <- toks) {
        if (tok(0) == '%') {
          val nodes = Module.printArgs(i).maybeFlatten
          for (j <- 0 until nodes.length)
            out_c.write("  fprintf(f, \"" + (if (j > 0) " " else "") +
                        "%s\", TO_CSTR(" + emitRef(nodes(j)) + "));\n");
          i += 1;
        } else {
          out_c.write("  fprintf(f, \"%s\", \"" + tok + "\");\n");
        }
      }
      out_c.write("  fprintf(f, \"\\n\");\n");
      out_c.write("  fflush(f);\n");
    }
    out_c.write("}\n");
    def constantArgSplit(arg: String): Array[String] = arg.split('=');
    def isConstantArg(arg: String): Boolean = constantArgSplit(arg).length == 2;
    out_c.write("bool " + c.name + "_t::scan ( FILE* f ) {\n");
    if (Module.scanArgs.length > 0) {
      val format =
        if (Module.scanFormat == "") {
          Module.scanArgs.map(a => "%x").reduceLeft((y,z) => z + y)
        } else {
          Module.scanFormat;
        }
      val toks = splitFormat(format);
      var i = 0;
      for (tok <- toks) {
        if (tok(0) == '%') {
          val nodes = c.keepInputs(Module.scanArgs(i).maybeFlatten)
          for (j <- 0 until nodes.length)
            out_c.write("  str_to_dat(read_tok(f), " + emitRef(nodes(j)) + ");\n");
          i += 1;
        } else {
          out_c.write("  fscanf(f, \"%s\", \"" + tok + "\");\n");
        }
      }
      // out_c.write("  getc(f);\n");
    }
    out_c.write("  return(!feof(f));\n");
    out_c.write("}\n");
    vcd.dumpVCD(c, out_c.write);
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
