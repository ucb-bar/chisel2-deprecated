package Chisel
import scala.collection.mutable.ArrayBuffer
import scala.math.max;
import java.io.File;
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import scala.sys.process._
import Node._
import Reg._
import ChiselError._
import Component._
import Literal._

class CppBackend extends Backend {
  override def emitTmp(node: Node): String = {
    if (node.isInObject)
      emitRef(node)
    else
      "dat_t<" + node.width + "> " + emitRef(node)
  }

  override def emitRef(node: Node): String = {
    node match {
      case l: Literal =>
        (if (l.isBinary) { 
          var (bits, mask, swidth) = parseLit(l.name);
           var bwidth = if(l.base == 'b') l.width else swidth;
           if (l.isZ) {
             ("LITZ<" + bwidth + ">(0x" + toHex(bits) + ", 0x" + toHex(mask) + ")")
           } else
             ("LIT<" + bwidth + ">(0x" + toHex(bits) + ")")
         } else if (l.base == 'd' || l.base == 'x'){
           ("LIT<" + l.width + ">(" + l.name + "L)")
         } else
           ("LIT<" + l.width + ">(0x" + l.name + "L)")
        ) // + "/*" + l.inputVal + "*/";

      case x: Reg =>
        if(isHiC) super.emitRef(node) + "_shadow_out" else super.emitRef(node)

      case x: Binding =>
        emitRef(x.inputs(0))

      case x: Bits => 
        if (!node.isInObject && node.inputs.length == 1) emitRef(node.inputs(0)) else super.emitRef(node) 

      case _ =>
        super.emitRef(node)
    }
  }

  override def emitDec(node: Node): String = {
    node match {
      case x: Binding =>
        ""
      case x: Literal =>
        ""
      case x: Lit =>
        ""
      case x: ListNode =>
        ""
      case x: MapNode =>
        ""
      case x: LookupMap =>
        ""
      case x: Reg =>
        "  dat_t<" + node.width + "> " + emitRef(node) + ";\n" +
        "  dat_t<" + node.width + "> " + emitRef(node) + "_shadow;\n";
      case m: Mem[_] =>
        "  mem_t<" + m.width + "," + m.n + "> " + emitRef(m) + ";\n"
      case r: ROM[_] =>
        "  mem_t<" + r.width + "," + r.lits.length + "> " + emitRef(r) + ";\n"
      case _ =>
        "  dat_t<" + node.width + "> " + emitRef(node) + ";\n"
    }
  }

  def emitOpRef (o: Op, k: Int): String = {
    if (o.op == "<<") {
      if (k == 0 && o.inputs(k).width < o.width)
	"DAT<" + o.width + ">(" + emitRef(o.inputs(k)) + ")"
      else
	emitRef(o.inputs(k))
    } else if (o.op == "##" || o.op == ">>" || o.op == "*" ||
             o.op == "s*s" || o.op == "u*s" || o.op == "s*u") {
      emitRef(o.inputs(k))
    } else {
      var w = 0;
      for (i <- 0 until o.nGrow)
	w = max(w, o.inputs(i).width);
      if (isCoercingArgs && o.nGrow > 0 && k < o.nGrow && w > o.inputs(k).width)
	"DAT<" + w + ">(" + emitRef(o.inputs(k)) + ")"
      else
	emitRef(o.inputs(k))
    }
  }

  def emitDefLo(node: Node): String = {
    node match {
      case x: Mux =>
        "  " + emitTmp(x) + " = mux<" + x.width + ">(" + emitRef(x.inputs(0)) + ", " + emitRef(x.inputs(1)) + ", " + emitRef(x.inputs(2)) + ");\n"

      case o: Op => {
        "  " + emitTmp(node) + " = " +
          (if (o.op == "##") 
            "cat<" + node.width + ">(" + emitOpRef(o, 0) + ", " + emitOpRef(o, 1) + ")"
           else if (o.op == "s*s")
             emitRef(o.inputs(0)) + ".fix_times_fix(" + emitRef(o.inputs(1)) + ")"
           else if (o.op == "s*u")
             emitRef(o.inputs(0)) + ".fix_times_ufix(" + emitRef(o.inputs(1)) + ")"
           else if (o.op == "u*s")
             emitRef(o.inputs(0)) + ".ufix_times_fix(" + emitRef(o.inputs(1)) + ")"
           else if (o.inputs.length == 1)
             if (o.op == "|")
               "reduction_or(" + emitRef(o.inputs(0)) + ")"
             else if (o.op == "&")
               "reduction_and(" + emitRef(o.inputs(0)) + ")"
             else if (o.op == "^")
               "reduction_xor(" + emitRef(o.inputs(0)) + ")"
             else
               o.op + emitRef(o.inputs(0))
           else if(o.isSigned) {
             if(o.op == ">>")
               emitOpRef(o, 0) + ".rsha(" + emitOpRef(o, 1) + ")"
             else if(o.op == ">")
               emitOpRef(o, 0) + ".gt(" + emitOpRef(o, 1) + ")"
             else if(o.op == ">=")
               emitOpRef(o, 0) + ".gte(" + emitOpRef(o, 1) + ")"
             else if(o.op == "<")
               emitOpRef(o, 0) + ".lt(" + emitOpRef(o, 1) + ")"
             else if(o.op == "<=")
               emitOpRef(o, 0) + ".lt(" + emitOpRef(o, 1) + ")"
             else 
               emitOpRef(o, 0) + " " + o.op + " " + emitOpRef(o, 1)
           } else
             emitOpRef(o, 0) + " " + o.op + " " + emitOpRef(o, 1)) + 
        ";\n"
      }

      case x: Extract =>
        x.inputs.tail.foreach(e => x.validateIndex(e))
        if (node.inputs.length < 3 )
          "  " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ".bit(" + emitRef(node.inputs(1)) + ");\n"
        else{
          "  " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ".extract<" + node.width + ">(" + emitRef(node.inputs(1)) + "," + emitRef(node.inputs(2)) + ");\n"}

      case x: Fill =>
        if (node.inputs(1).isLit)
          "  " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ".fill<" + node.width + "," + node.inputs(1).value + ">();\n";
        else
          "  " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ".fill<" + node.width + ">(" + emitRef(node.inputs(1)) + ");\n";

      case ll: ListLookup[_] =>
        var res = "";
        var isFirst = true;
        for (w <- ll.wires)
          if(w.component != null) // TODO: WHY IS COMPONENT EVER NULL?
            res = res + "  dat_t<" + w.width + "> " + emitRef(w) + ";\n";
        for ((addr, data) <- ll.map) {
          res = res + "  " + (if (isFirst) { isFirst = false; "" } else "else ");
          res = res + "if ((" + emitRef(addr) + " == " + emitRef(ll.inputs(0)) + ").to_bool()) {\n";
          for ((w, e) <- ll.wires zip data)
            if(w.component != null)
              res = res + "    " + emitRef(w) + " = " + emitRef(e) + ";\n";
          res = res + "  }\n";
        }
        res = res + "  else {\n";
        for ((w, e) <- ll.wires zip ll.defaultWires)
          if(w.component != null)
            res = res + "    " + emitRef(w) + " = " + emitRef(e) + ";\n";
        res = res + "  }\n";
        res

      case l: Lookup =>
        var res = "";
        for (node <- l.map) 
          res = res +
            "  if ((" + emitRef(node.addr) + " == " + emitRef(l.inputs(0)) + ").to_bool()) " + emitRef(l) + " = " + emitRef(node.data) + ";\n";
        res
        
      case x: Bits =>
        if (node.isInObject && node.inputs.length == 1)
          "  " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ";\n"
        else if (node.inputs.length == 0 && !node.isInObject) {
          "  " + emitTmp(node) + ";\n"
        } else
          ""

      case m: MemRead[_] =>
        "  " + emitTmp(m) + " = " + emitRef(m.mem) + ".get(" + emitRef(m.addr) + ");\n"

      case r: ROMRead[_] =>
        "  " + emitTmp(r) + " = " + emitRef(r.rom) + ".get(" + emitRef(r.addr) + ");\n"

      case reg: Reg =>
        val updateLogic = 
          (if (reg.isReset) "mux<" + reg.width + ">(" + emitRef(reg.inputs.last) + ", " + emitRef(reg.resetVal) + ", " else "") + 
        emitRef(reg.updateVal) + (if (reg.isReset) ");\n" else ";\n");
        "  " + emitRef(reg) + "_shadow = " +  updateLogic;

      case x: Log2 =>
        " " + emitTmp(x) + " = " + emitRef(x.inputs(0)) + ".log2<" + x.width + ">();\n";

      case _ =>
        ""
    }
  }

  def emitDefHi(node: Node): String = {
    node match {
      case m: MemWrite[_] =>
        if (m.inputs.length == 2)
          return ""
        isHiC = true
        var res = "  if (" + emitRef(m.cond) + ".to_bool()) {\n"
        if (m.isMasked)
          res += "    " + emitRef(m.mem) + ".put(" + emitRef(m.addr) + ", (" + emitRef(m.data) + " & " + emitRef(m.wmask) + ") | (" + emitRef(m.mem) + ".get(" + emitRef(m.addr) + ") & ~" + emitRef(m.wmask) + "));\n"
        else
          res += "    " + emitRef(m.mem) + ".put(" + emitRef(m.addr) + ", " + emitRef(m.data) + ");\n"
        res += "  }\n"
        isHiC = false
        res

      case reg: Reg =>
        "  " + emitRef(reg) + " = " + emitRef(reg) + "_shadow;\n"
      case _ =>
        ""
    }
  }

  def emitInit(node: Node): String = {
    node match {
      case x: Reg =>
        "  " + emitRef(node) + " = random_initialization ? dat_t<" + node.width + ">::rand() : LIT<" + node.width + ">(0);\n"

      case x: Mem[_] =>
        "  if (random_initialization) " + emitRef(node) + ".randomize();\n"

      case r: ROM[_] =>
        r.lits.zipWithIndex.map { case (lit, i) =>
          "  " + emitRef(r) + ".put(" + i + ", " + emitRef(lit) + ");\n"
        }.reduceLeft(_ + _)

      case _ =>
        ""
    }
  }

  def emitInitHi(node: Node): String = {
    node match {
      case x: Reg =>
        "  dat_t<" + node.width + "> " + emitRef(node) + "_shadow_out = " + emitRef(node) + ";\n"

      case _ =>
        ""
    }
  }

  def genHarness(c: Component, base_name: String, name: String) = {
    // val makefile = new java.io.FileWriter(base_name + name + "-makefile");
    // makefile.write("CPPFLAGS = -O2 -I../ -I${CHISEL}/csrc\n\n");
    // makefile.write(name + ": " + name + ".o" + " " + name + "-emulator.o\n");
    // makefile.write("\tg++ -o " + name + " " + name + ".o " + name + "-emulator.o\n\n");
    // makefile.write(name + ".o: " + name + ".cpp " + name + ".h\n");
    // makefile.write("\tg++ -c ${CPPFLAGS} " + name + ".cpp\n\n");
    // makefile.write(name + "emulator.o: " + name + "-emulator.cpp " + name + ".h\n");
    // makefile.write("\tg++ -c ${CPPFLAGS} " + name + "-emulator.cpp\n\n");
    // makefile.close();
    val harness  = new java.io.FileWriter(base_name + name + "-emulator.cpp");
    harness.write("#include \"" + name + ".h\"\n");
    harness.write("int main (int argc, char* argv[]) {\n");
    harness.write("  " + name + "_t* c = new " + name + "_t();\n");
    harness.write("  int lim = (argc > 1) ? atoi(argv[1]) : -1;\n");
    harness.write("  c->init();\n");
    if (isVCD)
      harness.write("  FILE *f = fopen(\"" + name + ".vcd\", \"w\");\n");
    harness.write("  for(int i = 0; i < 5; i++) {\n")
    harness.write("    dat_t<1> reset = LIT<1>(1);\n")
    harness.write("    c->clock_lo(reset);\n")
    harness.write("    c->clock_hi(reset);\n")
    harness.write("  }\n")
    harness.write("  for (int t = 0; lim < 0 || t < lim; t++) {\n");
    harness.write("    dat_t<1> reset = LIT<1>(0);\n");
    harness.write("    if (!c->scan(stdin)) break;\n");
    harness.write("    c->clock_lo(reset);\n");
    harness.write("    c->print(stdout);\n");
    harness.write("    c->clock_hi(reset);\n");
    if (isVCD)
      harness.write("    c->dump(f, t);\n");
    harness.write("  }\n");
    harness.write("}\n");
    harness.close();
  }

  def gcc(c: Component, flags: String = "-O2"): Unit = {
    val chiselENV = java.lang.System.getenv("CHISEL")
    val allFlags = flags + " -I../ -I" + chiselENV + "/csrc/"
    val dir = targetDir + "/"
    def run(cmd: String) = {
      val c = Process(cmd).!
      println(cmd + " RET " + c)
    }
    def link(name: String) = {
      val ac = "g++ -o " + dir + name + " " + dir + name + ".o " + dir + name + "-emulator.o"
      run(ac)
    }
    def cc(name: String) = {
      val cmd = "g++ -c -o " + dir + name + ".o " + allFlags + " " + dir + name + ".cpp"
      run(cmd)
    }
    cc(c.name + "-emulator")
    cc(c.name)
    link(c.name)
  }

  def emitDefLos(c: Component): String = {
    var res = "";
    for ((n, w) <- c.wires) {
      w match {
        case io: Bits  => 
          if (io.dir == INPUT)
            res += "  " + emitRef(c) + "->" + n + " = " + emitRef(io.inputs(0)) + ";\n";
      };
    }
    res += emitRef(c) + "->clock_lo(reset);\n";
    for ((n, w) <- c.wires) {
      w match {
        case io: Bits => 
          if (io.dir == OUTPUT)
            res += "  " + emitRef(io.consumers(0)) + " = " + emitRef(c) + "->" + n + ";\n";
      };
    }
    res
  }

  def emitDefHis(c: Component): String = {
    var res = emitRef(c) + "->clock_hi(reset);\n";
    res
  }

  def renameNodes(c: Component, nodes: Seq[Node]) = {
    for (m <- nodes) {
      m match {
        case l: Literal => ;
        case any        => 
          if (m.name != "" && !(m == m.component.reset) && !(m.component == null)) {
	    // only modify name if it is not the reset signal or not in top component
	    if(m.name != "reset" || !(m.component == c)) 
	      m.name = m.component.getPathName + "__" + m.name;
	  }
      }
    }
  }

  override def compile(c: Component): Unit = {
    val vcd = new VcdBackend()
    val dot = new DotBackend()
    components.foreach(_.elaborate(0));
    for (c <- components)
      c.markComponent();
    c.genAllMuxes;
    components.foreach(_.postMarkNet(0));
    val base_name = ensure_dir(targetDir)
    val out_h = new java.io.FileWriter(base_name + c.name + ".h");
    val out_c = new java.io.FileWriter(base_name + c.name + ".cpp");
    println("// COMPILING " + c + "(" + c.children.length + ")");
    topComponent = c;
    assignResets()
    c.removeTypeNodes()
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors)	err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length + " ERRORS");
      return
    }
    c.inferAll();
    if(saveWidthWarnings)
      widthWriter = new java.io.FileWriter(base_name + c.name + ".width.warnings")
    c.forceMatchingWidths;
    c.traceNodes();
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors)	err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length + " ERRORS");
      return
    }
    if(!dontFindCombLoop) c.findCombLoop();
    for (cc <- components) {
      if (!(cc == c)) {
        c.mods       ++= cc.mods;
        c.asserts    ++= cc.asserts;
        c.blackboxes ++= cc.blackboxes;
        c.debugs     ++= cc.debugs;
      }
    }
    c.findConsumers();
    c.verifyAllMuxes;
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors)	err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length + " ERRORS");
      return
    }
    c.collectNodes(c);
    c.findOrdering(); // search from roots  -- create omods
    renameNodes(c, c.omods);
    if (isReportDims) {
      val (numNodes, maxWidth, maxDepth) = c.findGraphDims();
      println("NUM " + numNodes + " MAX-WIDTH " + maxWidth + " MAX-DEPTH " + maxDepth);
    }

    if (isGenHarness)
      genHarness(c, base_name, c.name);
    out_h.write("#include \"emulator.h\"\n\n");
    out_h.write("class " + c.name + "_t : public mod_t {\n");
    out_h.write(" public:\n");
    if (isTesting && tester != null) {
      scanArgs.clear();  scanArgs  ++= tester.testInputNodes;    scanFormat  = ""
      printArgs.clear(); printArgs ++= tester.testNonInputNodes; printFormat = ""

      for (n <- scanArgs ++ printArgs)
        if(!c.omods.contains(n)) c.omods += n
    } 
    for (m <- c.omods) {
      if(m.name != "reset") /* && !(m.component == c) */{
        if (m.isInObject)
          out_h.write(emitDec(m));
        if (m.isInVCD)
          out_h.write(vcd.emitDec(m));
      }
    }
    out_h.write("\n");
    out_h.write("  void init ( bool random_initialization = false );\n");
    out_h.write("  void clock_lo ( dat_t<1> reset );\n");
    out_h.write("  void clock_hi ( dat_t<1> reset );\n");
    out_h.write("  void print ( FILE* f );\n");
    out_h.write("  bool scan ( FILE* f );\n");
    out_h.write("  void dump ( FILE* f, int t );\n");
    out_h.write("};\n");
    out_h.close();

    out_c.write("#include \"" + c.name + ".h\"\n");
    for(str <- includeArgs) out_c.write("#include \"" + str + "\"\n"); 
    out_c.write("\n");
    out_c.write("void " + c.name + "_t::init ( bool random_initialization ) {\n");
    for (m <- c.omods) {
      out_c.write(emitInit(m));
    }
    out_c.write("}\n");

    out_c.write("void " + c.name + "_t::clock_lo ( dat_t<1> reset ) {\n");
    for (m <- c.omods) {
      out_c.write(emitDefLo(m));
    }
    for (a <- c.asserts) {
      out_c.write("  ASSERT(" + emitRef(a.cond) + ", \"" + a.message + "\");\n");
    }
    out_c.write("}\n");
    out_c.write("void " + c.name + "_t::clock_hi ( dat_t<1> reset ) {\n");
    for (r <- c.omods) 
      out_c.write(emitInitHi(r));
    for (m <- c.omods) 
      out_c.write(emitDefHi(m));
    out_c.write("}\n");
    def splitFormat(s: String) = {
      var off = 0;
      var res: List[String] = Nil;
      for (i <- 0 until s.length) {
        if (s(i) == '%') {
          if (off < i) 
            res = s.substring(off, i) :: res;
          res = "%" :: res;
          if (i == (s.length-1)) {
            println("Badly formed format argument kind: %");
          } else if (s(i+1) != 'x') {
            println("Unsupported format argument kind: %" + s(i+1));
          } 
          off = i + 2;
        }
      }
      if (off < (s.length-1))
        res = s.substring(off, s.length) :: res;
      res.reverse
    }
    out_c.write("void " + c.name + "_t::print ( FILE* f ) {\n");
    if (printArgs.length > 0) {
      val format =
        if (printFormat == "") printArgs.map(a => "%x").reduceLeft((y,z) => z + " " + y) 
        else printFormat;
      val toks = splitFormat(format);
      var i = 0;
      for (tok <- toks) {
        if (tok(0) == '%') {
          val nodes = printArgs(i).maybeFlatten
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
    def constantArgSplit(arg: String) = arg.split('=');
    def isConstantArg(arg: String) = constantArgSplit(arg).length == 2;
    out_c.write("bool " + c.name + "_t::scan ( FILE* f ) {\n");
    if (scanArgs.length > 0) {
      val format =
        if (scanFormat == "") scanArgs.map(a => "%x").reduceLeft((y,z) => z + y) 
        else scanFormat;
      val toks = splitFormat(format);
      var i = 0;
      for (tok <- toks) {
        if (tok(0) == '%') {
          val nodes = c.keepInputs(scanArgs(i).maybeFlatten)
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
    vcd.dumpVCD(c, out_c);
    out_c.close();
    if(saveComponentTrace)
      printStack
  }

}
