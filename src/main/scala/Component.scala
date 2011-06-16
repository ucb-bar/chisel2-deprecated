// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import java.lang.reflect.Modifier._;
import java.io.File;

import scala.math.max;
import Node._;
import Component._;
import Bundle._;
import IOdir._;


object Component {
  var isGenHarness = false;
  var isReportDims = false;
  var scanFormat = "";
  var scanArgs: List[String] = Nil;
  var printFormat = "";
  var printArgs: List[String] = Nil;
  var includeArgs: List[String] = Nil;
  var targetEmulatorRootDir: String = null;
  var targetVerilogRootDir: String = null;
  var targetDir: String = null;
  var compIndex = -1;
  val compIndices = HashMap.empty[String,Int];
  val compDefs = new HashMap[String, String];
  var isEmittingComponents = false;
  var isEmittingC = false;
  var topComponent: Component = null;
  val components = ArrayBuffer[Component]();
  var genCount = 0;
  def genCompName(name: String): String = {
    genCount += 1;
    name + "_" + genCount
  }
  def nextCompIndex : Int = { compIndex = compIndex + 1; compIndex }
  /*def apply (name: String, i: Interface): Component = {
    val res = new Component();
    res.name = name + "_" + nextCompIndex;
    res.ioVal = i;
    if (res.parent != null)
      res.parent.children += res;
    val wires = res.io.flatten;
    for ((n, w) <- wires) {
      w.component = res;
      w match {
        case io: IO => 
          if (io.dir == INPUT)
            res.inputs  += io; 
          else
            res.outputs += io; 
      };
    }
    res
  }* */
  /*def apply (name: String): Component = Component(name, nullBundle);* */
  def splitArg (s: String) = s.split(' ').toList;
  def initChisel () = {
    cond = new Stack[Node];
    isGenHarness = false;
    isReportDims = false;
    scanFormat = "";
    scanArgs = Nil;
    printFormat = "";
    printArgs = Nil;
    isCoercingArgs = true;
    targetEmulatorRootDir = System.getProperty("CHISEL_EMULATOR_ROOT");
    if (targetEmulatorRootDir == null) targetEmulatorRootDir = "../emulator";
    targetVerilogRootDir = System.getProperty("CHISEL_VERILOG_ROOT");
    if (targetVerilogRootDir == null) targetVerilogRootDir = "../verilog";
    targetDir = "";
    compIndex = -1;
    compIndices.clear();
    components.clear();
    isEmittingComponents = false;
    isEmittingC = false;
    topComponent = null;
  }
}

abstract class Component extends Node {
  var ioVal: Interface = null;
  val rules = new ArrayBuffer[Rule];
  val bindings = new ArrayBuffer[Binding];
  var wiresCache: Array[(String, IO)] = null;
  var parent: Component = null;
  var containsReg = false;
  val children = new ArrayBuffer[Component];
  
  val mods  = new ArrayBuffer[Node];
  val omods = new ArrayBuffer[Node];
  val gmods = new ArrayBuffer[Node];
  val regs  = new ArrayBuffer[Node];
  val nexts = new Queue[Node];
  var nindex = -1;
  var defaultWidth = 32;
  var moduleName: String = "";
  var className:  String = "";
  components += this;
  def ownIo() = {
    // println("COMPONENT " + name + " IO " + io);
    val wires = io.flatten;
    for ((n, w) <- wires) {
      // println(">>> " + w + " IN " + this);
      w.component = this;
    }
  }
  def name_it() = {
    val cname  = getClass().getName(); 
    val dotPos = cname.lastIndexOf('.');
    name = if (dotPos >= 0) cname.substring(dotPos+1) else cname;
    className = name;
    if (compIndices contains name) {
      val compIndex = (compIndices(name) + 1);
      compIndices += (name -> compIndex);
      name = name + "_" + compIndex;
    } else {
      compIndices += (name -> 0);
    }
  }
  def findBinding(m: Node): Binding = {
    // println("FINDING BINDING " + m + " OUT OF " + bindings.length + " IN " + this);
    for (b <- bindings) {
      // println("LOOKING AT " + b + " INPUT " + b.inputs(0));
      if (b.inputs(0) == m)
        return b
    }
    // println("UNABLE TO FIND BINDING FOR " + m);
    return null
  }
  //def io: Interface = ioVal;
  def io: Interface
  def nextIndex : Int = { nindex = nindex + 1; nindex }
  def genName (name: String): String = 
    if (name == null || name.length() == 0) "" else name + "_" + nextIndex;
  var isWalking = new HashSet[Node];
  var isWalked = new HashSet[Node];
  override def toString: String = name
  def wires: Array[(String, IO)] = {
    if (wiresCache == null)
      wiresCache = io.flatten;
    wiresCache
  }
  def <>(src: Component) = io <> src.io;
  def apply(name: String): Interface = io(name);
  // COMPILATION OF REFERENCE
  override def emitDec: String = {
    var res = "";
    val wires = io.flatten;
    for ((n, w) <- wires) 
      res += w.emitDec;
    res
  }
  override def emitRefV: String = name;
  override def emitDef: String = {
    var res = "  " + moduleName + " " + name + "(";
    val hasReg = containsReg || childrenContainsReg;
    res = res + (if(hasReg) ".clk(clk), .reset(reset)" else "");
    var isFirst = true;
    var nl = ""
    for ((n, w) <- wires) {
      if (isFirst && !hasReg) {isFirst = false; nl = "\n"} else nl = ",\n";
      res += nl + "       ." + n + "( ";
      w match {
        case io: IO  => 
          if (io.dir == INPUT) {
            if (io.inputs.length == 0)
              println("// " + io + " UNCONNECTED IN " + io.component); 
            else if (io.inputs.length > 1) 
              println("// " + io + " CONNECTED TOO MUCH " + io.inputs.length); 
            else 
              res += io.inputs(0).emitRef;
          } else {
            if (io.consumers.length == 0) 
              println("// " + io + " UNCONNECTED IN " + io.component + " BINDING " + findBinding(io)); 
            else {
              var consumer: Node = parent.findBinding(io);
              if (consumer == null) 
                println("// " + io + "(" + io.component + ") OUTPUT UNCONNECTED (" + io.consumers.length + ") IN " + parent); 
              else 
                res += consumer.emitRef; // TODO: FIX THIS?
            }
          }
      };
      res += " )";
    }
    res += ");\n";
    res
  }
  override def emitDefLoC: String = {
    var res = "";
    for ((n, w) <- wires) {
      w match {
        case io: IO  => 
          if (io.dir == INPUT)
            res += "  " + emitRef + "->" + n + " = " + io.inputs(0).emitRef + ";\n";
      };
    }
    res += emitRef + "->clock_lo(reset);\n";
    for ((n, w) <- wires) {
      w match {
        case io: IO => 
          if (io.dir == OUTPUT)
            res += "  " + io.consumers(0).emitRef + " = " + emitRef + "->" + n + ";\n";
      };
    }
    res
  }
  override def emitDefHiC: String = {
    var res = emitRef + "->clock_hi(reset);\n";
    res
  }
  // COMPILATION OF BODY
  def emitDefs: String = {
    var res = "";
    for (m <- mods) 
      res += m.emitDef;
    for (c <- children) 
      res += c.emitDef;
    res
  }
  def emitRegs: String = {
    var res = "  always @(posedge clk) begin\n";
    for (m <- mods) 
      res += m.emitReg;
    res += "  end\n";
    res
  }
  def emitDecs: String = {
    var res = "";
    for (m <- mods) 
      res += m.emitDec;
    res
  }
  def isInferenceTerminal(m: Node): Boolean = {
    m.isFixedWidth || (
      m match { 
        case io: IO => true; 
        case b: Binding => true; 
        case _ => false }
    )
    /*
    var isAllKnown = true;
    for (i <- m.inputs) {
      if (i.width == -1)
        isAllKnown = false;
    }
    isAllKnown
    */
  }
  def initInference() = {
    for (m <- mods) {
      // if (isInferenceTerminal(m)) {
        // println("ENQUEUE " + m);
      // println("INIT " + m);
        nexts.enqueue(m);
      // }
    }
  }
  def inferAll() = {
    initInference;
    while (!nexts.isEmpty) {
      val next = nexts.dequeue();
      if (next.infer) {
        nexts.enqueue(next);
        for (c <- next.consumers) {
          // println("ENQUEUING " + c);
          nexts.enqueue(c);
        }
        for (i <- next.inputs)
          nexts.enqueue(i);
      }
    }
  }
  def findConsumers() = {
    for (m <- mods) {
      m.addConsumers;
    }
  }
  def findRoots(): ArrayBuffer[Node] = {
    val roots = new ArrayBuffer[Node];
    for (m <- mods) {
      m match {
        case io: IO => if (io.dir == OUTPUT) { if (io.consumers.length == 0) roots += m; }
        case d: Delay  => roots += m;
        case any       =>
      }
    }
    roots
  }
  def findLeaves(): ArrayBuffer[Node] = {
    val leaves = new ArrayBuffer[Node];
    for (m <- mods) {
      m match {
        case io: IO    => if (io.dir == INPUT) { if (io.inputs.length == 0) leaves += m; }
        case l: Lit    => leaves += m;
        case d: Delay  => leaves += m;
        case any       =>
      }
    }
    leaves
  }
  def findOrdering() = {
    val roots = findRoots();
    isWalked.clear();
    for (r <- roots)
      r.visitNode(0);
  }
  def findGraph() = {
    val leaves = findLeaves();
    isWalked.clear();
    for (r <- leaves)
      r.visitNodeRev(0);
  }
  def findGraphDims(): (Int, Int, Int) = {
    var maxDepth = 0;
    val imods = new ArrayBuffer[Node]();
    for (m <- mods) {
      m match {
        case o: IO  =>
        case l: Lit =>
        case i      => imods += m;
      }
    }
    val whist = new HashMap[Int, Int]();
    for (m <- imods) {
      val w = m.width;
      if (whist.contains(w))
        whist(w) = whist(w) + 1;
      else
        whist(w) = 1;
    }
    val hist = new HashMap[String, Int]();
    for (m <- imods) {
      var name = m.getClass().getName();
      m match {
        case m: Mux => name = "Mux";
        case op: Op => name = op.op;
        case o      => name = name.substring(name.indexOf('.')+1);
      }
      if (hist.contains(name))
        hist(name) = hist(name) + 1;
      else
        hist(name) = 1;
    }
    for (m <- imods) 
      maxDepth = max(m.depth, maxDepth);
    // for ((n, c) <- hist) 
    println("%6s: %s".format("name", "count"));
    for (n <- hist.keys.toList.sortWith((a, b) => a < b)) 
      println("%6s: %4d".format(n, hist(n)));
    println("%6s: %s".format("width", "count"));
    for (w <- whist.keys.toList.sortWith((a, b) => a < b)) 
      println("%3d: %4d".format(w, whist(w)));
    var widths = new Array[Int](maxDepth+1);
    for (i <- 0 until maxDepth+1)
      widths(i) = 0;
    for (m <- imods) 
      widths(m.depth) = widths(m.depth) + 1;
    var numNodes = 0;
    for (m <- imods) 
      numNodes += 1;
    var maxWidth = 0;
    for (i <- 0 until maxDepth+1)
      maxWidth = max(maxWidth, widths(i));
    (numNodes, maxWidth, maxDepth)
  }
  def collectNodes(c: Component) = {
    for (m <- c.mods) {
      // println("M " + m.name);
      m match {
        case io: IO  => 
          if (io.dir == INPUT) 
            inputs += m;
          else
            outputs += m;
        case r: Reg    => regs += m;
        case other     =>
      }
    }
  }
  override def findNodes(depth: Int, c: Component): Unit = {
    io.findNodes(depth, c);
  }
  def childrenContainsReg: Boolean = {
    var res = containsReg;
    if(children.isEmpty) return res; 
    for(child <- children){
      res = res || child.containsReg || child.childrenContainsReg;
      if(res) return res;
    }
    res
  }
  def doCompileV(out: java.io.FileWriter, depth: Int): Unit = {
    // println("COMPILING COMP " + name);
    println("// " + depthString(depth) + "COMPILING " + this + " " + children.length + " CHILDREN");
    if (isEmittingComponents) {
      for (top <- children)
        top.doCompileV(out, depth+1);
    } else
      topComponent = this;
    // isWalked.clear();
    findConsumers();
    inferAll();
    collectNodes(this);
    // for (m <- mods) {
    //   println("// " + depthString(depth+1) + " MOD " + m);
    // }
    val hasReg = containsReg || childrenContainsReg;
    //out.write("module " + name + "(" + (if (hasReg) "input clk, input reset" else ""));
    var res = (if (hasReg) "input clk, input reset" else "");
    var first = true;
    var nl = "";
    for ((n, w) <- wires) {
      if(first && !hasReg) {first = false; nl = "\n"} else nl = ",\n";
      w match {
        case io: IO => {
          if (io.dir == INPUT) {
            //out.write(nl + "    input " + io.emitWidth + " " + io.emitRef);
	    res += nl + "    input " + io.emitWidth + " " + io.emitRef;
          } else {
            //out.write(nl + "    output" + io.emitWidth + " " + io.emitRef);
	    res += nl + "    output" + io.emitWidth + " " + io.emitRef;
          }
        }
      };
    }
    //out.write(");\n\n");
    res += ");\n\n";
    // TODO: NOT SURE EXACTLY WHY I NEED TO PRECOMPUTE TMPS HERE
    for (m <- mods)
      m.emitTmp;
    //out.write(emitDecs);
    //out.write("\n");
    //out.write(emitDefs)
    res += emitDecs + "\n" + emitDefs
    // for (o <- outputs)
    //   out.writeln("  assign " + o.emitRef + " = " + o.inputs(0).emitRef + ";");
    if (regs.size > 0) {
      //out.write("\n");
      //out.write(emitRegs)
      res += "\n" + emitRegs;
    }
    //out.write("endmodule\n\n");
    res += "endmodule\n\n";
    if(compDefs contains res){
      moduleName = compDefs(res);
    }else{
      if(compDefs.values.toList contains name) name = genCompName(name);
      compDefs += (res -> name);
      moduleName = name;
      res = "module " + name + "(" + res;
      out.write(res); }
    // println("// " + depthString(depth) + "DONE");
  }
  def markComponent() = {
    name_it();
    ownIo();
    io.name_it("");
    // println("COMPONENT " + name);
    val c = getClass();
    for (m <- c.getDeclaredMethods) {
      val name = m.getName();
      // println("LOOKING FOR " + name);
      val types = m.getParameterTypes();
      if (types.length == 0) {
        val o = m.invoke(this);
        o match { 
	  case comp: Component => { comp.component = this;}
          case node: Node => { if (node.name == "" || node.name == null) node.name = name;
			       if (node.isReg) containsReg = true;
			     }
          case any =>
        }
      }
    }
  }
  def nameChildren() = {
    val childNameCount = new HashMap[String, Int];
    for(child <-children){
      if (childNameCount contains child.className){
	childNameCount(child.className)+=1;
	child.name = child.className + "_" + childNameCount(child.className);
      } else {
	childNameCount += (child.className -> 0)
	child.name = child.className;
      }
    }
  }
  def ensure_dir(dir: String) = {
    val d = dir + (if (dir == "" || dir(dir.length-1) == '/') "" else "/");
    new File(d).mkdirs();
    d
  }

  def compileV(): Unit = {
    topComponent = this;
    for (c <- components) 
      c.markComponent();
    findNodes(0, this);
    for(c <- components)
      c.nameChildren();
    val base_name = ensure_dir(targetVerilogRootDir + "/" + targetDir);
    val out = new java.io.FileWriter(base_name + name + ".v");
    doCompileV(out, 0);
    out.close();
    compDefs.clear;
  }
  def nameAllIO(): Unit = {
    // println("NAMING " + this);
    io.name_it("");
    for (child <- children) 
      child.nameAllIO();
  }
  def genHarness(base_name: String, name: String) = {
    val makefile = new java.io.FileWriter(base_name + name + "-makefile");
    makefile.write("CPPFLAGS = -O2 -I../\n\n");
    makefile.write(name + ": " + name + ".o" + " " + name + "-emulator.o\n");
    makefile.write("\tg++ -o " + name + " " + name + ".o " + name + "-emulator.o\n\n");
    makefile.write(name + ".o: " + name + ".cpp " + name + ".h\n");
    makefile.write("\tg++ -c ${CPPFLAGS} " + name + ".cpp\n\n");
    makefile.write(name + "emulator.o: " + name + "-emulator.cpp " + name + ".h\n");
    makefile.write("\tg++ -c ${CPPFLAGS} " + name + "-emulator.cpp\n\n");
    makefile.close();
    val harness  = new java.io.FileWriter(base_name + name + "-emulator.cpp");
    harness.write("#include \"emulator.h\"\n");
    harness.write("#include \"" + name + ".h\"\n");
    harness.write("int main (int argc, char* argv[]) {\n");
    harness.write("  " + name + "_t* c = new " + name + "_t();\n");
    harness.write("  int lim = (argc > 1) ? atoi(argv[1]) : -1;\n");
    harness.write("  c->init();\n");
    harness.write("  for (int t = 0; lim < 0 || t < lim; t++) {\n");
    harness.write("    dat_t<1> reset = LIT<1>(t == 0);\n");
    harness.write("    c->scan(stdin);\n");
    harness.write("    c->clock_lo(reset);\n");
    harness.write("    c->clock_hi(reset);\n");
    harness.write("    c->print(stdout);\n");
    harness.write("  }\n");
    harness.write("}\n");
    harness.close();
  }
  def compileC(): Unit = {
    for (c <- components) 
      c.markComponent();
    val base_name = ensure_dir(targetEmulatorRootDir + "/" + targetDir);
    val out_h = new java.io.FileWriter(base_name + name + ".h");
    val out_c = new java.io.FileWriter(base_name + name + ".cpp");
    if (isGenHarness)
      genHarness(base_name, name);
    isEmittingC = true;
    println("// COMPILING " + this + "(" + children.length + ")");
    if (isEmittingComponents) {
      io.name_it("");
      for (top <- children)
        top.compileC();
    } else {
      topComponent = this;
    }
    // isWalked.clear();
    findNodes(0, this);
    if (!isEmittingComponents)
      for (c <- components)
        if (!(c == this))
          mods ++= c.mods;
    findConsumers();
    inferAll();
    collectNodes(this);
    findOrdering(); // search from roots  -- create omods
    findGraph();    // search from leaves -- create gmods
    for (m <- omods) {
      m match {
        case l: Lit => ;
        case any    => 
          if (m.name != "" && m != reset && !(m.component == null)) 
            m.name = m.component.name + "_" + m.name;
      }
      // println(">> " + m.name);
    }
    if (isReportDims) {
    val (numNodes, maxWidth, maxDepth) = findGraphDims();
    println("NUM " + numNodes + " MAX-WIDTH " + maxWidth + " MAX-DEPTH " + maxDepth);
    }
    // for (m <- omods)
    //   println("MOD " + m + " IN " + m.component.name);
    out_h.write("class " + name + "_t : public mod_t {\n");
    out_h.write(" public:\n");
    if (isEmittingComponents) {
      for ((n, w) <- wires) 
        out_h.write("  dat_t<" + w.width + "> " + w.emitRef + ";\n");
    }
    for (m <- omods) 
      if (m.isInObject)
        out_h.write(m.emitDecC);
    if (isEmittingComponents) {
      for (c <- children) 
        out_h.write("  " + c.emitRef + "_t* " + c.emitRef + ";\n");
    }
    out_h.write("\n");
    out_h.write("  void init ( void );\n");
    out_h.write("  void clock_lo ( dat_t<1> reset );\n");
    out_h.write("  void clock_hi ( dat_t<1> reset );\n");
    out_h.write("  void print ( FILE* f );\n");
    out_h.write("  void scan ( FILE* f );\n");
    out_h.write("};\n");
    out_h.close();

    out_c.write("#include \"emulator.h\"\n");
    out_c.write("#include \"" + name + ".h\"\n");
    for(str <- includeArgs) out_c.write("#include \"" + str + "\"\n"); 
    out_c.write("\n");
    out_c.write("void " + name + "_t::init ( void ) {\n");
    if (isEmittingComponents) {
      for (c <- children) {
        out_c.write("  " + c.emitRef + " = new " + c.emitRef + "_t();\n");
        out_c.write("  " + c.emitRef + "->init();\n");
      }
    }
    for (m <- omods) {
      out_c.write(m.emitInitC);
    }
    out_c.write("}\n");
    out_c.write("void " + name + "_t::clock_lo ( dat_t<1> reset ) {\n");
    for (m <- omods) {
      out_c.write(m.emitDefLoC);
    }
    // for (c <- children) 
    //   out_c.write("    " + c.emitRef + "->clock_lo(reset);\n");
    out_c.write("}\n");
    out_c.write("void " + name + "_t::clock_hi ( dat_t<1> reset ) {\n");
    for (m <- omods) 
      out_c.write(m.emitDefHiC);
    // for (c <- children) 
    //   out_c.write("    " + c.emitRef + "->clock_hi(reset);\n");
    out_c.write("}\n");
    def splitPrintFormat(s: String) = {
      var off = 0;
      var res: List[String] = Nil;
      for (i <- 0 until s.length) {
        if (s(i) == '%') {
          if (off < i) 
            res = s.substring(off, i) :: res;
          res = "%" :: res;
          off = i + 1;
        }
      }
      if (off < (s.length-1))
        res = s.substring(off, s.length) :: res;
      res.reverse
    }
    out_c.write("void " + name + "_t::print ( FILE* f ) {\n");
    if (printArgs.length > 0) {
      val format =
        if (printFormat == "") printArgs.map(a => "%").reduceLeft((y,z) => z + " " + y) 
        else printFormat;
      val toks = splitPrintFormat(format);
      var i = 0;
      for (tok <- toks) {
        if (tok(0) == '%') {
          out_c.write("  fprintf(f, \"%s\", " + name + "_" + printArgs(i) + ".to_str().c_str());\n");
          i += 1;
        } else {
          out_c.write("  fprintf(f, \"%s\", \"" + tok + "\");\n");
        }
      }
      out_c.write("  fprintf(f, \"\\n\");\n");
    }
    out_c.write("}\n");
    def constantArgSplit(arg: String) = arg.split('=');
    def isConstantArg(arg: String) = constantArgSplit(arg).length == 2;
    out_c.write("void " + name + "_t::scan ( FILE* f ) {\n");
    if (scanArgs.length > 0) {
      val format = 
        if (scanFormat == "") {
          var res = "";
          for (arg <- scanArgs) {
            if (!isConstantArg(arg)) {
              if (res.length > 0) res = res + " ";
              res = res + "%llx";
            }
          }
          res
        } else 
          scanFormat;
      out_c.write("  fscanf(f, \"" + format + "\"");
      for (arg <- scanArgs) {
        if (!isConstantArg(arg))
          out_c.write(",  &" + name + "_" + arg + ".values[0]");
      }
      out_c.write(");\n");
      for (arg <- scanArgs) {
        val argParts = constantArgSplit(arg);
        if (argParts.length == 2)
          out_c.write("  " + name + "_" + argParts(0) + ".values[0] = " + 
                      argParts(1).substring(0, argParts(1).length) + ";\n");
      }
    }
    out_c.write("}\n");
    out_c.close();
  }
};

}
