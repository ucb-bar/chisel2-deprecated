package Chisel
import Node._
import java.io.File;
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import scala.sys.process._
import Reg._
import ChiselError._
import Component._
import scala.collection.mutable.HashSet

object VerilogBackend {

  val keywords = new HashSet[String]()
  keywords ++= List("always", "and", "assign", "attribute", "begin", "buf", "bufif0", "bufif1", "case",
                    "casex", "casez", "cmos", "deassign", "default", "defparam", "disable", "edge",
                    "else", "end", "endattribute", "endcase", "endfunction", "endmodule", "endprimitive",
                    "endspecify", "endtable", "endtask", "event", "for", "force", "forever", "fork",
                    "function", "highz0", "highz1", "if", "ifnone", "initial", "inout", "input", 
                    "integer", "join", "medium", "module", "large", "macromodule", "nand", "negedge",
                    "nmos", "nor", "not", "notif0", "notif1", "or", "output", "parameter", "pmos", 
                    "posedge", "primitive", "pull0", "pull1", "pulldown", "pullup", "rcmos", "real",
                    "realtime", "reg", "release", "repeat", "rnmos", "rpmos", "rtran", "rtranif0",
                    "rtranif1", "scalared", "signed", "small", "specify", "specparam", "strength",
                    "strong0", "strong1", "supply0", "supply1", "table", "task", "time", "tran",
                    "tranif0", "tranif1", "tri", "tri0", "tri1", "triand", "trior", "trireg", "unsigned",
                    "vectored", "wait", "wand", "weak0", "weak1", "while", "wire", "wor", "xnor", "xor"
                 )
}

class VerilogBackend extends Backend {
  isEmittingComponents = true
  isCoercingArgs = false

  def emitWidth(node: Node): String = 
    if (node.width == 1) "" else "[" + (node.width-1) + ":0]"

  override def emitTmp(node: Node): String = 
    emitRef(node)

  override def emitRef(node: Node): String = {
    node match {
      case x: Literal =>
        (if (x.width == -1) x.name 
        else if(x.isBinary) ("" + x.width + "'b" + x.name)
        else if(x.base == 'x') ("" + x.width + "'h" + x.name.substring(2, x.name.length))
        else if(x.base == 'd') ("" + x.width + "'d" + x.name)
        else if(x.base == 'h') ("" + x.width + "'h" + x.name)
        else "") + "/* " + x.inputVal + "*/";

      case _ =>
        super.emitRef(node)
    }
  }

  def emitPortDef(m: MemAccess, idx: Int): String = {
    m match {
      case r: MemRead[_] =>
        "    .A" + idx + "(" + emitRef(r.addr) + "),\n" +
        "    .CS" + idx + "(" + emitRef(r.cond) + "),\n" +
        "    .O" + idx + "(" + emitTmp(r) + ")"

      case w: MemWrite[_] =>
        var we = "1'b1"
        var a = emitRef(w.addr)
        var cs = emitRef(w.cond)
        var res = ""

        if (w.isRW) {
          we = emitRef(w.emitRWEnable(w.pairedRead).get)
          cs = cs + " || " + emitRef(w.pairedRead.cond)
          if (w.addr != w.pairedRead.addr)
            a = we + " ? " + a + " : " + emitRef(w.pairedRead.addr)
          res += "    .O" + idx + "(" + emitTmp(w.pairedRead) + "),\n"
        }
        if (w.isMasked)
          res += "    .WBM" + idx + "(" + emitRef(w.wmask) + "),\n"

        res +
        "    .A" + idx + "(" + a + "),\n" +
        "    .WE" + idx + "(" + we + "),\n" +
        "    .CS" + idx + "(" + cs + "),\n" +
        "    .I" + idx + "(" + emitRef(w.data) + ")"
    }
  }

  def emitDef(c: Component): String = {
    val spacing = (if(c.verilog_parameters != "") " " else "");
    var res = "  " + c.moduleName + " " + c.verilog_parameters+ spacing + c.instanceName + "(";
    val hasReg = c.containsReg || c.childrenContainsReg;
    res = res + (if(hasReg) ".clk(clk), .reset(" + (if(c.reset.inputs.length==0) "reset" else emitRef(c.reset.inputs(0))) + ")" else "");
    var isFirst = true;
    var nl = ""
    for ((n, w) <- c.wires) {
      if(n != "reset") {
        if (isFirst && !hasReg) { isFirst = false; nl = "\n" } else nl = ",\n";
        res += nl + "       ." + n + "( ";
        //if(w.isInstanceOf[IO]) println("WALKED TO " + w + ": " + w.walked);
        //if(w.isInstanceOf[IO])
        //println("COMP WALKED " + w + " is " + c.isWalked.contains(w));
        w match {
          case io: Bits  => 
            if (io.dir == INPUT) {
              if (io.inputs.length == 0) { 
                  if(saveConnectionWarnings)
                    connWriter.write("// " + io + " UNCONNECTED IN " + io.component + "\n"); 
              } else if (io.inputs.length > 1) {
                  if(saveConnectionWarnings)
                    connWriter.write("// " + io + " CONNECTED TOO MUCH " + io.inputs.length + "\n"); 
              } else if (!c.isWalked.contains(w)){ 
                  if(saveConnectionWarnings)
                    connWriter.write("// UNUSED INPUT " +io+ " OF " + c + " IS REMOVED" + "\n");
              } else {
                res += emitRef(io.inputs(0));
              }
            } else if(io.dir == OUTPUT) {
              if (io.consumers.length == 0) {
                  if(saveConnectionWarnings)
                    connWriter.write("// " + io + " UNCONNECTED IN " + io.component + " BINDING " + c.findBinding(io) + "\n"); 
              } else {
                var consumer: Node = c.parent.findBinding(io);
                if (consumer == null) {
                  if(saveConnectionWarnings)
                    connWriter.write("// " + io + "(" + io.component + ") OUTPUT UNCONNECTED (" + io.consumers.length + ") IN " + c.parent + "\n"); 
                } else {
                  res += emitRef(consumer); // TODO: FIX THIS?
                }
              }
            }
        };
        res += " )";
      }
    }
    res += ");\n";
    res
  }

  override def emitDef(node: Node): String = {
    def getPathName[T <: Data](m: Mem[T]) = m.component.getPathName + "_" + emitRef(m)
    node match {
      case x: Bits =>
        if (x.dir == INPUT)
          ""
        else {
          if (node.inputs.length == 0) {
            println("// UNCONNECTED " + node + " IN " + node.component); ""
          } else if (node.inputs(0) == null) {
            println("// UNCONNECTED WIRE " + node + " IN " + node.component); ""
          } else
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ";\n" 
        }

      case x: Mux =>
        "  assign " + emitTmp(x) + " = " + emitRef(x.inputs(0)) + " ? " + emitRef(x.inputs(1)) + " : " + emitRef(x.inputs(2)) + ";\n"

      case o: Op =>
        val c = o.component;
        "  assign " + emitTmp(o) + " = " + 
          (if (o.op == "##") 
            "{" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + "}"
           else if (node.inputs.length == 1)
             o.op + " " + emitRef(node.inputs(0))
           else if (o.op == "s*s")
             "$signed(" + emitRef(node.inputs(0)) + ") * $signed(" + emitRef(node.inputs(1)) + ")"
           else if (o.op == "s*u")
             "$signed(" + emitRef(node.inputs(0)) + ") * " + emitRef(node.inputs(1))
           else if (o.op == "u*s")
             emitRef(node.inputs(0)) + " * $signed(" + emitRef(node.inputs(1)) + ")"
           else if(node.isSigned) {
             if (o.op == ">>")
               "$signed(" + emitRef(node.inputs(0)) +") " + ">>>" + " " + emitRef(node.inputs(1))
             else
               "$signed(" + emitRef(node.inputs(0)) +") " + o.op + " $signed(" + emitRef(node.inputs(1)) + ")"
           } else
             emitRef(node.inputs(0)) + " " + o.op + " " + emitRef(node.inputs(1))
          ) + ";\n"

      case x: Cat =>
        var res = "  assign " + emitTmp(node) + " = {";
        var first = true;
        for(e <- node.inputs)
          res += (if(first) {first = false; ""} else ", ") + emitRef(e);
        res += "};\n";
        res

      case x: Extract =>
        node.inputs.tail.foreach(x.validateIndex)
        if (node.inputs.length < 3) {
          if(node.inputs(0).width > 1) {
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + "[" + emitRef(node.inputs(1)) + "];\n"
          } else {
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ";\n"
          }
        } else {
          if(node.inputs(0).width > 1) {
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + "[" + emitRef(node.inputs(1)) + ":" + emitRef(node.inputs(2)) + "];\n"
          } else {
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ";\n"
          }
        }

      case x: Fill =>
        "  assign " + emitTmp(node) + " = {" + emitRef(node.inputs(1)) + "{" + emitRef(node.inputs(0)) + "}};\n";

      case ll: ListLookup[_] =>
        val res = new StringBuilder()
        res.append("  always @(*) begin\n" +
                   //"    " + emitRef + " = " + inputs(1).emitRef + ";\n" +
                   "    casez (" + emitRef(node.inputs(0)) + ")" + "\n");

        for ((addr, data) <- ll.map) {
          res.append("      " + emitRef(addr) + " : begin\n");
          for ((w, e) <- ll.wires zip data) 
            if(w.component != null)
              res.append("        " + emitRef(w) + " = " + emitRef(e) + ";\n");
          res.append("      end\n")
        }
        res.append("      default: begin\n")
        for ((w, e) <- ll.wires zip ll.defaultWires) {
          if(w.component != null)
            res.append("        " + emitRef(w) + " = " + emitRef(e) + ";\n");
        }
        res.append("      end\n");
        res.append( 
          "    endcase\n" +
          "  end\n");
        res.toString

      case l: Lookup =>
        var res = 
          "  always @(*) begin\n" +
          "    " + emitRef(l) + " = " + emitRef(l.inputs(1)) + ";\n" +
          "    casez (" + emitRef(l.inputs(0)) + ")" + "\n";

        for (node <- l.map) 
          res = res +
            "      " + emitRef(node.addr) + " : " + emitRef(l) + " = " + emitRef(node.data) + ";\n";
        res = res + 
          "    endcase\n" +
          "  end\n";
        res

      case m: Mem[_] =>
        if (isInlineMem)
          return ""

        m.reads.filter(r => r.used && r.getPortType == "read").foreach { r =>
          val pairedWrite = m.writes.find(w => w.used && w.isPossibleRW(r))
          if (!pairedWrite.isEmpty) {
            pairedWrite.get.setRW(r)
            m.ports -= r
          }
        }

        val usedports = m.ports.filter(_.used)
        val portdefs = usedports.zipWithIndex.map { case (p, i) => emitPortDef(p, i) }

        Component.configStr +=
          "name " + moduleNamePrefix+getPathName(m) +
          " depth " + m.n +
          " width " + m.width +
          " ports " + usedports.map(_.getPortType).reduceLeft(_ + "," + _) +
          "\n"

        val clkrst = Array("    .CLK(clk)", "    .RST(reset)")
        "  " + moduleNamePrefix+getPathName(m) + " " + emitRef(m) + " (\n" +
        (clkrst ++ portdefs).reduceLeft(_ + ",\n" + _) + "\n" +
        "  );\n"

        
      case m: MemRead[_] =>
        if (isInlineMem)
          "  assign " + emitTmp(node) + " = " + emitRef(m.mem) + "[" + emitRef(m.addr) + "];\n"
        else
          ""

      case r: ROM[_] =>
        val inits = r.lits.zipWithIndex.map { case (lit, i) =>
          "    " + emitRef(r) + "[" + i + "] = " + emitRef(lit) + ";\n"
        }

        "  initial begin\n" +
        inits.reduceLeft(_ + _) +
        "  end\n"

      case r: ROMRead[_] =>
        "  assign " + emitTmp(r) + " = " + emitRef(r.rom) + "[" + emitRef(r.addr) + "];\n"
        
      case _ =>
        ""
    }
  }

  def emitDecBase(node: Node): String = 
    "  wire" + (if (node.isSigned) " signed " else "") + emitWidth(node) + " " + emitRef(node) + ";\n"

  override def emitDec(node: Node): String = {
    if (node.isInstanceOf[Bundle]) println("found")

    node match {
      case x: Bits =>
        if(x.dir == null)
          emitDecBase(node)
        else
          ""

      case x: ListLookupRef[_] =>
        "  reg[" + (node.width-1) + ":0] " + emitRef(node) + ";\n";

      case x: Lookup =>
        "  reg[" + (node.width-1) + ":0] " + emitRef(node) + ";\n";

      case x: ListNode =>
        ""
      case x: MapNode =>
        ""
      case x: LookupMap =>
        ""
      case x: Literal =>
        ""

      case x: Reg =>
        if (node.isMemOutput)
          ""
        else
          "  reg[" + (node.width-1) + ":0] " + emitRef(node) + ";\n"

      case m: Mem[_] =>
        if (isInlineMem)
          "  reg [" + (m.width-1) + ":0] " + emitRef(m) + " [" + (m.n-1) + ":0];\n"
        else
          ""

      case r: ROM[_] =>
        "  reg [" + (r.width-1) + ":0] " + emitRef(r) + " [" + (r.lits.length-1) + ":0];\n"

      case x: MemAccess =>
        x.referenced = true
        emitDecBase(node)

      case _ =>
        emitDecBase(node)
    }
  }   

  def genHarness(c: Component, base_name: String, name: String) = {
    val harness  = new java.io.FileWriter(base_name + name + "-harness.v");
    val printFormat = printArgs.map(a => "0x%x").fold("")((y,z) => z + " " + y)
    val scanFormat = scanArgs.map(a => "%x").fold("")((y,z) => z + " " + y)
    val printNodes = for (arg <- printArgs; node <- arg.maybeFlatten) yield arg
    val scanNodes = for (arg <- scanArgs; node <- c.keepInputs(arg.maybeFlatten)) yield arg
    harness.write("module test;\n")
    for (node <- scanNodes)
      harness.write("    reg [" + (node.width-1) + ":0] " + emitRef(node) + ";\n")
    for (node <- printNodes)
      harness.write("     wire [" + (node.width-1) + ":0] " + emitRef(node) + ";\n")

    harness.write("  reg clk = 0;\n")
    harness.write("  reg reset = 1;\n\n")
    harness.write("  initial begin\n")
    harness.write("    reset = 1;\n")
    harness.write("    #250 reset = 0;\n")
    harness.write("  end\n\n")

    harness.write("  always #100 clk = ~clk;\n")
    
    harness.write("    " + c.moduleName + "\n")
    harness.write("      " + c.moduleName + "(\n")

    if(c.containsReg || c.childrenContainsReg) {
      harness.write("        .clk(clk),\n")
      harness.write("        .reset(reset),\n")
    }

    var first = true
    for (node <- (scanNodes ++ printNodes))
      if(node.isIo && node.component == c) {
        if (first) {
          harness.write("        ." + emitRef(node) + "(" + emitRef(node) + ")")
          first = false
        } else
          harness.write(",\n        ." + emitRef(node) + "(" + emitRef(node) + ")")
      }
    harness.write("\n")
    harness.write(" );\n")

    harness.write("  integer count;\n")
    harness.write("  always @(negedge clk) begin;\n")
    harness.write("  #50;\n")
    harness.write("    if (!reset) ")
    harness.write("count = $fscanf('h80000000, \"" + scanFormat.slice(0,scanFormat.length-1) + "\"")
    for (node <- scanNodes)
      harness.write(", " + emitRef(node))
    harness.write(");\n")
    harness.write("      if (count == -1) $finish(1);\n")
    harness.write("  end\n")
    harness.write("  always @(posedge clk) begin\n")
    harness.write("    if (!reset) ")
    harness.write("$display(\"" + printFormat.slice(0,printFormat.length-1) + "\"")

    for (node <- printNodes) {

      if(node.isIo && node.component == c) {
        harness.write(", " + emitRef(node))
      } else {
        var nextComp = node.component
        var path = "."
        while(nextComp != c) {
          path = "." + nextComp.instanceName + path
        }
        path = c.name + path + emitRef(node)
        harness.write(", " + path)
      }

    }
    harness.write(");\n")
    harness.write("  end\n")
    harness.write("endmodule\n")
    harness.close();
  }

  def emitDefs(c: Component): StringBuilder = {
    val res = new StringBuilder()
    for (m <- c.mods) {
      res.append(emitDef(m))
    }
    for (c <- c.children) {
      res.append(emitDef(c))
    }
    res
  }

  def emitRegs(c: Component): StringBuilder = {
    val res = new StringBuilder();
    res.append("  always @(posedge clk) begin\n");
    for (m <- c.mods) {
      res.append(emitReg(m))
    }
    res.append("  end\n");
    res
  }

  def emitReg(node: Node): String = {
    node match {
      case reg: Reg =>
        if(reg.isMemOutput)
            ""
        else if(reg.isEnable && (reg.enableSignal.litOf == null || reg.enableSignal.litOf.value != 1)){
          if(reg.isReset){
            "    if(reset) begin\n" + 
            "      " + emitRef(reg) + " <= " + emitRef(reg.resetVal) + ";\n" +
            "    end else if(" + emitRef(reg.enableSignal) + ") begin\n" + 
            "      " + emitRef(reg) + " <= " + emitRef(reg.updateVal) + ";\n" +
            "    end\n"
          } else {
            "    if(" + emitRef(reg.enableSignal) + ") begin\n" +
            "      " + emitRef(reg) + " <= " + emitRef(reg.updateVal) + ";\n" +
            "    end\n"
          }
        } else {
          "    " + emitRef(reg) + " <= " + 
          (if (reg.isReset) "reset ? " + emitRef(reg.resetVal) + " : " else "" ) + 
          emitRef(reg.updateVal) + ";\n"
        }

      case m: MemWrite[_] =>
        if (!m.used || !isInlineMem)
          return ""

        val i = "i" + emitTmp(m)
        if (m.isMasked)
          (0 until m.mem.width).map(i =>
            "    if (" + emitRef(m.cond) + " && " + emitRef(m.wmask) + "[" + i + "])\n" +
            "      " + emitRef(m.mem) + "[" + emitRef(m.addr) + "][" + i + "] <= " + emitRef(m.data) + "[" + i + "];\n"
          ).reduceLeft(_+_)
        else
          "    if (" + emitRef(m.cond) + ")\n" +
          "      " + emitRef(m.mem) + "[" + emitRef(m.addr) + "] <= " + emitRef(m.data) + ";\n"

      case _ =>
        ""
    }
  }

  // this function checks that there is no collision with verilog keywords, mangling the names if there
  // is a collision
  def checkNames(c: Component) = {
    for (m <- c.mods) {
      if (VerilogBackend.keywords.contains(m.name)) {
        m.name = m.name + "_"
      }
    }
  }

  def emitDecs(c: Component): StringBuilder = {
    val res = new StringBuilder();
    for (m <- c.mods) {
      res.append(emitDec(m))
    }
    res
  }

  def doCompile(c: Component, out: java.io.FileWriter, depth: Int): Unit = {
    c match {
      case x: BlackBox => c.traceNodes(); return
      case _ => 
    }

    // println("COMPILING COMP " + name);
    println("// " + depthString(depth) + "COMPILING " + c + " " + c.children.length + " CHILDREN");
    for (top <- c.children)
      doCompile(top, out, depth+1);
    c.findConsumers();
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors) err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length +" ERRORS");
    }
    c.collectNodes(c);
    val hasReg = c.containsReg || c.childrenContainsReg;
    val res = new StringBuilder()
    res.append((if (hasReg) "input clk, input reset" else ""));
    var first = true;
    var nl = "";
    for ((n, w) <- c.wires) {
      if(first && !hasReg) {first = false; nl = "\n"} else nl = ",\n";
      w match {
        case io: Bits => {
          if (io.dir == INPUT) {
	    res.append(nl + "    input " + emitWidth(io) + " " + emitRef(io));
          } else if(io.dir == OUTPUT) {
	    res.append(nl + "    output" + emitWidth(io) + " " + emitRef(io));
          }
        }
      };
    }
    res.append(");\n\n");
    checkNames(c)
    // TODO: NOT SURE EXACTLY WHY I NEED TO PRECOMPUTE TMPS HERE
    for (m <- c.mods)
      emitTmp(m);
    res.append(emitDecs(c));
    res.append("\n");
    res.append(emitDefs(c));
    if (c.containsReg) {
      res.append("\n");
      res.append(emitRegs(c));
    }
    res.append("endmodule\n\n");
    if(compDefs contains res){
      c.moduleName = compDefs(res);
    }else{
      c.moduleName = genCompName(c.name);
      compDefs += (res -> c.moduleName);
      //res.insert(0, "module " + c.moduleName + "(");
      out.append("module " + c.moduleName + "(")
      out.append(res);
    }
  }

  override def elaborate(c: Component): Unit = {
    topComponent = c;
    components.foreach(_.elaborate(0));
    for (c <- components)
      c.markComponent();
    c.genAllMuxes;
    components.foreach(_.postMarkNet(0));
    assignResets()
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors) err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length +" ERRORS");
    }
    c.inferAll();
    val base_name = ensure_dir(targetDir)
    if(saveWidthWarnings)
      widthWriter = new java.io.FileWriter(base_name + c.name + ".width.warnings")
    c.forceMatchingWidths;
    c.removeTypeNodes()
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors) err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length +" ERRORS");
    }
    nameChildren(topComponent)
    collectNodesIntoComp(c)
    transform(c, transforms)
    c.traceNodes();
    if(!ChiselErrors.isEmpty){
      for(err <- ChiselErrors) err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length +" ERRORS");
    }
    if(!dontFindCombLoop) c.findCombLoop();
    val out = new java.io.FileWriter(base_name + moduleNamePrefix + c.name + ".v");
    if(saveConnectionWarnings)
      connWriter = new java.io.FileWriter(base_name + c.name + ".connection.warnings")
    doCompile(c, out, 0);
    c.verifyAllMuxes;
    if(saveConnectionWarnings)
      connWriter.close()
    if(ChiselErrors isEmpty)
      out.close();
    else {
      for(err <- ChiselErrors)	err.printError;
      throw new IllegalStateException("CODE HAS " + ChiselErrors.length +" ERRORS");
    }
    if (configStr.length > 0) {
      val out_conf = new java.io.FileWriter(base_name+moduleNamePrefix+Component.topComponent.name+".conf");
      out_conf.write(configStr);
      out_conf.close();
    }
    if(saveComponentTrace)
      printStack
    if (isTesting && tester != null) {
      scanArgs.clear();  scanArgs  ++= tester.testInputNodes;    scanFormat  = ""
      printArgs.clear(); printArgs ++= tester.testNonInputNodes; printFormat = ""
    }
    if (isGenHarness)
      genHarness(c, base_name, c.name);
    compDefs.clear;
  }

  override def compile(c: Component, flags: String): Unit = {

    def run(cmd: String) = {
      val c = Process(cmd).!
      println(cmd + " RET " + c)
    }
    val dir = targetDir + "/"
    val src = dir + c.name + "-harness.v " + dir + c.name +".v"
    val cmd = "vcs +vc +v2k -timescale=10ns/10ps " + src + " -o " + dir + c.name
    run(cmd)

  }


}

