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

import java.io.File;
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import scala.sys.process._
import Reg._
import ChiselError._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap

object VerilogBackend {

  val keywords = HashSet[String](
    "always", "and", "assign", "attribute", "begin", "buf", "bufif0", "bufif1",
    "case", "casex", "casez", "cmos", "deassign", "default", "defparam",
    "disable", "edge", "else", "end", "endattribute", "endcase", "endfunction",
    "endmodule", "endprimitive", "endspecify", "endtable", "endtask", "event",
    "for", "force", "forever", "fork", "function", "highz0", "highz1", "if",
    "ifnone", "initial", "inout", "input", "integer", "join", "medium",
    "module", "large", "macromodule", "nand", "negedge", "nmos", "nor", "not",
    "notif0", "notif1", "or", "output", "parameter", "pmos", "posedge",
    "primitive", "pull0", "pull1", "pulldown", "pullup", "rcmos", "real",
    "realtime", "reg", "release", "repeat", "rnmos", "rpmos", "rtran",
    "rtranif0", "rtranif1", "scalared", "signed", "small", "specify",
    "specparam", "strength", "strong0", "strong1", "supply0", "supply1",
    "table", "task", "time", "tran", "tranif0", "tranif1", "tri", "tri0",
    "tri1", "triand", "trior", "trireg", "unsigned", "vectored", "wait",
    "wand", "weak0", "weak1", "while", "wire", "wor", "xnor", "xor")
}


/** This backend generates Verilog source code from a Chisel graph.
  */
class VerilogBackend extends Backend {
  Module.isEmittingComponents = true
  val keywords = VerilogBackend.keywords

  val flushedTexts = HashSet[String]()

  val memConfs = HashMap[String, String]()
  val compIndices = HashMap.empty[String,Int];

  private def getMemConfString: String =
    memConfs.map { case (conf, name) => "name " + name + " " + conf } reduceLeft(_ + _)

  private def getMemName(mem: MemDelay, configStr: String): String = {
    if (!memConfs.contains(configStr)) {
      /* Generates memory that are different in (depth, width, ports).
       All others, we return the previously generated name. */
      val compName = if (mem.component != null) {
        (if( !mem.component.moduleName.isEmpty ) {
          Backend.moduleNamePrefix + mem.component.moduleName
        } else {
          extractClassName(mem.component)
        } + "_")
      } else {
        Backend.moduleNamePrefix
      }
      // Generate a unique name for the memory module.
      val candidateName = compName + emitRef(mem)
      val memModuleName = if( compIndices contains candidateName ) {
        val count = (compIndices(candidateName) + 1)
        compIndices += (candidateName -> count)
        candidateName + "_" + count
      } else {
        compIndices += (candidateName -> 0)
        candidateName
      }
      memConfs += (configStr -> memModuleName)
    }
    memConfs(configStr)
  }

  /** Emit a width suffix */
  def emitWidth(node: Node): String = {

    if (node.width == 1) "" else " [" + (node.width-1) + ":0]"
  }

  override def emitTmp(node: Node): String =
    emitRef(node)

  override def emitRef(node: Node): String = {
    node match {
      case x: Literal =>
        (if (x.width == -1) {
          x.name
        } else if(x.isBinary) {
          ("" + x.width + "'b" + x.name)
        } else if(x.base == 'x') {
          ("" + x.width + "'h" + x.name.substring(2, x.name.length))
        } else if(x.base == 'd') {
          ("" + x.width + "'d" + x.name)
        } else if(x.base == 'h') {
          ("" + x.width + "'h" + x.name)
        } else {
          ""}
        ) + "/* " + x.inputVal + "*/";

      case _ =>
        super.emitRef(node)
    }
  }

  /** Emit definitions for a memory port */
  def emitPortDef(m: MemAccess, idx: Int): String = {
    def str(prefix: String, ports: (String, String)*): String =
      ports.toList.filter(_._2 != null)
        .map(p => "    ." + prefix + idx + p._1 + "(" + p._2 + ")")
        .reduceLeft(_ + ",\n" + _)

    m match {
      case r: MemSeqRead =>
        val addr = ("A", emitRef(r.addr))
        val en = ("E", emitRef(r.cond))
        val out = ("O", emitTmp(r))
        str("R", addr, en, out)

      case w: MemWrite =>
        val addr = ("A", emitRef(w.addr))
        val en = ("E", emitRef(w.cond))
        val data = ("I", emitRef(w.data))
        val mask = ("M", if (w.isMasked) emitRef(w.mask) else null)
        str("W", addr, en, data, mask)

      case rw: MemReadWrite =>
        val (r, w) = (rw.read, rw.write)
        val addr = ("A", emitRef(w.cond) + " ? " + emitRef(w.addr)
          + " : " + emitRef(r.addr))
        val en = ("E", emitRef(r.cond) + " || " + emitRef(w.cond))
        val write = ("W", emitRef(w.cond))
        val data = ("I", emitRef(w.data))
        val mask = ("M", if (w.isMasked) emitRef(w.mask) else null)
        val out = ("O", emitTmp(r))
        str("RW", addr, en, write, data, mask, out)

      case _ =>
        ""
    }
  }

  /** Emit definitions for a ``Module``. */
  def emitDef(c: Module): String = {
    /* module prototype */
    var res = ("  " + c.moduleName + " " + c.verilog_parameters
      + (if(c.verilog_parameters != "") " " else "") + c.name + "(")
    /* generated clocks and resets */
    if (c.clocks.length > 0) {
      res = res + (c.clocks).map(x => "." + emitRef(x)
        + "(" + emitRef(x) + ")").reduceLeft(_ + ", " + _)
    }
    if (c.resets.size > 0 ) {
      if (c.clocks.length > 0) res = res + ", "
      /* The reset signal is an end-point that needs to be propagated
       back the hierarchy to the top module. */
      res = res + (c.resets.toList).map(x => "." + emitRef(x)
        + "(" + emitRef(x) + ")").reduceLeft(_ + ", " + _)
    }
    var isFirst = true;
    val portDecs = new ArrayBuffer[StringBuilder]
    for ((n, w) <- c.wires) {
      if(n != "reset") {
        var portDec = "." + n + "( ";
        w match {
          case io: IOBound  =>
            if (io.isDirected(INPUT)) { // if reached, then input has consumers
              if (io.inputs.length == 0) {
                portDec = "//" + portDec
              } else if (io.inputs.length > 1) {
                  if(Module.saveConnectionWarnings) {
                    ChiselError.warning("" + io + " CONNECTED TOO MUCH " + io.inputs.length);
                  }
                portDec = "//" + portDec
              } else {
                portDec += emitRef(io.inputs(0));
              }
            } else if(io.isDirected(OUTPUT)) {
              if (io.consumers.length == 0) {
                /* pruneUnconnectedsIOs should have picked it up so no need
                 to generate a warning here. */
                portDec = "//" + portDec
              } else {
                /* There could be multiple consumers. We need to to find
                 the correct one. */
                val consumer = io.consumers.filter(_.component != io.component).head
                if (consumer == null) {
                  if(Module.saveConnectionWarnings) {
                    ChiselError.warning("" + io + "(" + io.component + ") OUTPUT UNCONNECTED (" + io.consumers.length + ") IN " + c.parent);
                  }
                  portDec = "//" + portDec
                } else {
                  if (io.prune)
                    portDec = "//" + portDec + emitRef(consumer)
                  else
                    portDec += emitRef(consumer); // TODO: FIX THIS?
                }
              }
            }
        }
        portDec += " )"
        portDecs += new StringBuilder(portDec)
      }
    }
    val uncommentedPorts = portDecs.filter(!_.result.contains("//"))
    uncommentedPorts.slice(0, uncommentedPorts.length-1).map(_.append(","))
    portDecs.map(_.insert(0, "       "))
    if (c.clocks.length > 0 || c.resets.size > 0) res += ",\n" else res += "\n"
    res += portDecs.map(_.result).reduceLeft(_ + "\n" + _)
    res += "\n  );\n";
    if (c.wires.map(_._2.driveRand).reduceLeft(_ || _)) {
      res += "  `ifndef SYNTHESIS\n"
      for ((n, w) <- c.wires) {
        if (w.driveRand) {
          res += "    assign " + c.name + "." + n + " = $random();\n"
        }
      }
      res += "  `endif\n"
    }
    res
  }

  /** Emit code that computes a ``Node``. */
  override def emitDef(node: Node): String = {
    val res =
    node match {
      case x: IOBound =>
        if( x.isDirected(INPUT) ) {
          if( node.inputs.length > 0 && node.inputs(0) != null
            && node.component == node.inputs(0).component ) {
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ";\n"
          } else {
            ""
          }
        } else {
          if (node.inputs.length == 0) {
            ChiselError.warning("UNCONNECTED " + node + " IN " + node.component); ""
          } else if (node.inputs(0) == null) {
            ChiselError.warning("UNCONNECTED WIRE " + node + " IN " + node.component); ""
          } else if( node.component == node.inputs(0).component ) {
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ";\n"
          } else {
            ""
          }
        }

      case x: CatOp => {
        var res = "  assign " + emitTmp(node) + " = {";
        var first = true;
        for(e <- x.inputs)
          res += (if(first) {first = false; ""} else ", ") + emitRef(e);
        res += "};\n";
        res
      }

      case x: DivSOp =>
        ("  assign " + emitTmp(x) + " = " + "$signed(" + emitRef(x.left) + ") "
          + "/ $signed(" + emitRef(x.right) + ");\n")

      case x: ExtractOp =>
        if (x.inputs.length < 3) {
          if(x.inputs(0).width > 1) {
            ("  assign " + emitTmp(x) + " = "
              + emitRef(x.inputs(0)) + "[" + emitRef(x.inputs(1)) + "];\n")
          } else {
            ("  assign " + emitTmp(x) + " = " + emitRef(x.inputs(0)) + ";\n")
          }
        } else {
          if(x.inputs(0).width > 1) {
            ("  assign " + emitTmp(x) + " = "
              + emitRef(x.inputs(0)) + "[" + emitRef(x.inputs(1))
              + ":" + emitRef(x.inputs(2)) + "];\n")
          } else {
            "  assign " + emitTmp(x) + " = " + emitRef(x.inputs(0)) + ";\n"
          }
        }

      case x: FillOp =>
        ("  assign " + emitTmp(x) + " = {"
          + emitRef(x.inputs(0)) + "{" + x.n + "}};\n");

      case ll: ListLookupRef => {
        ""
      }

      case ll: ListLookup => {
        val res = new StringBuilder()
        res.append("  always @(*) begin\n" +
                   //"    " + emitRef + " = " + inputs(1).emitRef + ";\n" +
                   "    casez (" + emitRef(ll.addr) + ")" + "\n");
        for ((addr, data) <- ll.map) {
          res.append("      " + emitRef(addr) + " : begin\n");
          for ((w, e) <- ll.wires zip data) {
            if(w.component != null && w.component.nodes.contains(w) ) {
              res.append("        " + emitRef(w) + " = " + emitRef(e) + ";\n");
            }
          }
          res.append("      end\n")
        }
        res.append("      default: begin\n")
        for ((w, e) <- ll.wires zip ll.defaultWires) {
          if(w.component != null && w.component.nodes.contains(w)) {
            res.append("        " + emitRef(w) + " = " + emitRef(e) + ";\n");
          }
        }
        res.append("      end\n");
        res.append(
          "    endcase\n" +
          "  end\n");
        res.toString
      }

      case l: Lookup => {
        var res =
          "  always @(*) begin\n" +
          "    " + emitRef(l) + " = " + emitRef(l.inputs(1)) + ";\n" +
          "    casez (" + emitRef(l.inputs(0)) + ")" + "\n";

        for (node <- l.map)
          res = res + "      " + emitRef(node._1) + " : " + emitRef(l) + " = " + emitRef(node._2) + ";\n";
        res = res +
          "    endcase\n" +
          "  end\n";
        res
      }

      case x: Log2Op => {
        val res = new StringBuilder
        var mux = UInt(0)
        res.append(emitDef(mux.node))
        for (i <- 1 until x.nbits) {
          val cond = Extract(UInt(x.opand), UInt(i))
          res.append(emitDef(cond.node))
          val left = UInt(i, Literal.sizeof(x.nbits-1))
          res.append(emitDef(left.node))
          mux = Mux(cond, left, mux)
          res.append(emitDef(mux.node))
        }
        res.toString
      }

      case x: MuxOp =>
        // XXX .otherwise might be null
        ("  assign " + emitTmp(x) + " = "
          + (if(x.otherwise != null)
            (emitRef(x.cond) + " ? " + emitRef(x.thenNode)
              + " : " + emitRef(x.otherwise)) else emitRef(x.thenNode)) + ";\n")

      case x: MulSOp =>
        ("  assign " + emitTmp(x) + " = " + "$signed(" + emitRef(x.left) + ") "
          + "* $signed(" + emitRef(x.right) + ");\n")

      case x: RemSOp =>
        ("  assign " + emitTmp(x) + " = " + "$signed(" + emitRef(x.left) + ") "
          + "% $signed(" + emitRef(x.right) + ");\n")

      case x: RightShiftSOp =>
        ("  assign " + emitTmp(x) + " = "
          + "$signed(" + emitRef(x.left) + ") "
          + ">>>" + " " + emitRef(x.right) + ";\n")

      case r: ROMemDelay => {
        val inits = new StringBuilder
        for (i <- 0 until r.inputs.length)
          inits append("    " + emitRef(r) + "[" + i + "] = "
            + emitRef(r.inputs(i)) + ";\n")

        "  always @(*) begin\n" +
        inits +
        "  end\n"
      }

      case m: MemDelay =>
        if(m.isInline) {
          ""
        } else {
          if( m.ports.length > 0 ) {
            val configStr =
              (" depth " + m.depth +
                " width " + m.width +
                " ports " + m.ports.map(_.getPortType).reduceLeft(_ + "," + _) +
                "\n")
            val name = getMemName(m, configStr)
            ChiselError.info("MEM " + name)
            val clkrst = Array("    .CLK(" + emitRef(m.clock) + ")", "    .RST(" + emitRef(m.reset) + ")")
            val portdefs = for (i <- 0 until m.ports.size)
            yield emitPortDef(m.ports(i), i)
            "  " + name + " " + emitRef(m) + " (\n" +
              (clkrst ++ portdefs).reduceLeft(_ + ",\n" + _) + "\n" +
            "  );\n"
          } else {
            ChiselError.error("Memory " + m.name + " has no ports")
          }
        }

      case m: MemRead =>
        if (m.mem.isInline) {
          "  assign " + emitTmp(m) + " = " + emitRef(m.mem) + "[" + emitRef(m.addr) + "];\n"
        } else {
          ""
        }

      case s: Sprintf =>
        ("  always @(*) $sformat(" + emitTmp(s) + ", "
          + s.args.map(emitRef _).foldLeft(CString(s.format))(_ + ", " + _)
          + ");\n")

        /* These cannot be alphabetically sorted. They need to appear
         after all subclasses have been matched. */
      case x:BinaryOp =>
       ("  assign " + emitTmp(x) + " = "
         + emitRef(x.inputs(0)) + " "
         + x.opInfix + " " + emitRef(x.inputs(1)) + ";\n")

      case x: UnaryOp =>
        ("  assign " + emitTmp(x) + " = "
          + x.opPrefix + " " + emitRef(x.inputs(0)) + ";\n")

      case _ =>
        ""
    }
    (if (node.prune && res != "") "//" else "") + res
  }

  def emitDecBase(node: Node): String = {
    "  wire" + emitWidth(node) + " " + emitRef(node) + ";\n"
  }

  override def emitDec(node: Node): String = {
    val res =
    node match {
      case x: ListLookupRef =>
        "  reg" + emitWidth(x) + " " + emitRef(x) + ";\n";

      case x: Lookup =>
        "  reg" + emitWidth(x) + " " + emitRef(x) + ";\n";

      case x: Sprintf =>
        "  reg" + emitWidth(x) + " " + emitRef(x) + ";\n";

      case x: Literal =>
        ""

      case x: Update =>
        ""

      case x: RegDelay =>
          "  reg" + emitWidth(x) + " " + emitRef(x) + (
            if( x.depth > 1) " [" + (x.depth-1) + ":0]" else "") + ";\n"

      case m: MemDelay =>
        if (m.isInline) {
          "  reg" + emitWidth(m) + " " + emitRef(m) + " [" + (m.depth - 1) + ":0];\n"
        } else {
          ""
        }

      case x: MemAccess =>
        emitDecBase(node)

      case x =>
        if( !x.isReset ) emitDecBase(node) else ""
    }
    (if (node.prune && res != "") "//" else "") + res
  }

  def genHarness(c: Module, name: String) {
    val harness  = createOutputFile(name + "-harness.v");
    val printFormat = Module.printArgs.map(a => "0x%x").fold("")((y,z) => z + " " + y)
    val scanFormat = Module.scanArgs.map(a => "%x").fold("")((y,z) => z + " " + y)
    val printNodes = for (arg <- Module.printArgs) yield arg
    val scanNodes = for (arg <- Module.scanArgs.filter(n => c.isInput(n))) yield arg
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

    if(c.containsRegInTree) {
      harness.write("        .clk(clk),\n")
      harness.write("        .reset(reset),\n")
    }

    var first = true
    for (node <- (scanNodes ++ printNodes))
      if(node.isIo && node.component == c) {
        if (first) {
          harness.write("        ." + emitRef(node) + "(" + emitRef(node) + ")")
          first = false
        } else {
          harness.write(",\n        ." + emitRef(node) + "(" + emitRef(node) + ")")
        }
      }
    harness.write("\n")
    harness.write(" );\n")

    harness.write("  integer count;\n")
    harness.write("  always @(negedge clk) begin\n")
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
          /* XXX This code is most likely never executed otherwise
                 it would end in an infinite loop. */
          path = "." + nextComp.name + path
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

  def emitDefs(c: Module): StringBuilder = {
    val res = new StringBuilder()
    for (m <- c.nodes) {
      res.append(emitDef(m))
    }
    for (c <- c.children) {
      res.append(emitDef(c))
    }
    res
  }

  def emitRegs(c: Module): StringBuilder = {
    val res = new StringBuilder();
    val clkDomains = new HashMap[Update, StringBuilder]
    for (clock <- c.clocks) {
      val sb = new StringBuilder
      sb.append("  always @(posedge " + emitRef(clock) + ") begin\n")
      clkDomains += (clock -> sb)
    }
    for (m <- c.nodes) {
      if( m.isInstanceOf[Delay] || m.isInstanceOf[MemWrite] ) {
        val clock = (if( m.isInstanceOf[Delay] ) m.asInstanceOf[Delay].clock
        else m.asInstanceOf[MemWrite].mem.clock)
        if( clock != null ) {
          clkDomains(clock).append(emitReg(m))
        }
      }
    }
    for (clock <- c.clocks) {
      clkDomains(clock).append("  end\n")
      res.append(clkDomains(clock).result())
    }
    res
  }

  def emitReg(node: Node): String = {
    node match {
      case reg: RegDelay =>
        val enable = reg.enable
        if( enable != null ){
          if(reg.hasReset){
            "    if(" + emitRef(reg.reset) + ") begin\n" +
            "      " + emitRef(reg) + " <= " + emitRef(reg.init) + ";\n" +
            "    end else if(" + emitRef(enable) + ") begin\n" +
            "      " + emitRef(reg) + " <= " + emitRef(reg.next) + ";\n" +
            "    end\n"
          } else {
            "    if(" + emitRef(enable) + ") begin\n" +
            "      " + emitRef(reg) + " <= " + emitRef(reg.next) + ";\n" +
            "    end\n"
          }
        } else {
          "    " + emitRef(reg) + " <= " +
          (if (reg.hasReset) {
            emitRef(reg.reset) + " ? " + emitRef(reg.init) + " : "
          } else {
            ""
          }) + emitRef(reg.next) + ";\n"
        }

      case m: MemWrite =>
        if (m.mem.isInline) {
          val i = "i" + emitTmp(m)
          if (m.isMasked) {
            (0 until m.mem.width).map(i =>
              "    if (" + emitRef(m.cond) + " && " + emitRef(m.mask) + "[" + i + "])\n" +
                "      " + emitRef(m.mem) + "[" + emitRef(m.addr) + "][" + i + "] <= " + emitRef(m.data) + "[" + i + "];\n"
            ).reduceLeft(_ + _)
          } else {
            "    if (" + emitRef(m.cond) + ")\n" +
            "      " + emitRef(m.mem) + "[" + emitRef(m.addr) + "] <= " + emitRef(m.data) + ";\n"
          }
        } else {
          ""
        }
      case a: Assert =>
        "`ifndef SYNTHESIS\n" +
        "    if(!" + emitRef(a.cond) + ") begin\n" +
        "      $fwrite(32'h80000002, " + CString("ASSERTION FAILED: %s\n") + ", " + CString(a.message) + ");\n" +
        "      $finish;\n" +
        "    end\n" +
        "`endif\n"
      case p: Printf =>
        "`ifndef SYNTHESIS\n" +
        "`ifdef PRINTF_COND\n" +
        "    if (`PRINTF_COND)\n" +
        "`endif\n" +
        "      if (" + emitRef(p.cond) + ")\n" +
        "        $fwrite(32'h80000002, " + p.args.map(emitRef _).foldLeft(CString(p.format))(_ + ", " + _) + ");\n" +
        "`endif"
      case _ =>
        ""
    }
  }

  def emitDecs(c: Module): StringBuilder = {
    val res = new StringBuilder();
    for (m <- c.nodes -- c.io.flatten.map(x => x._2.node)) {
      res.append(emitDec(m))
    }
    res
  }


  def emitModuleText(c: Module): String = {
    if (c.isInstanceOf[BlackBox])
      return ""

    val res = new StringBuilder()
    var first = true;
    var nl = "";
    if (c.clocks.length > 0 || c.resets.size > 0)
      res.append((c.clocks ++ c.resets.toList).map(x => "input " + emitRef(x)).reduceLeft(_ + ", " + _))
    val ports = new ArrayBuffer[StringBuilder]
    for ((n, w) <- c.wires) {
      // if(first && !hasReg) {first = false; nl = "\n"} else nl = ",\n";
      w match {
        case io: IOBound => {
          val prune = if (io.prune && c != Module.topComponent) "//" else ""
          if (io.isDirected(INPUT)) {
            ports += new StringBuilder(nl + "    " + prune + "input" +
              emitWidth(io) + " " + emitRef(io));
          } else if(io.isDirected(OUTPUT)) {
            ports += new StringBuilder(nl + "    " + prune + "output" +
              emitWidth(io) + " " + emitRef(io));
          }
        }
      };
    }
    val uncommentedPorts = ports.filter(!_.result.contains("//"))
    uncommentedPorts.slice(0, uncommentedPorts.length-1).map(_.append(","))
    if (c.clocks.length > 0 || c.resets.size > 0) res.append(",\n") else res.append("\n")
    res.append(ports.map(_.result).reduceLeft(_ + "\n" + _))
    res.append("\n);\n\n");
    // TODO: NOT SURE EXACTLY WHY I NEED TO PRECOMPUTE TMPS HERE
    for (m <- c.nodes)
      emitTmp(m);
    res.append(emitDecs(c));
    res.append("\n");
    res.append(emitDefs(c));
    if (c.containsReg) {
      res.append("\n");
      res.append(emitRegs(c));
    }
    res.append("endmodule\n\n");
    res.result();
  }

  def flushModules( out: java.io.FileWriter,
    defs: HashMap[String, LinkedHashMap[String, ArrayBuffer[Module] ]],
    level: Int ) {
    for( (className, modules) <- defs ) {
      var index = 0
      for ( (text, comps) <- modules) {
        val moduleName = if( modules.size > 1 ) {
          className + "_" + index.toString;
        } else {
          className;
        }
        index = index + 1
        var textLevel = 0;
        for( flushComp <- comps ) {
          textLevel = flushComp.level;
          if( flushComp.level == level ) {
            flushComp.moduleName = moduleName
          }
        }
        if( textLevel == level ) {
          /* XXX We write the module source text in *emitChildren* instead
                 of here so as to generate a minimal "diff -u" with the previous
                 implementation. */
        }
      }
    }
  }


  def emitChildren(top: Module,
    defs: HashMap[String, LinkedHashMap[String, ArrayBuffer[Module] ]],
    out: java.io.FileWriter, depth: Int) {
    if (top.isInstanceOf[BlackBox])
      return

    for (child <- top.children) {
      emitChildren(child, defs, out, depth + 1);
    }
    val className = extractClassName(top);
    for( (text, comps) <- defs(className)) {
      if( comps contains top ) {
        if( !(flushedTexts contains text) ) {
          out.append("module " + top.moduleName + "(")
          out.append(text);
          flushedTexts += text
        }
        return;
      }
    }
  }


  def doCompile(top: Module, out: java.io.FileWriter, depth: Int): Unit = {
    /* *defs* maps Mod classes to Mod instances through
       the generated text of their module.
       We use a LinkedHashMap such that later iteration is predictable. */
    val defs = new HashMap[String, LinkedHashMap[String, ArrayBuffer[Module] ]];
    var level = 0;
    for( c <- sortedComps ) {
      ChiselError.info(depthString(depth) + "COMPILING " + c
        + " " + c.children.length + " CHILDREN"
        + " (" + c.level + "," + c.traversal + ")");
      ChiselError.checkpoint()

      if( c.level > level ) {
        /* When a component instance instantiates different sets
         of sub-components based on its constructor parameters, the same
         Module class might appear with different level in the tree.
         We thus wait until the very end to generate module names.
         If that were not the case, we could flush modules as soon as
         the source text for all components at a certain level in the tree
         has been generated. */
        flushModules(out, defs, level);
        level = c.level
      }
      val res = emitModuleText(c);
      val className = extractClassName(c);
      if( !(defs contains className) ) {
        defs += (className -> LinkedHashMap[String, ArrayBuffer[Module] ]());
      }
      if( defs(className) contains res ) {
        /* We have already outputed the exact same source text */
        defs(className)(res) += c;
        ChiselError.info("\t" + defs(className)(res).length + " components");
      } else {
        defs(className) += (res -> ArrayBuffer[Module](c));
      }
    }
    flushModules(out, defs, level);
    emitChildren(top, defs, out, depth);
  }

  override def elaborate(c: Module) {
    super.elaborate(c)

    val out = createOutputFile(c.name + ".v");
    doCompile(c, out, 0);
    out.close();

    if (!memConfs.isEmpty) {
      val out_conf = createOutputFile(Module.topComponent.name + ".conf");
      out_conf.write(getMemConfString);
      out_conf.close();
    }
    if( Module.tester != null ) {
      Module.scanArgs.clear();
      Module.scanArgs  ++= Module.tester.testInputNodes.map(n => n.node)
      Module.scanFormat  = ""

      Module.printArgs.clear();
      Module.printArgs ++= Module.tester.testNonInputNodes.map(n => n.node)
      Module.printFormat = ""
    }
    if (Module.isGenHarness) {
      genHarness(c, c.name);
    }
  }

  override def compile(c: Module, flags: String) {

    def run(cmd: String) {
      val c = Process(cmd).!
      ChiselError.info(cmd + " RET " + c)
    }
    val dir = Module.targetDir + "/"
    val src = dir + c.name + "-harness.v " + dir + c.name + ".v"
    val cmd = "vcs +vc +v2k -timescale=10ns/10ps " + src + " -o " + dir + c.name
    run(cmd)

  }


}

