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
import scala.collection.mutable.{ArrayBuffer, HashSet, HashMap, LinkedHashMap}
import scala.collection.immutable.ListSet
import sys.process.stringSeqToProcess

object VerilogBackend {

  val keywords = Set[String](
    "always", "and", "assign", "attribute", "begin", "buf", "bufif0", "bufif1",
    "case", "casex", "casez", "cmos", "deassign", "default", "defparam",
    "disable", "edge", "else", "end", "endattribute", "endcase", "endfunction",
    "endmodule", "endprimitive", "endspecify", "endtable", "endtask", "event",
    "for", "force", "forever", "fork", "function", "highz0", "highz1", "if",
    "ifnone", "initial", "inout", "input", "integer", "initvar", "join",
    "medium", "module", "large", "macromodule", "nand", "negedge", "nmos",
    "nor", "not", "notif0", "notif1", "or", "output", "parameter", "pmos",
    "posedge", "primitive", "pull0", "pull1", "pulldown", "pullup", "rcmos",
    "real", "realtime", "reg", "release", "repeat", "rnmos", "rpmos", "rtran",
    "rtranif0", "rtranif1", "scalared", "signed", "small", "specify",
    "specparam", "strength", "strong0", "strong1", "supply0", "supply1",
    "table", "task", "time", "tran", "tranif0", "tranif1", "tri", "tri0",
    "tri1", "triand", "trior", "trireg", "unsigned", "vectored", "wait",
    "wand", "weak0", "weak1", "while", "wire", "wor", "xnor", "xor",
    "SYNTHESIS", "PRINTF_COND", "VCS")

  var traversalIndex = 0
}

class VerilogBackend extends Backend {
  val keywords = VerilogBackend.keywords
  override val needsLowering = Set("PriEnc", "OHToUInt", "Log2")

  override def isEmittingComponents: Boolean = true

  val emittedModules = HashSet[String]()

  val memConfs = HashMap[String, String]()
  val compIndices = HashMap[String, Int]()

  private def getMemConfString: String =
    memConfs.map { case (conf, name) => "name " + name + " " + conf } reduceLeft(_ + _)

  private def getMemName(mem: Mem[_], configStr: String): String = {
    if (!memConfs.contains(configStr)) {
      /* Generates memory that are different in (depth, width, ports).
       All others, we return the previously generated name. */
      val compName = (if( !mem.component.moduleName.isEmpty ) {
        Driver.moduleNamePrefix + mem.component.moduleName
      } else {
        extractClassName(mem.component)
      }) + "_"
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

  def emitWidth(node: Node): String = {
    val w = node.needWidth()
    if (w == 1) "" else "[" + (w-1) + ":0]"
  }

  override def emitTmp(node: Node): String =
    emitRef(node)

  override def emitRef(node: Node): String = {
    node match {
      case x: Literal => emitLit(x.value, x.needWidth())
      case _: Reg =>
        if (node.name != "") node.name else "R" + node.emitIndex
      case _ =>
        if (node.name != "") node.name else "T" + node.emitIndex
    }
  }

  private def emitLit(x: BigInt): String =
    emitLit(x, x.bitLength + (if (x < 0) 1 else 0))
  private def emitLit(x: BigInt, w: Int): String = {
    val unsigned = if (x < 0) (BigInt(1) << w) + x else x
    require(unsigned >= 0)
    w + "'h" + unsigned.toString(16)
  }

  // $random only emits 32 bits; repeat its result to fill the Node
  private def emitRand(node: Node): String =
    "{" + ((node.needWidth()+31)/32) + "{$random}}"

  def emitPortDef(m: MemAccess, idx: Int): String = {
    def str(prefix: String, ports: (String, String)*): String =
      ports.toList map (p => "    ." + prefix + idx + p._1 + "(" + p._2 + ")") reduceLeft (_ + ",\n" + _)

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
        val addr = ("A", emitRef(w.cond) + " ? " + emitRef(w.addr) + " : " + emitRef(r.addr))
        val en = ("E", emitRef(r.cond) + " || " + emitRef(w.cond))
        val write = ("W", emitRef(w.cond))
        val data = ("I", emitRef(w.data))
        val mask = ("M", if (w.isMasked) emitRef(w.mask) else null)
        val out = ("O", emitTmp(r))
        str("RW", addr, en, write, data, mask, out)
    }
  }

  def emitDef(c: Module): String = {
    val spacing = (if(c.verilog_parameters != "") " " else "");
    var res = "  " + c.moduleName + " " + c.verilog_parameters + spacing + c.name + "(";
    if (c.clocks.length > 0) {
      res = res + (c.clocks).map(x => "." + emitRef(x) + "(" + emitRef(x) + ")").reduceLeft(_ + ", " + _)
    }
    if (c.resets.size > 0 ) {
      if (c.clocks.length > 0) res = res + ", "
      res = res + (c.resets.values.toList).map(x => "." + emitRef(x) + "(" + emitRef(x.inputs(0)) + ")").reduceLeft(_ + ", " + _)
    }
    var isFirst = true;
    val portDecs = new ArrayBuffer[StringBuilder]
    for ((n, w) <- c.wires) {
      if(n != "reset" && n != Driver.implicitReset.name) {
        var portDec = "." + n + "( ";
        w match {
          case io: Bits  =>
            if (io.dir == INPUT) { // if reached, then input has consumers
              if (io.inputs.length == 0) {
                  // if (Driver.saveConnectionWarnings) {
                  //   ChiselError.warning("" + io + " UNCONNECTED IN " + io.component);
                  // } removed this warning because pruneUnconnectedIOs should have picked it up
                portDec = "//" + portDec
              } else if (io.inputs.length > 1) {
                  if (Driver.saveConnectionWarnings) {
                    ChiselError.warning("" + io + " CONNECTED TOO MUCH " + io.inputs.length);
                  }
                portDec = "//" + portDec
              /* } else if (!c.isWalked.contains(w)){
                  if (Driver.saveConnectionWarnings) {
                    ChiselError.warning(" UNUSED INPUT " + io + " OF " + c + " IS REMOVED");
                  }
                portDec = "//" + portDec // I don't think this is necessary */
              } else {
                portDec += emitRef(io.inputs(0));
              }
            } else if(io.dir == OUTPUT) {
              if (io.consumers.size == 0) {
                  // if (Driver.saveConnectionWarnings) {
                  //   ChiselError.warning("" + io + " UNCONNECTED IN " + io.component + " BINDING " + c.findBinding(io));
                  // } removed this warning because pruneUnconnectedsIOs should have picked it up
                portDec = "//" + portDec
              } else {
                c.parent.findBinding(io) match {
                  case None => {
                    if (Driver.saveConnectionWarnings) {
                      ChiselError.warning("" + io + "(" + io.component + ") OUTPUT UNCONNECTED (" + 
                                          io.consumers.size + ") IN " + c.parent)
                    }
                    portDec = "//" + portDec
                  }
                  case Some(consumer) => {
                    if (io.prune)
                      portDec = "//" + portDec + emitRef(consumer)
                    else
                      portDec += emitRef(consumer); // TODO: FIX THIS?
                  }
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
      res += if_not_synthesis
      for ((n, w) <- c.wires) {
        if (w.driveRand) {
          res += "    assign " + c.name + "." + n + " = " + emitRand(w) + ";\n"
        }
      }
      res += endif_not_synthesis
    }
    res
  }

  override def emitDef(node: Node): String = {
    val res =
    node match {
      case x: Bits =>
        if (x.isIo && x.dir == INPUT) {
          ""
        } else {
          if (node.inputs.length == 0) {
            ChiselError.warning("UNCONNECTED " + node + " IN " + node.component)
            "  assign " + emitTmp(node) + " = " + emitRand(node) + ";\n"
          } /* else if (node.inputs(0) == null) {
            ChiselError.warning("UNCONNECTED WIRE " + node + " IN " + node.component)
            "  assign " + emitTmp(node) + " = " + emitRand(node) + ";\n"
          } */ else {
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ";\n"
          }
        }

      case x: Mux =>
        "  assign " + emitTmp(x) + " = " + emitRef(x.inputs(0)) + " ? " + emitRef(x.inputs(1)) + " : " + emitRef(x.inputs(2)) + ";\n"

      case o: Op =>
        val c = o.component;
        "  assign " + emitTmp(o) + " = " +
        (if (o.op == "##") {
          "{" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + "}"
        } else if (node.inputs.length == 1) {
          o.op + " " + emitRef(node.inputs(0))
        } else if (o.op == "s*s" || o.op == "s*u" || o.op == "s%s" || o.op == "s/s") {
          "$signed(" + emitRef(node.inputs(0)) + ") " + o.op(1) + " $signed(" + emitRef(node.inputs(1)) + ")"
        } else if (o.op == "s<" || o.op == "s<=") {
          "$signed(" + emitRef(node.inputs(0)) + ") " + o.op.tail + " $signed(" + emitRef(node.inputs(1)) + ")"
        } else if (o.op == "s>>") {
          "$signed(" + emitRef(node.inputs(0)) + ") >>> " + emitRef(node.inputs(1))
        } else {
          emitRef(node.inputs(0)) + " " + o.op + " " + emitRef(node.inputs(1))
        }) + ";\n"

      case x: Extract =>
        node.inputs.tail.foreach(x.validateIndex)
        val gotWidth = node.inputs(0).needWidth()
        if (node.inputs.length < 3) {
          if(gotWidth > 1) {
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + "[" + emitRef(node.inputs(1)) + "];\n"
          } else {
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ";\n"
          }
        } else {
          if(gotWidth > 1) {
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + "[" + emitRef(node.inputs(1)) + ":" + emitRef(node.inputs(2)) + "];\n"
          } else {
            "  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)) + ";\n"
          }
        }

      case m: Mem[_] =>
        if(!m.isInline) {
          def gcd(a: Int, b: Int) : Int = { if(b == 0) a else gcd(b, a%b) }
          def find_gran(x: Node) : Int = {
            if (x.isInstanceOf[Literal])
              return x.needWidth()
            else if (x.isInstanceOf[UInt])
              return if (x.inputs.length>0) find_gran(x.inputs(0)) else 1
            else if (x.isInstanceOf[Mux])
              return gcd(find_gran(x.inputs(1)), find_gran(x.inputs(2)))
            else if (x.isInstanceOf[Op])
              return (x.inputs.map(find_gran(_))).reduceLeft(_ max _)
            else
              return 1
          }
          val mask_writers = m.writeAccesses.filter(_.isMasked)
          val mask_grans = mask_writers.map(x => find_gran(x.mask))
          val mask_gran = if (!mask_grans.isEmpty && mask_grans.forall(_ == mask_grans(0))) mask_grans(0) else 1
          val configStr =
          (" depth " + m.n +
            " width " + m.needWidth() +
            " ports " + m.ports.map(_.getPortType).reduceLeft(_ + "," + _) +
            (if (mask_gran != 1) " mask_gran " + mask_gran else "") +
            "\n")
          val name = getMemName(m, configStr)
          ChiselError.info("MEM " + name)

          val clk = "    .CLK(" + emitRef(m.clock.get) + ")"
          val portdefs = for (i <- 0 until m.ports.size)
            yield emitPortDef(m.ports(i), i)
          "  " + name + " " + emitRef(m) + " (\n" +
            (clk +: portdefs).reduceLeft(_ + ",\n" + _) + "\n" +
          "  );\n"
        } else {
          ""
        }
      case m: MemRead =>
        if (m.mem.isInline) {
          "  assign " + emitTmp(node) + " = " + emitRef(m.mem) + "[" + emitRef(m.addr) + "];\n"
        } else {
          ""
        }

      case r: ROMRead =>
        val inits = new StringBuilder
        for ((i, v) <- r.rom.sparseLits)
          inits append s"    ${i}: ${emitRef(r)} = ${emitRef(v)};\n"
        s"  always @(*) case (${emitRef(r.inputs.head)})\n" +
        inits +
        s"    default: begin\n" +
        s"      ${emitRef(r)} = ${r.needWidth()}'bx;\n" +
        if_not_synthesis +
        s"      ${emitRef(r)} = ${emitRand(r)};\n" +
        endif_not_synthesis +
        s"    end\n" +
        "  endcase\n"

      case s: Sprintf =>
        "  always @(*) $sformat(" + emitTmp(s) + ", " + s.args.map(emitRef _).foldLeft(CString(s.format))(_ + ", " + _) + ");\n"

      case _ =>
        ""
    }
    (if (node.prune && res != "") "//" else "") + res
  }

  def emitDecBase(node: Node, wire: String = "wire"): String =
    s"  ${wire}${emitWidth(node)} ${emitRef(node)};\n"

  def emitDecReg(node: Node): String = emitDecBase(node, "reg ")

  override def emitDec(node: Node): String = {
    val gotWidth = node.needWidth()
    val res =
    node match {
      case x: Bits =>
        if(!x.isIo) {
          emitDecBase(node)
        } else {
          ""
        }

      case _: Assert =>
        "  reg" + "[" + (gotWidth-1) + ":0] " + emitRef(node) + ";\n"

      case _: Reg =>
        emitDecReg(node)

      case _: Sprintf =>
        emitDecReg(node)

      case _: ROMRead =>
        emitDecReg(node)

      case m: Mem[_] =>
        if (m.isInline) {
          "  reg [" + (m.needWidth()-1) + ":0] " + emitRef(m) + " [" + (m.n-1) + ":0];\n"
        } else {
          ""
        }

      case x: MemAccess =>
        x.referenced = true
        emitDecBase(node)

      case _: ROMData => ""

      case _: Literal => ""

      case _ =>
        emitDecBase(node)
    }
    (if (node.prune && res != "") "//" else "") + res
  }

  def emitInit(node: Node): String = node match {
    case r: Reg =>
      "    " + emitRef(r) + " = " + emitRand(r) + ";\n"
    case m: Mem[_] =>
      if (m.isInline)
        "    for (initvar = 0; initvar < " + m.n + "; initvar = initvar+1)\n" +
        "      " + emitRef(m) + "[initvar] = " + emitRand(m) + ";\n"
      else
        ""
    case a: Assert =>
      "    " + emitRef(a) + " = 1'b0;\n"
    case _ =>
      ""
  }

  def genHarness(c: Module, name: String) {
    val harness  = createOutputFile(name + "-harness.v");
    val printNodes = for ((n, io) <- c.wires ; if io.dir == OUTPUT) yield io
    val scanNodes = for ((n, io) <- c.wires ; if io.dir == INPUT) yield io
    val mainClk = Driver.implicitClock
    val clocks = ListSet(mainClk) ++ c.clocks
    val resets = c.resets.values.toList

    harness.write("module test;\n")
    for (node <- scanNodes) {
      val gotWidth = node.needWidth()
      harness.write("  reg [" + (gotWidth-1) + ":0] " + emitRef(node) + ";\n")
    }
    for (node <- printNodes) {
      val gotWidth = node.needWidth()
      harness.write("  wire [" + (gotWidth-1) + ":0] " + emitRef(node) + ";\n")
    }
    for (rst <- resets)
      harness.write("  reg %s;\n".format(rst.name))

    // Diffent code generation for clocks
    if (Driver.isCompiling) {
      harness.write("  reg %s = 0;\n".format(mainClk.name))
      if (clocks.size > 1) {
        for (clk <- clocks) {
          val clkLength = clk.srcClock match {
            case None => "0"
            case Some(src) => src.name + "_length " + clk.initStr
          }
          harness.write("  integer %s_length = %s;\n".format(clk.name, clkLength))
          harness.write("  integer %s_cnt = 0;\n".format(clk.name))
          harness.write("  reg %s_fire = 0;\n".format(clk.name))
        }
      }
      harness.write("  always #`CLOCK_PERIOD %s = ~%s;\n\n".format(mainClk.name, mainClk.name))
    } else {
      for (clk <- clocks) {
        val clkLength = clk.srcClock match {
          case None => "`CLOCK_PERIOD"
          case Some(src) => src.name + "_length " + clk.initStr
        }
        harness.write("  reg %s = 1;\n".format(clk.name))
        harness.write("  parameter %s_length = %s;\n".format(clk.name, clkLength))
      }
      for (clk <- clocks) {
        harness.write("  always #%s_length %s = ~%s;\n".format(clk.name, clk.name, clk.name))
      }
    }

    harness.write("  /*** DUT instantiation ***/\n")
    harness.write("    " + c.moduleName + "\n")
    harness.write("      " + c.name + "(\n")
    if (Driver.isCompiling) {
      if (c.clocks.size == 1) {
        harness.write("        .%s(%s),\n".format(mainClk.name, mainClk.name))
      } else {
        for (clk <- c.clocks)
          harness.write("        .%s(%s && %s_fire),\n".format(
            clk.name, mainClk.name, clk.name)
         )
      }
    } else {
      for (clk <- c.clocks)
        harness.write("        .%s(%s),\n".format(clk.name, clk.name))
    }
    for (rst <- resets)
      harness.write("        .%s(%s),\n".format(rst.name, rst.name))
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
    harness.write(" );\n\n")

    // collect Chisel nodes for VCD dump
    val dumpvars = new ArrayBuffer[Node]
    for (m <- Driver.components ; node <- m.nodes ; if node.isInVCD) {
      node match {
        case io: Bits if m != c => {
          var included = true
          if (io.dir == INPUT) {
            if (io.inputs.length == 0 || io.inputs.length > 1)
              included = false
          }
          else if (io.dir == OUTPUT) {
            if (io.consumers.size == 0 || m.parent.findBinding(io) == None || io.prune)
              included = false
          }
          if (included) dumpvars += io
        }
        case _ => dumpvars += node
      }
    }

    harness.write("  /*** VCD / VPD dumps ***/\n")
    harness.write("  initial begin\n")
    if (Driver.isDebug) {
      harness.write("    /*** Debuggin with VPD dump ***/\n")
      harness.write("    $vcdplusfile(\"%s.vpd\");\n".format(Driver.targetDir+c.name))
      harness.write("    $vcdpluson(0, %s);\n".format(c.name))
      if (Driver.isVCDMem) harness.write("  $vcdplusmemon;\n")
    }
    if (!Driver.isDebug && Driver.isVCD) {
      harness.write("    /*** VCD dump ***/\n")
      var first = true
      for (dumpvar <- dumpvars) {
        val pathName = dumpvar.component.getPathName(".") + "." + emitRef(dumpvar)
        harness.write("    $dumpvars(1, %s);\n".format(pathName))
      }
      harness.write("    $dumpfile(\"%s.vcd\");\n".format(Driver.targetDir+c.name))
      harness.write("    $dumpon;\n")
    }
    harness.write("  end\n\n")

    if (Driver.isCompiling) {
      harness write harnessAPIs(mainClk, clocks, resets) 
    } else {
      harness write harnessBase(mainClk, resets, scanNodes, printNodes)
    }
    harness.write("endmodule\n")

    harness.close();
  }

  def harnessAPIs (mainClk: Clock, clocks: ListSet[Clock], resets: List[Bool]) = {
    val apis = new StringBuilder

    apis.append("\n  /*** API variables ***/\n")
    apis.append("  reg[20*8:0] cmd;    // API command\n")
    apis.append("  reg[1000*8:0] node; // Chisel node name;\n")
    apis.append("  reg[2047:0] value;   // 'poked' value\n")
    apis.append("  integer offset;     // mem's offset\n")
    apis.append("  integer steps;      // number of steps\n")
    apis.append("  integer delta;      // number of steps\n")
    apis.append("  integer min = (1 << 31 -1);\n")
    apis.append("  reg is_stale = 0;\n")

    apis.append("\n  integer count;\n")

    def fscanf(form: String, args: String*) =
      "count = $fscanf('h80000000, \"%s\", %s);\n".format(form, (args.tail foldLeft args.head) (_ + ", " + _))
    def display(form: String, args: String*) =
      "$display(\"%s\", %s);\n".format(form, (args.tail foldLeft args.head) (_ + ", " + _))

    apis.append("  initial begin\n")
    apis.append("  /*** API interpreter ***/\n")
    apis.append("  forever begin\n")
    for (rst <- resets)
      apis.append("    %s = 0;\n".format(rst.name))
    apis.append("    "+ fscanf("%s", "cmd"))
    apis.append("    case (cmd)\n")

    apis.append("      // < reset >\n")
    apis.append("      // inputs: # cycles the reset consumes\n")
    apis.append("      // return: none\n")
    apis.append("      \"reset\": begin\n")
    apis.append("        " + fscanf("%d", "steps"))
    for (rst <- resets)
      apis.append("        %s = 1;\n".format(rst.name))
    apis.append("        repeat (steps) @(negedge %s) begin\n".format(mainClk.name))
    apis.append("        delta = delta + min;\n")
    if (clocks.size > 1) {
      for (clk <- clocks)
        apis.append("      %s_fire = 1;\n".format(clk.name, clk.name))
    } 
    apis.append("        end")
    apis.append("        // return delta\n")
    apis.append("        " + display("%1d", "delta"))
    apis.append("        delta = 0;\n")
    for (rst <- resets)
      apis.append("        %s = 0;\n".format(rst.name))
    apis.append("      end\n")

    apis.append("      // < wire_peek >\n")
    apis.append("      // inputs: wire's name\n")
    apis.append("      // return: wire's value\n")
    apis.append("      \"wire_peek\": begin\n")
    apis.append("        if (is_stale) #0.1 is_stale = 0;\n")
    apis.append("        " + fscanf("%s", "node"))
    apis.append("        $wire_peek(node);\n")
    apis.append("      end\n")

    apis.append("      // < mem_peek >\n")
    apis.append("      // inputs: mem's name\n")
    apis.append("      // return: mem's value\n")
    apis.append("      \"mem_peek\": begin\n")
    apis.append("        if (is_stale) #0.1 is_stale = 0;\n")
    apis.append("        " + fscanf("%s %d", "node", "offset"))
    apis.append("        $mem_peek(node, offset);\n")
    apis.append("      end\n")

    apis.append("      // < wire_poke >\n")
    apis.append("      // inputs: wire's name\n")
    apis.append("      // return: \"ok\" or \"error\"\n")
    apis.append("      \"wire_poke\": begin\n")
    apis.append("        " + fscanf("%s 0x%x", "node", "value"))
    apis.append("        $wire_poke(node, value);\n")
    apis.append("        is_stale = 1;\n")
    apis.append("      end\n")

    apis.append("      // < mem_poke >\n")
    apis.append("      // inputs: wire's name\n")
    apis.append("      // return: \"ok\" or \"error\"\n")
    apis.append("      \"mem_poke\": begin\n")
    apis.append("        " + fscanf("%s %d 0x%x", "node", "offset", "value"))
    apis.append("        $mem_poke(node, offset, value);\n")
    apis.append("        is_stale = 1;\n")
    apis.append("      end\n")

    apis.append("      // < step > \n")
    apis.append("      // inputs: # cycles\n")
    apis.append("      // return: # cycles the target will proceed\n")
    apis.append("      \"step\": begin\n")
    apis.append("        " + fscanf("%d", "steps"))
    apis.append("        if (is_stale) #0.1 is_stale = 0;\n")
    apis.append("        repeat (steps) @(negedge %s) begin\n".format(mainClk.name))
    if (clocks.size > 1) {
      for (clk <- clocks)
        apis.append("          if (%s_length < min) min = %s_cnt;\n".format(clk.name, clk.name))
      for (clk <- clocks)
        apis.append("          %s_cnt = %s_cnt - min;\n".format(clk.name, clk.name))
      for (clk <- clocks) {
        apis.append("          if (%s_cnt == 0) %s_fire = 1;\n".format(clk.name, clk.name))
        apis.append("          else %s_fire = 0;\n".format(clk.name))
      }
      for (clk <- clocks)
        apis.append("          if (%s_cnt == 0) %s_cnt = %s_length;\n".format(clk.name, clk.name, clk.name))
    }
    apis.append("          delta = delta + min;\n")
    apis.append("        end\n")
    apis.append("        // return delta\n")
    apis.append("        " + display("%1d", "delta"))
    apis.append("        delta = 0;\n")
    apis.append("      end\n")

    if (clocks.size > 1) {
      apis.append("      // <set_clocks> \n")
      apis.append("      // inputs: clocks' length\n")
      apis.append("      // return: \"ok\" or \"error\"\n")
      apis.append("      \"set_clocks\": begin\n")
      val clkFormat = ((clocks filter (_.srcClock == None)).toList map (x => "%x"))
      val clkFires  = ((clocks filter (_.srcClock == None)) map (_.name + "_length")).toList
      apis.append("        " + fscanf((clkFormat foldLeft "")(_ + " " + _), clkFires:_*) )
      apis.append("        " + display("%s", "\"ok\""))
      for (clk <- clocks) {
        apis.append("        %s_cnt = %s_length;\n".format(clk.name, clk.name))
      }
      apis.append("      end\n")
    }

    apis.append("      // < quit>: finish simulation\n")
    apis.append("      \"quit\": $finish;\n")
    apis.append("      // default return: \"error\"\n")
    apis.append("      default: " + display("%s", "\"error\""))
    apis.append("    endcase\n")
    apis.append("    end\n\n")

    apis.append("    if (count == -1) $finish(1);\n\n")
    apis.append("  end\n\n")
    apis.result
  }

  def harnessBase (mainClk: Clock, resets: List[Bool], scanNodes: Array[Bits], printNodes: Array[Bits]) = {
    val base = new StringBuilder
    val printFormat = printNodes.map(a => a.chiselName + ": 0x%x, ").fold("")((y,z) => z + " " + y)
    val scanFormat = scanNodes.map(a => "%x").fold("")((y,z) => z + " " + y)

    if (!resets.isEmpty) {
      base.append("  parameter reset_period = `CLOCK_PERIOD * 4;\n")
      base.append("  initial begin\n")
      for (rst <- resets) base.append("    %s = 1;\n".format(rst.name))
      base.append("    #reset_period;\n")
      for (rst <- resets) base.append("    %s = 0;\n".format(rst.name))
      base.append("  end\n\n")
    }

    base.append("  always @(posedge %s) begin\n".format(mainClk.name))
    if (!resets.isEmpty)
      base.append("    if (%s)\n".format(
        (resets.tail foldLeft ("!" + resets.head.name))(_ + " || !" + _.name)))
    base.append("      $display(\"" + printFormat.slice(0,printFormat.length-1) + "\"")
    for (node <- printNodes) {
      base.append(", " + emitRef(node))
    }
    base.append(");\n")
    base.append("  end\n\n")

    base.result
  }

  // Is the specified node synthesizeable?
  // This could be expanded. For the moment, we're flagging unconnected Bits,
  // for which we generate un-synthesizable random values.
  def synthesizeable(node: Node): Boolean = {
    node match {
      case x: Bits =>
        if (x.isIo && x.dir == INPUT) {
          true
        } else if (node.inputs.length > 0) {
          true
        } else {
          false
        }
      case _ => true
    }
  }

  def emitDefs(c: Module): StringBuilder = {
    val resSimulate = new StringBuilder()
    val resSynthesis = new StringBuilder()
    val res = new StringBuilder()
    for (m <- c.nodes) {
      val resNode = if (synthesizeable(m)) {
        resSynthesis
      } else {
        resSimulate
      }
      resNode.append(emitDef(m))
    }
    // Did we generate any non-synthesizable definitions?
    if (resSimulate.length > 0) {
      res.append(if_not_synthesis)
      res ++= resSimulate
      res.append(endif_not_synthesis)
    }
    res ++= resSynthesis
    for (c <- c.children) {
      res.append(emitDef(c))
    }
    res
  }

  def emitRegs(c: Module): StringBuilder = {
    val res = new StringBuilder
    val clkDomains = (c.clocks map (_ -> new StringBuilder)).toMap
    if (Driver.isAssert) {
      c.asserts foreach (p => p.clock match {
        case Some(clk) if clkDomains contains clk =>
          clkDomains(clk) append emitAssert(p)
        case _ =>
      })
    }
    c.nodes foreach (m => m.clock match {
      case Some(clk) if clkDomains contains clk =>
        clkDomains(clk) append emitReg(m)
      case _ => 
    })
    c.printfs foreach (p => p.clock match {
      case Some(clk) if clkDomains contains clk => 
        clkDomains(clk) append emitPrintf(p)
      case _ =>
    })
    for ((clock, dom) <- clkDomains ; if !dom.isEmpty) {
      if (res.isEmpty) res.append("\n")
      res.append("  always @(posedge " + emitRef(clock) + ") begin\n")
      res.append(dom.result)
      res.append("  end\n")
    }
    res
  }

  def emitPrintf(p: Printf): String = {
    if_not_synthesis +
    "`ifdef PRINTF_COND\n" +
    "    if (`PRINTF_COND)\n" +
    "`endif\n" +
    "      if (" + emitRef(p.cond) + ")\n" +
    "        $fwrite(32'h80000002, " + p.args.map(emitRef _).foldLeft(CString(p.format))(_ + ", " + _) + ");\n" +
    endif_not_synthesis
  }
  def emitAssert(a: Assert): String = {
    if_not_synthesis +
    "  if(" + emitRef(a.reset) + ") " + emitRef(a) + " <= 1'b1;\n" +
    "  if(!" + emitRef(a.cond) + " && " + emitRef(a) + " && !" + emitRef(a.reset) + ") begin\n" +
    "    $fwrite(32'h80000002, " + CString("ASSERTION FAILED: %s\n") + ", " + CString(a.message) + ");\n" +
    "    $finish;\n" +
    "  end\n" +
    endif_not_synthesis
  }

  def emitReg(node: Node): String = {
    node match {
      case reg: Reg =>
        def cond(c: Node) = "if(" + emitRef(c) + ") begin"
        def uncond = "begin"
        def sep = "\n      "
        def assign(r: Reg, x: Node) = emitRef(r) + " <= " + emitRef(x) + ";\n"
        def traverseMuxes(r: Reg, x: Node): List[String] = x match {
          case m: Mux => (cond(m.inputs(0)) + sep + assign(r, m.inputs(1))) :: traverseMuxes(r, m.inputs(2))
          case _ => if (x eq r) Nil else List(uncond + sep + assign(r, x))
        }
        if (!reg.next.isInstanceOf[Mux]) "    " + assign(reg, reg.next)
        else "    " + traverseMuxes(reg, reg.next).reduceLeft(_ + "    end else " + _) + "    end\n"

      case m: MemWrite =>
        if (m.mem.isInline) {
          "    if (" + emitRef(m.cond) + ")\n" +
          "      " + emitRef(m.mem) + "[" + emitRef(m.addr) + "] <= " + emitRef(m.data) + ";\n"
        } else {
          ""
        }
      case _ =>
        ""
    }
  }

  def emitDecs(c: Module): StringBuilder =
    c.nodes.map(emitDec(_)).addString(new StringBuilder)

  def emitInits(c: Module): StringBuilder = {
    val sb = new StringBuilder
    c.nodes.map(emitInit(_)).addString(sb)

    val res = new StringBuilder
    if (!sb.isEmpty) {
      res append if_not_synthesis
      res append "  integer initvar;\n"
      res append "  initial begin\n"
      res append "    #0.002;\n"
      res append sb
      res append "  end\n"
      res append endif_not_synthesis
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
      res.append((c.clocks ++ c.resets.values.toList).map(x => "input " + emitRef(x)).reduceLeft(_ + ", " + _))
    val ports = new ArrayBuffer[StringBuilder]
    for ((n, w) <- c.wires) {
      // if(first && !hasReg) {first = false; nl = "\n"} else nl = ",\n";
      w match {
        case io: Bits => {
          val prune = if (io.prune && c != topMod) "//" else ""
          if (io.dir == INPUT) {
            ports += new StringBuilder(nl + "    " + prune + "input " +
                                       emitWidth(io) + " " + emitRef(io));
          } else if(io.dir == OUTPUT) {
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
    res.append(emitDecs(c));
    res.append("\n");
    res.append(emitInits(c));
    res.append("\n");
    res.append(emitDefs(c));
    res.append(emitRegs(c))
    res.append("endmodule\n\n");
    res.result();
  }

  def flushModules(
    defs: LinkedHashMap[String, LinkedHashMap[String,ArrayBuffer[Module]]],
    level: Int ): Unit =
  {
    for( (className, modules) <- defs ) {
      var index = 0
      for ( (text, comps) <- modules) {
        val moduleName = if( modules.size > 1 ) {
          className + "_" + index.toString;
        } else {
          className;
        }
        index = index + 1
        for( flushComp <- comps ) {
          if( flushComp.level == level && flushComp.moduleName == "") {
            flushComp.moduleName = moduleName
          }
        }
      /* XXX We write the module source text in *emitChildren* instead
             of here so as to generate a minimal "diff -u" with the previous
             implementation. */
      }
    }
  }


  def emitChildren(top: Module,
    defs: LinkedHashMap[String, LinkedHashMap[String, ArrayBuffer[Module] ]],
    out: java.io.FileWriter, depth: Int): Unit =
  {
    if (top.isInstanceOf[BlackBox])
      return

    // First, emit my children
    for (child <- top.children) {
      emitChildren(child, defs, out, depth + 1);
    }

    // Now, find and emit me
    // Note: emittedModules used to ensure modules only emitted once
    //    regardless of how many times used (e.g. when folded)
    val className = extractClassName(top);
    for{
      (text, comps) <- defs(className)
      if comps contains top
      if !(emittedModules contains top.moduleName)
    } {
      out.append(s"module ${top.moduleName}(")
      out.append(text);
      emittedModules += top.moduleName
      return
    }
  }


  def doCompile(top: Module, out: java.io.FileWriter, depth: Int): Unit = {
    /* *defs* maps Mod classes to Mod instances through
       the generated text of their module.
       We use a LinkedHashMap such that later iteration is predictable. */
    val defs = LinkedHashMap[String, LinkedHashMap[String, ArrayBuffer[Module]]]()
    var level = 0;
    for (c <- Driver.sortedComps) {
      ChiselError.info(depthString(depth) + "COMPILING " + c
        + " " + c.children.size + " CHILDREN"
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
        flushModules(defs, level);
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
    flushModules(defs, level);
    emitChildren(top, defs, out, depth);
  }

  override def elaborate(c: Module) {
    super.elaborate(c)
    // execute addBindings only in the Verilog Backend
    addBindings
    nameBindings
    findConsumers(c)

    val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName)
    val out = createOutputFile(n + ".v")
    doCompile(c, out, 0)
    ChiselError.checkpoint()
    out.close()

    if (!memConfs.isEmpty) {
      val out_conf = createOutputFile(n + ".conf")
      out_conf.write(getMemConfString);
      out_conf.close();
    }
    if (Driver.isGenHarness) {
      genHarness(c, n);
    }
  }

  override def compile(c: Module, flags: Option[String]) {
    def copyToTarget(filename: String) = {
          val resourceStream = getClass().getResourceAsStream("/" + filename)
          if( resourceStream != null ) {
            val classFile = createOutputFile(filename)
            while(resourceStream.available > 0) {
              classFile.write(resourceStream.read())
            }
            classFile.close()
            resourceStream.close()
          } else {
            println(s"WARNING: Unable to copy '$filename'" )
          }
    }
    copyToTarget("vpi_user.cc")
    val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName)
    def run(cmd: String): Boolean = {
      val bashCmd = Seq("bash", "-c", cmd)
      val c = bashCmd.!
      if (c == 0) {
        ChiselError.info(cmd + " RET " + c)
        true
      } else {
        ChiselError.error(cmd + " RET " + c)
        false
      }
    }
    val dir = Driver.targetDir + "/"
    val src = n + "-harness.v " + n + ".v"
    val cmd =  "cd " + dir + " && vcs -full64 -quiet +v2k -Mdir=" + n + ".csrc " +
              "-timescale=1ns/1ps +define+CLOCK_PERIOD=120 +vpi -use_vpiobj vpi_user.cc " +
              "+vcs+initreg+random " + src + " -o " + n + " -debug_pp"
    if (!run(cmd)) {
      throwException("vcs command failed")
    }
  }

  private def if_not_synthesis = "`ifndef SYNTHESIS\n// synthesis translate_off\n"
  private def endif_not_synthesis = "// synthesis translate_on\n`endif\n"
}

