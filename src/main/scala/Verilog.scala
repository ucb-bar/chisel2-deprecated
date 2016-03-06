/*
 Copyright (c) 2011 - 2016 The Regents of the University of
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
}

class VerilogBackend extends Backend {
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
    if (w == 1) "" else s"[${w-1}:0]"
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
    "{" + ((node.needWidth() + 31)/32) + "{$random}}"

  def emitPortDef(m: MemAccess, idx: Int): String = {
    def str(prefix: String, ports: (String, Option[String])*): String =
      ports.toList filter (_._2 != None) map {
        case (x, y) => List("    .", prefix, idx, x, "(", y.get, ")").mkString
      } mkString ",\n"

    m match {
      case r: MemSeqRead =>
        val addr = ("A", Some(emitRef(r.addr)))
        val en = ("E", Some(emitRef(r.cond)))
        val out = ("O", Some(emitTmp(r)))
        str("R", addr, en, out)

      case w: MemWrite =>
        val addr = ("A", Some(emitRef(w.addr)))
        val en = ("E", Some(emitRef(w.cond)))
        val data = ("I", Some(emitRef(w.data)))
        val mask = ("M", if (w.isMasked) Some(emitRef(w.mask)) else None)
        str("W", addr, en, data, mask)

      case rw: MemReadWrite =>
        val (r, w) = (rw.read, rw.write)
        val addr = ("A", Some(emitRef(w.cond) + " ? " + emitRef(w.addr) + " : " + emitRef(r.addr)))
        val en = ("E", Some(emitRef(r.cond) + " || " + emitRef(w.cond)))
        val write = ("W", Some(emitRef(w.cond)))
        val data = ("I", Some(emitRef(w.data)))
        val mask = ("M", if (w.isMasked) Some(emitRef(w.mask)) else None)
        val out = ("O", Some(emitTmp(r)))
        str("RW", addr, en, write, data, mask, out)
    }
  }

  // Add trailing comma to port declarations.
  def delimitUncommentedPortDecls(portDecls: ArrayBuffer[StringBuilder]) {
    val uncommentedPorts = portDecls.filter(!_.result.contains("//"))
    if (!uncommentedPorts.isEmpty) {
      uncommentedPorts.init map (_ append ",")
    }
  }

  def emitDef(c: Module): StringBuilder = {
    val spacing = (if(c.verilog_parameters != "") " " else "")
    var res = new StringBuilder
    List("  ", c.moduleName, " ", c.verilog_parameters, spacing, c.name, "(") addString res
    def getMapClk(x : Clock) : String = if ( c.isInstanceOf[BlackBox] ) c.asInstanceOf[BlackBox].mapClock(emitRef(x)) else emitRef(x)
    c.clocks map (x => "." + getMapClk(x) + "(" + emitRef(x) + ")") addString (res, ", ")
    if (!c.clocks.isEmpty && !c.resets.isEmpty) res append ", "
    c.resets.values map (x => "." + emitRef(x) + "(" + emitRef(x.inputs(0)) + ")") addString (res, ", ")
    var isFirst = true
    val portDecs = ArrayBuffer[StringBuilder]()
    for ((n, io) <- c.wires if n != "reset" && n != Driver.implicitReset.name) {
      val portDec = new StringBuilder("." + n + "( ")
      io.dir match {
        case INPUT if io.inputs.isEmpty =>
          // if (Driver.saveConnectionWarnings) {
          //   ChiselError.warning("" + io + " UNCONNECTED IN " + io.component)
          // } removed this warning because pruneUnconnectedIOs should have picked it up
          portDec insert (0, "//")
        case INPUT if io.inputs.size > 1 =>
          if (Driver.saveConnectionWarnings) {
            ChiselError.warning("" + io + " CONNECTED TOO MUCH " + io.inputs.length)
          }
          portDec insert (0, "//")
        /* case INPUT if !(c.isWalked conatins io) =>
          if (Driver.saveConnectionWarnings) {
            ChiselError.warning(" UNUSED INPUT " + io + " OF " + c + " IS REMOVED")
          }
          portDec = "//" + portDec // I don't think this is necessary */
        case INPUT =>
          portDec append emitRef(io.inputs(0))
        case OUTPUT if io.consumers.isEmpty =>
          // if (Driver.saveConnectionWarnings) {
          //   ChiselError.warning("" + io + " UNCONNECTED IN " + io.component + " BINDING " + c.findBinding(io))
          // } removed this warning because pruneUnconnectedsIOs should have picked it up
          portDec insert (0, "//")
        case OUTPUT => c.parent.findBinding(io) match {
          case None =>
            if (Driver.saveConnectionWarnings) {
              ChiselError.warning("" + io + "(" + io.component + ") OUTPUT UNCONNECTED (" +
                                  io.consumers.size + ") IN " + c.parent) }
            portDec insert (0, "//")
          case Some(consumer) =>
            if (io.prune) portDec insert (0, "//")
            portDec append emitRef(consumer)
        }
      }
      portDec append " )"
      portDecs += portDec
    }
    // Add trailing ',' to uncommented portDecs
    delimitUncommentedPortDecls(portDecs)
    portDecs map (_ insert (0, "       "))
    if (!c.clocks.isEmpty || !c.resets.isEmpty) res append ",\n" else res append "\n"
    res append (portDecs addString (new StringBuilder, "\n"))
    res append "\n  );\n"
    val driveRandPorts = c.wires filter (_._2.driveRand)
    if (!driveRandPorts.isEmpty) res append if_not_synthesis
    driveRandPorts map { case (n ,w) =>
      List("    assign ", c.name, ".", n, " = ", emitRand(w), ";\n").mkString } addString res
    if (!driveRandPorts.isEmpty) res append endif_not_synthesis
    res
  }

  override def emitDef(node: Node): String = {
    val res = node match {
      case x: Bits if x.isIo && x.dir == INPUT => ""
      case x: Bits if node.inputs.isEmpty =>
        if (Driver.saveConnectionWarnings) ChiselError.warning("UNCONNECTED " + node + " IN " + node.component)
        List("  assign ", emitTmp(node), " = ", emitRand(node), ";\n").mkString
      case x: Bits =>
        List("  assign ", emitTmp(node), " = ", emitRef(node.inputs(0)), ";\n").mkString

      case x: Mux =>
        List("  assign ", emitTmp(x), " = ", emitRef(x.inputs(0)), " ? ", emitRef(x.inputs(1)), " : ", emitRef(x.inputs(2)), ";\n").mkString

      case o: Op if o.op == "##" =>
        List("  assign ", emitTmp(o), " = ", "{", emitRef(node.inputs(0)), ", ", emitRef(node.inputs(1)), "}", ";\n").mkString
      case o: Op if o.inputs.size == 1 =>
        List("  assign ", emitTmp(o), " = ", o.op, " ", emitRef(node.inputs(0)), ";\n").mkString
      case o: Op if o.op == "s*s" || o.op == "s*u" || o.op == "s%s" || o.op == "s/s" =>
        List("  assign ", emitTmp(o), " = ", "$signed(", emitRef(node.inputs(0)), ") ", o.op(1), " $signed(", emitRef(node.inputs(1)), ")", ";\n").mkString
      case o: Op if o.op == "s<" || o.op == "s<=" =>
        List("  assign ", emitTmp(o), " = ", "$signed(", emitRef(node.inputs(0)), ") ", o.op.tail, " $signed(", emitRef(node.inputs(1)), ")", ";\n").mkString
      case o: Op if o.op == "s>>" =>
        List("  assign ", emitTmp(o), " = ", "$signed(", emitRef(node.inputs(0)), ") >>> ", emitRef(node.inputs(1)), ";\n").mkString
      case o: Op =>
        List("  assign ", emitTmp(o), " = ", emitRef(node.inputs(0)), " ", o.op, " ", emitRef(node.inputs(1)), ";\n").mkString

      case x: Extract =>
        node.inputs.tail.foreach(x.validateIndex)
        val extractWidth = node.inputs(0).needWidth()
        val source = emitRef(node.inputs(0))
        val hi = emitRef(node.inputs(1))
        List("  assign " + emitTmp(node) + " = ",
          // Is this a no-op - extract all the source bits.
          if (x.isNop) {
            source
          } else
          // Is this a single bit extraction?
            if (x.isOneBit) {
            List(source, "[", hi, "]").mkString
          } else {
            // We have three inputs.
            val lo = emitRef(node.inputs(2))
            // Are hi and lo constant expressions?
            if (x.isStaticWidth) {
              List(source, "[", hi , ":", lo, "]").mkString
            } else {
              // The extraction operands are different and at least one of them is not an integer.
              // We need to generate the equivalent sequence of shifts and masks.
              val resultWidth = node.needWidth
              List( "(", source, " >> ", lo, ") & ((", resultWidth, "'h1 << (", hi, " - ", lo, " + 1)) - 1)").mkString
           }
         }
        , ";\n").mkString

      case m: Mem[_] if !m.isInline =>
        def gcd(a: Int, b: Int) : Int = { if(b == 0) a else gcd(b, a%b) }
        def find_gran(x: Node) : Int = x match {
          case _: Literal => x.needWidth()
          case _: UInt => if (x.inputs.isEmpty) 1 else find_gran(x.inputs(0))
          case _: Mux => gcd(find_gran(x.inputs(1)), find_gran(x.inputs(2)))
          case _: Op => x.inputs map (find_gran(_)) reduceLeft (_ max _)
          case _ => 1
        }
        val mask_writers = m.writeAccesses.filter(_.isMasked)
        val mask_grans = mask_writers.map(x => find_gran(x.mask))
        val mask_gran = if (!mask_grans.isEmpty && mask_grans.forall(_ == mask_grans(0))) mask_grans(0) else 1
        val configStr = List(
         " depth ", m.n,
         " width ", m.needWidth(),
         " ports ", m.ports map (_.getPortType) mkString ",",
         (if (mask_gran != 1) " mask_gran " + mask_gran else ""), "\n").mkString
        val name = getMemName(m, configStr)
        ChiselError.info("MEM " + name)

        val clk = List("    .CLK(", emitRef(m.clock.get), ")").mkString
        val portdefs = m.ports.zipWithIndex map { case (p, i) => emitPortDef(p, i) }
        List("  ", name, " ", emitRef(m), " (\n", (clk +: portdefs) mkString ",\n", "\n", "  );\n").mkString

      case m: MemRead if m.mem.isInline =>
        List("  assign ", emitTmp(node), " = ", emitRef(m.mem), "[", emitRef(m.addr), "];\n").mkString

      case r: ROMRead =>
        val inits = r.rom.sparseLits map { case (i, v) =>
          s"    ${i}: ${emitRef(r)} = ${emitRef(v)};\n" } addString (new StringBuilder)
        (List(s"  always @(*) case (${emitRef(r.inputs.head)})\n",
        inits.result,
        s"    default: begin\n",
        s"      ${emitRef(r)} = ${r.needWidth()}'bx;\n",
        if_not_synthesis,
        s"      ${emitRef(r)} = ${emitRand(r)};\n",
        endif_not_synthesis,
        s"    end\n",
        "  endcase\n") addString (new StringBuilder)).result

      case s: Sprintf =>
        List("  always @(*) $sformat(", emitTmp(s), ", ", (CString(s.format) +: (s.args map (emitRef _))) mkString ", ", ");\n").mkString

      case _ =>
        ""
    }
    List(if (node.prune && res != "") "//" else "", res).mkString
  }

  def emitDecBase(node: Node, wire: String = "wire"): String =
    s"  ${wire}${emitWidth(node)} ${emitRef(node)};\n"

  def emitDecReg(node: Node): String = emitDecBase(node, "reg ")

  override def emitDec(node: Node): String = {
    val gotWidth = node.needWidth()
    val res =
    node match {
      case x: Bits if x.isIo => ""

      case _: Assert =>
        emitDecReg(node)

      case _: Reg =>
        emitDecReg(node)

      case _: Sprintf =>
        emitDecReg(node)

      case _: ROMRead =>
        emitDecReg(node)

      case m: Mem[_] if !m.isInline => ""
      case m: Mem[_] =>
        s"  reg ${emitWidth(m)} ${emitRef(m)} [${m.n-1}:0];\n"

      case x: MemAccess =>
        x.referenced = true
        emitDecBase(node)

      case _: ROMData => ""

      case _: Literal => ""

      case _ =>
        emitDecBase(node)
    }
    List(if (node.prune && res != "") "//" else "", res).mkString
  }

  def emitInit(node: Node): String = node match {
    case r: Reg =>
      List("    ", emitRef(r), " = ", emitRand(r), ";\n").mkString
    case m: Mem[_] if m.isInline =>
      "    for (initvar = 0; initvar < " + m.n + "; initvar = initvar+1)\n" +
      "      " + emitRef(m) + "[initvar] = " + emitRand(m) + ";\n"
    case a: Assert =>
      "    " + emitRef(a) + " = 1'b0;\n"
    case _ =>
      ""
  }

  def genHarness(c: Module, name: String) {
    val harness  = createOutputFile(name + "-harness.v")
    val ins = for ((n, io) <- c.wires if io.dir == INPUT) yield io
    val outs = for ((n, io) <- c.wires if io.dir == OUTPUT) yield io
    val mainClk = Driver.implicitClock
    val clocks = Driver.clocks
    val resets = c.resets.values.toList

    harness write "module test;\n"
    ins foreach (node => harness write "  reg[%d:0] %s = 0;\n".format(node.needWidth()-1, emitRef(node)))
    outs foreach (node => harness write "  wire[%d:0] %s;\n".format(node.needWidth()-1, emitRef(node)))
    clocks foreach (clk => harness write "  reg %s = 0;\n".format(clk.name))
    resets foreach (rst => harness write "  reg %s = 1;\n".format(rst.name))
    clocks foreach (clk => harness write "  integer %s_len;\n".format(clk.name))
    clocks foreach (clk => harness write "  always #%s_len %s = ~%s;\n".format(clk.name, clk.name, clk.name))
    if (clocks.size > 1) {
      harness write "  integer min = 1 << 31 - 1;\n\n"
      clocks foreach (clk => harness write "  integer %s_cnt;\n".format(clk.name))
    }
    harness write "  reg vcdon = 0;\n"
    harness write "  reg [1023:0] vcdfile = 0;\n"
    harness write "  reg [1023:0] vpdfile = 0;\n"

    harness write "\n  /*** DUT instantiation ***/\n"
    harness write "  %s %s(\n".format(c.moduleName, c.name)
    c.clocks foreach (clk => harness write "    .%s(%s),\n".format(clk.name, clk.name))
    resets   foreach (rst => harness write "    .%s(%s),\n".format(rst.name, rst.name))

    harness write ((ins ++ outs) map (node => "    .%s(%s)".format(emitRef(node), emitRef(node))) mkString ",\n")
    harness write ");\n\n"

    harness write "  initial begin\n"
    clocks foreach (clk => clk.srcClock match {
      case None => harness write "    %s_len = `CLOCK_PERIOD;\n".format(clk.name)
      case Some(src) =>
        val initStr = "%s_len".format(src.name) + (if (src.period > clk.period)
          " / " + (src.period / clk.period).round else
          " * " + (clk.period / src.period).round)
        harness write "    %s_len = %s;\n".format(clk.name, initStr)
    })
    if (clocks.size > 1) clocks foreach (clk => harness write "    %s_cnt = %s_len;\n".format(clk.name, clk.name))
    harness write "    $init_clks(" + (clocks map (_.name + "_len") mkString ", ") + ");\n"
    harness write "    $init_rsts(" + (resets map (_.name) mkString ", ") + ");\n"
    harness write "    $init_ins(" + (ins map (emitRef(_)) mkString ", ") + ");\n"
    harness write "    $init_outs(" + (outs map (emitRef(_)) mkString ", ") + ");\n"
    harness write "    $init_sigs(%s);\n".format(c.name)

    harness write "    /*** VCD & VPD dump ***/\n"
    harness write "    if ($value$plusargs(\"vcdfile=%s\", vcdfile)) begin\n"
    harness write "      $dumpfile(vcdfile);\n"
    harness write "      $dumpvars(0, %s);\n".format(c.name)
    harness write "      $dumpoff;\n"
    harness write "      vcdon = 0;\n"
    harness write "    end\n"
    harness write "    if ($value$plusargs(\"vpdfile=%s\", vpdfile)) begin\n"
    harness write "      $vcdplusfile(vpdfile);\n"
    harness write "      $vcdpluson(0);\n"
    harness write "      $vcdplusautoflushon;\n"
    harness write "    end\n"
    harness write "    if ($test$plusargs(\"vpdmem\")) begin\n"
    harness write "      $vcdplusmemon;\n"
    harness write "    end\n"
    harness write "  end\n\n"

    if (clocks.size > 1) {
      harness write "  initial forever begin\n"
      clocks foreach (clk => harness write "    if (%s_cnt < min) min = %s_cnt;\n".format(clk.name, clk.name))
      clocks foreach (clk => harness write "    %s_cnt = %s_cnt - min;\n".format(clk.name, clk.name))
      clocks foreach (clk => harness write "    if (%s_cnt == 0) %s_cnt = %s_len;\n".format(clk.name, clk.name, clk.name))
      harness write "    #min $tick();\n"
      if (!resets.isEmpty) {
        harness write s"""    if (vcdfile && (${resets map (_.name) mkString " || "})) begin\n"""
        harness write "      $dumpoff;\n"
        harness write "      vcdon = 0;\n"
        harness write "    end\n"
        harness write "    else if (vcdfile && !vcdon) begin\n"
        harness write "      $dumpon;\n"
        harness write "      vcdon = 1;\n"
        harness write "    end\n"
      }
      harness write "    #min ;\n"
      harness write "  end\n"
    } else {
      harness write "  always @(negedge %s) begin\n".format(mainClk.name)
      harness write "    $tick();\n".format(mainClk.name)
      if (!resets.isEmpty) {
        harness write s"""    if (vcdfile && (${resets map (_.name) mkString " || "})) begin\n"""
        harness write "      $dumpoff;\n"
        harness write "      vcdon = 0;\n"
        harness write "    end\n"
        harness write "    else if (vcdfile && !vcdon) begin\n"
        harness write "      $dumpon;\n"
        harness write "      vcdon = 1;\n"
        harness write "    end\n"
      }
      harness write "  end\n\n"
    }
    harness write "endmodule\n"

    harness.close
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
      res.append(dom)
      res.append("  end\n")
    }
    res
  }

  def emitPrintf(p: Printf): String = {
    val file = "32'h80000002"
    (List(if_not_synthesis,
    "`ifdef PRINTF_COND\n",
    "    if (`PRINTF_COND)\n",
    "`endif\n",
    "      if (", emitRef(p.cond), ")\n",
    "        $fwrite(", file, ", ", (CString(p.format) +: (p.args map (emitRef _))) mkString ", ", ");\n",
    endif_not_synthesis) addString (new StringBuilder)).result
  }
  def emitAssert(a: Assert): String = {
    val file = "32'h80000002"
    (List(if_not_synthesis,
    "  if(", emitRef(a.reset), ") ", emitRef(a), " <= 1'b1;\n",
    "  if(!", emitRef(a.cond), " && ", emitRef(a), " && !", emitRef(a.reset), ") begin\n",
    "    $fwrite(", file, ", ", CString("ASSERTION FAILED: %s\n"), ", ", CString(a.message), ");\n",
    "    $finish;\n",
    "  end\n",
    endif_not_synthesis) addString (new StringBuilder)).result
  }

  def emitReg(node: Node): String = {
    node match {
      case reg: Reg =>
        def cond(c: Node) = List("if(", emitRef(c), ") begin").mkString
        def uncond = "begin"
        def sep = "\n      "
        def assign(r: Reg, x: Node) = List(emitRef(r), " <= ", emitRef(x), ";\n").mkString
        def traverseMuxes(r: Reg, x: Node): List[String] = x match {
          case m: Mux => (cond(m.inputs(0)) + sep + assign(r, m.inputs(1))) :: traverseMuxes(r, m.inputs(2))
          case _ => if (x eq r) Nil else List(uncond + sep + assign(r, x))
        }
        reg.next match {
          case _: Mux =>
            List("    ", traverseMuxes(reg, reg.next) mkString "    end else ", "    end\n").mkString
          case _ =>
            List("    ", assign(reg, reg.next)).mkString
        }

      case m: MemWrite if m.mem.isInline => List(
        "    if (", emitRef(m.cond), ")\n",
        "      ", emitRef(m.mem), "[", emitRef(m.addr), "] <= ", emitRef(m.data), ";\n").mkString

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

  def emitModuleText(c: Module): StringBuilder = c match {
    case _: BlackBox => new StringBuilder
    case _ =>

    val res = new StringBuilder()
    var first = true
    (c.clocks ++ c.resets.values.toList) map ("input " + emitRef(_)) addString (res, ", ")
    val ports = ArrayBuffer[StringBuilder]()
    for ((n, io) <- c.wires) {
      val prune = if (io.prune && c != topMod) "//" else ""
      io.dir match {
        case INPUT =>
          ports += List("    ", prune, "input ", emitWidth(io), " ", emitRef(io)) addString (new StringBuilder)
        case OUTPUT =>
          ports += List("    ", prune, "output", emitWidth(io), " ", emitRef(io)) addString (new StringBuilder)
      }
    }
    // Add trailing ',' to uncommented ports
    delimitUncommentedPortDecls(ports)
    if (!c.clocks.isEmpty || !c.resets.isEmpty) res.append(",\n") else res.append("\n")
    res.append(ports addString (new StringBuilder, "\n"))
    res.append("\n);\n\n")
    res.append(emitDecs(c))
    res.append("\n")
    res.append(emitInits(c))
    res.append("\n")
    res.append(emitDefs(c))
    res.append(emitRegs(c))
    res.append("endmodule\n\n")
    res
  }

  def flushModules(
    defs: LinkedHashMap[String, LinkedHashMap[StringBuilder,ArrayBuffer[Module]]],
    level: Int ): Unit =
  {
    for( (className, modules) <- defs ) {
      var index = 0
      for ( (text, comps) <- modules) {
        val moduleName = if( modules.size > 1 ) {
          className + "_" + index.toString
        } else {
          className
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
    defs: LinkedHashMap[String, LinkedHashMap[StringBuilder, ArrayBuffer[Module] ]],
    out: java.io.FileWriter, depth: Int): Unit = top match {
    case _: BlackBox =>
    case _ =>

    // First, emit my children
    for (child <- top.children) {
      emitChildren(child, defs, out, depth + 1)
    }

    // Now, find and emit me
    // Note: emittedModules used to ensure modules only emitted once
    //    regardless of how many times used (e.g. when folded)
    val className = extractClassName(top)
    for{
      (text, comps) <- defs(className)
      if comps contains top
      if !(emittedModules contains top.moduleName)
    } {
      out.append(s"module ${top.moduleName}(")
      out.append(text)
      emittedModules += top.moduleName
      return
    }
  }


  def doCompile(top: Module, out: java.io.FileWriter, depth: Int): Unit = {
    /* *defs* maps Mod classes to Mod instances through
       the generated text of their module.
       We use a LinkedHashMap such that later iteration is predictable. */
    val defs = LinkedHashMap[String, LinkedHashMap[StringBuilder, ArrayBuffer[Module]]]()
    var level = 0
    for (c <- Driver.sortedComps) {
      ChiselError.info(genIndent(depth) + "COMPILING " + c
        + " " + c.children.size + " CHILDREN"
        + " (" + c.level + "," + c.traversal + ")")
      ChiselError.checkpoint()

      if( c.level > level ) {
        /* When a component instance instantiates different sets
         of sub-components based on its constructor parameters, the same
         Module class might appear with different level in the tree.
         We thus wait until the very end to generate module names.
         If that were not the case, we could flush modules as soon as
         the source text for all components at a certain level in the tree
         has been generated. */
        flushModules(defs, level)
        level = c.level
      }
      val res = emitModuleText(c)
      val className = extractClassName(c)
      if( !(defs contains className) ) {
        defs += (className -> LinkedHashMap[StringBuilder, ArrayBuffer[Module] ]())
      }
      if( defs(className) contains res ) {
        /* We have already outputed the exact same source text */
        defs(className)(res) += c
        ChiselError.info("\t" + defs(className)(res).length + " components")
      } else {
        defs(className) += (res -> ArrayBuffer[Module](c))
      }
    }
    flushModules(defs, level)
    emitChildren(top, defs, out, depth)
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
      out_conf.write(getMemConfString)
      out_conf.close()
    }
    if (Driver.isGenHarness) {
      genHarness(c, n)
      copyToTarget("sim_api.h")
      copyToTarget("vpi.h")
      copyToTarget("vpi.cpp")
      copyToTarget("vpi.tab")
    }
  }

  override def compile(c: Module, flags: Option[String]) {
    val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName)
    val dir = Driver.targetDir + "/"
    val ccFlags = List("-I$VCS_HOME/include", "-I" + dir, "-fPIC", "-std=c++11") mkString " "
    val vcsFlags = List("-full64", "-quiet", "-timescale=1ns/1ps", "-debug_pp", "-Mdir=" + n + ".csrc",
      "-licwait 5",
      "+v2k", "+vpi", "+define+CLOCK_PERIOD=1", "+vcs+initreg+random") mkString " "
    val vcsSrcs = List(n + ".v", n + "-harness.v") mkString " "
    val cmd = List("cd", dir, "&&", "vcs", vcsFlags, "-P", "vpi.tab", "vpi.o", "-o", n, vcsSrcs) mkString " "
    cc(dir, "vpi", ccFlags)
    if (!run(cmd)) throw new RuntimeException("vcs command failed")
  }

  private def if_not_synthesis = "`ifndef SYNTHESIS\n// synthesis translate_off\n"
  private def endif_not_synthesis = "// synthesis translate_on\n`endif\n"
}

