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
import Node._
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
import scala.collection.immutable.ListSet

object LoFIRRTLBackend {

  val keywords = Set[String](
    "circuit", "module", "extmodule", "input", "output", "UInt", "SInt", "Clock", "default", "reverse", "wire", "reg", "smem", "cmem", "inst", "node", "accessor", "onreset", "through", "when", "else", "assert", "skip", "infer", "read", "write", "rdwr", "add", "sub", "addw", "subw", "mul", "div", "mod", "quo", "rem", "lt", "Leq", "gt", "geq", "eq", "neq", "mux", "pad", "asUInt", "asSInt", "shl", "shr", "dshl", "dshr", "cvt", "neg", "not", "and", "or", "xor", "andr", "orr", "xorr", "cat", "bit", "bits",
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

class LoFIRRTLBackend extends Backend {
  val keywords = LoFIRRTLBackend.keywords
  override val needsLowering = Set("PriEnc", "OHToUInt", "Log2")

  override def isEmittingComponents: Boolean = true

  val emittedModules = HashSet[String]()

  val memConfs = HashMap[String, String]()
  val compIndices = HashMap[String, Int]()
  val memDec = new StringBuilder()

  private def getMemConfString: String =
    memConfs.map { case (conf, name) => "name " + name + " " + conf } reduceLeft(_ + _)

          //val name = getMemName( m, m.n,       m.needWidth(),m.ports,                        mask_gran)
  private def getMemName(mem: Mem[_], depth:Int, width:Int, ports:ArrayBuffer[_ <: MemAccess], mask_gran:Int): String = {
    val configStr =
    (" depth " + depth +
      " width " + width +
      " ports " + ports.map(_.getPortType).reduceLeft(_ + "," + _) +
      (if (mask_gran != 1) " mask_gran " + mask_gran else "") +
      "\n")
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
        candidateName + "__" + count
      } else {
        compIndices += (candidateName -> 0)
        candidateName
      }
      memConfs += (configStr -> memModuleName)
      memDec.append(emitMem(memModuleName,ports))
    }
    memConfs(configStr)
  }

  def emitMem(name:String,ports:ArrayBuffer[_ <: MemAccess]): String = {
    val res = new StringBuilder()
    res.append("  extmodule " + name + " :\n")
    res.append("    input CLK : Clock\n")
    for (i <- 0 until ports.size)
      res.append(emitMemPort(ports(i), i))
    res.result()
  }

  def emitMemPort(port:MemAccess,idx:Int) : String = {
    def str(prefix: String, ports: (String, String)*): String =
      ports.toList.filter(_._2 != null)
        .map(p => "    " + p._1 + " " + prefix + idx + p._2 + " : UInt" + "\n")
        .reduce(_ + _)

    val in = "input"
    val ou = "output"
    port match {
      case r: MemSeqRead =>
        val addr = (in, "A")
        val en = (in,"E")
        val out = (ou,"O")
        str("R", addr, en, out)

      case w: MemWrite =>
        val addr = (in,"A")
        val en = (in,"E")
        val data = (in,"I")
        val mask = (in, if (w.isMasked) "M" else null)
        str("W", addr, en, data, mask)

      case rw: MemReadWrite =>
        val (r, w) = (rw.read, rw.write)
        val addr = (in,"A")
        val en = (in,"E")
        val write = (in,"W")
        val data = (in,"I")
        val mask = (in, if (w.isMasked) "M" else null)
        val out = (ou,"O")
        str("RW", addr, en, write, data, mask, out)
    }
  }

  def emitWidth(node: Node): String = {
    val w = node.needWidth()
    "<" + (w) + ">"
  }

  override def emitTmp(node: Node): String =
    emitRef(node)

  override def emitRef(node: Node): String = {
    node match {
      case x: Literal => emitLit(x.value, x.needWidth())
      case _: Reg =>
        if (node.name != "") asValidName(node.name) else "R" + node.emitIndex
      case _ =>
        if (node.name != "") asValidName(node.name) else "T" + node.emitIndex
    }
  }

  private def emitLit(x: BigInt): String =
    emitLit(x, x.bitLength + (if (x < 0) 1 else 0))
  private def emitLit(x: BigInt, w: Int): String = {
    val unsigned = if (x < 0) (BigInt(1) << w) + x else x
    require(unsigned >= 0)
    "UInt<" + w + ">(" + unsigned.toString(10) + ")"
  }

  // $random only emits 32 bits; repeat its result to fill the Node
  private def emitRand(node: Node): String =
    "UInt<1>(0)"
    //"{" + ((node.needWidth()+31)/32) + "{$random}}"

  def emitPortDef(m: MemAccess, name:String, idx: Int): String = {
    def str(prefix: String, ports: (String, String, Boolean)*): String =
      ports.toList.filter(_._2 != null)
        .map(p => 
          if (p._3) {
            "    " + name + "." + prefix + idx + p._1 + " := " + p._2 + "\n"
          } else {
            "    " + p._2 + " := " + name + "." + prefix + idx + p._1 + "\n"
          }).reduce(_ + _)

    m match {
      case r: MemSeqRead =>
        val addr = ("A", emitRef(r.addr),true)
        val en = ("E", emitRef(r.cond),true)
        val out = ("O",emitTmp(r),false)
        str("R", addr, en, out)

      case w: MemWrite =>
        val addr = ("A", emitRef(w.addr),true)
        val en = ("E", emitRef(w.cond),true)
        val data = ("I", emitRef(w.data),true)
        val mask = ("M", if (w.isMasked) emitRef(w.mask) else null,true)
        str("W", addr, en, data, mask)

      case rw: MemReadWrite =>
        val (r, w) = (rw.read, rw.write)
        val addr = ("A", "mux(" + emitRef(w.cond) + "," + emitRef(w.addr) + ", " + emitRef(r.addr) + ")",true)
        val en = ("E", "or(" + emitRef(r.cond) + ", " + emitRef(w.cond) + ")",true)
        val write = ("W", emitRef(w.cond),true)
        val data = ("I", emitRef(w.data),true)
        val mask = ("M", if (w.isMasked) emitRef(w.mask) else null,true)
        val out = ("O",emitTmp(r),false)
        str("RW", addr, en, write, data, mask, out)
    }

    //m match {
    //  case r: MemSeqRead =>
    //    val addr = ("A", emitRef(r.addr))
    //    val en = ("E", emitRef(r.cond))
    //    val out = ("O", emitTmp(r))
    //    str("R", addr, en, out)

    //  case w: MemWrite =>
    //    val addr = ("A", emitRef(w.addr))
    //    val en = ("E", emitRef(w.cond))
    //    val data = ("I", emitRef(w.data))
    //    val mask = ("M", if (w.isMasked) emitRef(w.mask) else null)
    //    str("W", addr, en, data, mask)

    //  case rw: MemReadWrite =>
    //    val (r, w) = (rw.read, rw.write)
    //    val addr = ("A", emitRef(w.cond) + " ? " + emitRef(w.addr) + " : " + emitRef(r.addr))
    //    val en = ("E", emitRef(r.cond) + " || " + emitRef(w.cond))
    //    val write = ("W", emitRef(w.cond))
    //    val data = ("I", emitRef(w.data))
    //    val mask = ("M", if (w.isMasked) emitRef(w.mask) else null)
    //    val out = ("O", emitTmp(r))
    //    str("RW", addr, en, write, data, mask, out)
    //}
  }

  def emitDef(c: Module): String = {
    val spacing = (if(c.verilog_parameters != "") " " else "");
    var res = "    inst " + c.name + " of " + c.moduleName + "\n";
    if (c.clocks.length > 0) {
      res = res + (c.clocks).map(x => "    " + c.name + "." + emitRef(x) + " := " + emitRef(x) + "\n").reduce(_ + _)
    }
    if (c.resets.size > 0 ) {
      res = res + (c.resets.values.toList).map(x => "    " + c.name + "." + emitRef(x) + " := " + emitRef(x.inputs(0)) + "\n").reduce(_ + _)
    }
    var isFirst = true;
    val portDecs = new ArrayBuffer[StringBuilder]
    for ((n, w) <- c.wires) {
      if(n != "reset" && n != Driver.implicitReset.name) {
        var portDec = "";
        w match {
          case io: Bits  =>
            if (io.dir == INPUT) { // if reached, then input has consumers
              if (io.inputs.length == 0) {
                //portDec = ";" + portDec
                portDec = portDec + c.name + "." + asValidName(n) + " := " + emitRand(io)
              } else if (io.inputs.length > 1) {
                  if (Driver.saveConnectionWarnings) {
                    ChiselError.warning("" + io + " CONNECTED TOO MUCH " + io.inputs.length);
                  }
                //portDec = ";" + portDec
              } else {
                portDec = portDec + c.name + "." + asValidName(n) + " := " + emitRef(io.inputs(0))
              }
            } else if(io.dir == OUTPUT) {
              //if (io.consumers.size == 0) {
                //portDec = ";" + portDec

              //} else {
                c.parent.findBinding(io) match {
                  case None => {
                    if (Driver.saveConnectionWarnings) {
                      ChiselError.warning("" + io + "(" + io.component + ") OUTPUT UNCONNECTED (" + 
                                          io.consumers.size + ") IN " + c.parent)
                    }
                    //portDec = ";" + portDec
                  }
                  case Some(consumer) => {
                    //if (io.prune)
                      //portDec = ";" + portDec + emitRef(consumer)
                    //else
                      portDec = portDec + emitRef(consumer) + " := " + c.name + "." + asValidName(n)
                  }
                }
              //}
            }
        }
        portDec += "\n"
        portDecs += new StringBuilder(portDec)
      }
    }
    val uncommentedPorts = portDecs.filter(!_.result.contains(";"))
    //uncommentedPorts.slice(0, uncommentedPorts.length-1).map(_.append(","))
    portDecs.map(_.insert(0, "    "))
    //if (c.clocks.length > 0 || c.resets.size > 0) res += ",\n" else res += "\n"
    //res += portDecs.map(_.result).reduceLeft(_ + "\n" + _)
    res += portDecs.map(_.result).reduceLeft(_ + _)
    //res += "\n  );\n";
    //if (c.wires.map(_._2.driveRand).reduceLeft(_ || _)) {
    //  res += if_not_synthesis
    //  for ((n, w) <- c.wires) {
    //    if (w.driveRand) {
    //      res += "    " + c.name + "." + n + " := " + emitRand(w) + ";\n"
    //    }
    //  }
    //  res += endif_not_synthesis
    //}
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
            "    " + emitTmp(node) + " := " + emitRand(node) + "\n"
          } else if (node.inputs(0) == null) {
            ChiselError.warning("UNCONNECTED WIRE " + node + " IN " + node.component)
            "    " + emitTmp(node) + " := " + emitRand(node) + "\n"
          } else {
            "    " + emitTmp(node) + " := " + emitRef(node.inputs(0)) + "\n"
          }
        }

      case x: Mux =>
        "    " + emitTmp(x) + " := " + "mux(" + emitRef(x.inputs(0)) + ", " + emitRef(x.inputs(1)) + ", " + emitRef(x.inputs(2)) + ")\n"

      case o: Op =>
        val c = o.component;
        val value = 
          if (o.op == "~") { "not(" + emitRef(node.inputs(0)) + ")" }
          else if (o.op == "##") { "cat(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "s*s") { "asUInt(mul(asSInt(" + emitRef(node.inputs(0)) + "), asSInt(" + emitRef(node.inputs(1)) + ")))" }
          else if (o.op == "s*u") { "asUInt(mul(asSInt(" + emitRef(node.inputs(0)) + "), " + emitRef(node.inputs(1)) + "))" }
          else if (o.op == "s%s") { "asUInt(mod(asSInt(" + emitRef(node.inputs(0)) + "), asSInt(" + emitRef(node.inputs(1)) + ")))" }
          else if (o.op == "s/s") { "asUInt(div(asSInt(" + emitRef(node.inputs(0)) + "), asSInt(" + emitRef(node.inputs(1)) + ")))" }
          else if (o.op == "s<") { "asUInt(lt(asSInt(" + emitRef(node.inputs(0)) + "), asSInt(" + emitRef(node.inputs(1)) + ")))" }
          else if (o.op == "s<=") {"asUInt(leq(asSInt(" + emitRef(node.inputs(0)) + "), asSInt(" + emitRef(node.inputs(1)) + ")))" }
          else if (o.op == "s>>") { "asUInt(dshr(asSInt(" + emitRef(node.inputs(0)) + "), " + emitRef(node.inputs(1)) + "))" }
          else if (o.op == "<<") { "dshl(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == ">>") { "dshr(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "+") { "addw(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "*") { "mul(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "/") { "div(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "%") { "mod(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "^") { "xor(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "-") { "subw(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "&") { "and(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "|") { "or(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "==") { "eq(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "!=" ) {"neq(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "<") { "lt(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else if (o.op == "<=" ) {"leq(" + emitRef(node.inputs(0)) + ", " + emitRef(node.inputs(1)) + ")" }
          else { throw new Exception("Unsupported operator (" + o.op + ")") }
        "    " + emitTmp(o) + " := " + value + "\n"

      case x: Extract =>
        node.inputs.tail.foreach(x.validateIndex)
        val gotWidth = node.inputs(0).needWidth()
        if (node.inputs.length < 3) {
          if(gotWidth > 1) {
            "    " + emitTmp(node) + " := bit(" + emitRef(node.inputs(0)) + "," + node.inputs(1).litOf.value + ")\n"
          } else {
            "    " + emitTmp(node) + " := " + emitRef(node.inputs(0)) + "\n"
          }
        } else {
          if(gotWidth > 1) {
            "    " + emitTmp(node) + " := bits(" + emitRef(node.inputs(0)) + ", " + node.inputs(1).litOf.value + ", " + node.inputs(2).litOf.value + ")\n"
          } else {
            "    " + emitTmp(node) + " := " + emitRef(node.inputs(0)) + "\n"
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
          val name = getMemName(m, m.n,m.needWidth(),m.ports,mask_gran)
          ChiselError.info("MEM " + name)

          val dec = "    inst " + emitRef(m) + " of " + name + "\n"
          val clk = "    " + emitRef(m) + ".CLK := " + emitRef(m.clock) + "\n"
          val portdefs = for (i <- 0 until m.ports.size)
            yield emitPortDef(m.ports(i),emitRef(m), i)

          val res = new StringBuilder()
          res.append(dec)
          res.append(clk)
          res.append(portdefs.reduceLeft((x:String, y:String) => x + y))
          res.result()
        } else {
          ""
        }
      case m: MemRead =>
        if (m.mem.isInline) {
          //"    assign " + emitTmp(node) + " = " + emitRef(m.mem) + "[" + emitRef(m.addr) + "];\n"
          ""
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
    (if (node.prune && res != "") "" else "") + res
  }

  def emitDecBase(node: Node, wire: String = "wire"): String =
    s"    ${wire} ${emitRef(node)} : UInt${emitWidth(node)}\n"

  def emitDecReg(node: Node): String = 
    s"    reg ${emitRef(node)} : UInt${emitWidth(node)},${emitRef(node.clock)},UInt<1>(0)\n"

  def emitDecAccessor(node: MemAccess): String =
    s"    infer accessor ${emitRef(node)} = ${emitRef(node.mem)}[${emitRef(node.addr)}]\n"
          //emitRef(m.mem) + "[" + emitRef(m.addr) + "] <= " + emitRef(m.data) + ";\n"

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
        ""
        //"    reg" + "[" + (gotWidth-1) + ":0] " + emitRef(node) + ";\n"

      case _: Reg =>
        emitDecReg(node)

      case _: Sprintf =>
        emitDecReg(node)

      case _: ROMRead =>
        emitDecReg(node)

      case m: Mem[_] =>
        if (m.isInline) {
          //"    reg [" + (m.needWidth()-1) + ":0] " + emitRef(m) + " [" + (m.n-1) + ":0];\n"
          "    cmem " + emitRef(m) + " : " + "UInt" + emitWidth(m) + "[" + m.n + "], " + emitRef(m.clock) + "\n"
        } else {
          ""
        }

      case x: MemAccess =>
        if (x.mem.isInline) {
          x.referenced = true
          emitDecAccessor(x)
        } else { 
          emitDecBase(node)
        }

      case _: ROMData => ""

      case _: Literal => ""

      case _ =>
        emitDecBase(node)
    }
    (if (node.prune && res != "") "//" else "") + res
  }


  // Is the specified node synthesizeable?
  // This could be expanded. For the moment, we're flagging unconnected Bits,
  // for which we generate un-synthesizable random values.
  def synthesizeable(node: Node): Boolean = {
    node match {
      case x: Bits =>
        if (x.isIo && x.dir == INPUT) {
          true
        } else if (node.inputs.length > 0 && node.inputs(0) != null) {
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
      //res.append(if_not_synthesis)
      res ++= resSimulate
      //res.append(endif_not_synthesis)
    }
    res ++= resSynthesis
    for (c <- c.children) {
      res.append(emitDef(c))
    }
    res
  }

  def emitRegs(c: Module): StringBuilder = {
    val res = new StringBuilder();
    val clkDomains = new HashMap[Clock, StringBuilder]
    for (clock <- c.clocks) {
      clkDomains += (clock -> new StringBuilder)
    }
    if (Driver.isAssert) {
      for (p <- c.asserts) {
        clkDomains(p.clock).append(emitAssert(p))
      }
    }
    for (m <- c.nodes) {
      val clkDomain = clkDomains getOrElse (m.clock, null)
      if (m.clock != null && clkDomain != null)
        clkDomain.append(emitReg(m))
    }
    for (p <- c.printfs) {
      val clkDomain = clkDomains getOrElse (p.clock, null)
      if (p.clock != null && clkDomain != null)
        clkDomain.append(emitPrintf(p))
    }
    for (clock <- c.clocks) {
      val dom = clkDomains(clock)
      if (!dom.isEmpty) {
        if (res.isEmpty)
          res.append("\n")
        //res.append("  always @(posedge " + emitRef(clock) + ") begin\n")
        res.append(dom.result())
        //res.append("  end\n")
      }
    }
    res
  }

  def emitPrintf(p: Printf): String = { ""
    //if_not_synthesis +
    //"`ifdef PRINTF_COND\n" +
    //"    if (`PRINTF_COND)\n" +
    //"`endif\n" +
    //"      if (" + emitRef(p.cond) + ")\n" +
    //"        $fwrite(32'h80000002, " + p.args.map(emitRef _).foldLeft(CString(p.format))(_ + ", " + _) + ");\n" +
    //endif_not_synthesis
  }
  def emitAssert(a: Assert): String = { ""
    //if_not_synthesis +
    //"  if(" + emitRef(a.reset) + ") " + emitRef(a) + " <= 1'b1;\n" +
    //"  if(!" + emitRef(a.cond) + " && " + emitRef(a) + " && !" + emitRef(a.reset) + ") begin\n" +
    //"    $fwrite(32'h80000002, " + CString("ASSERTION FAILED: %s\n") + ", " + CString(a.message) + ");\n" +
    //"    $finish;\n" +
    //"  end\n" +
    //endif_not_synthesis
  }

  def emitReg(node: Node): String = {
    node match {
      case reg: Reg =>
        def cond(c: Node) = "when " + emitRef(c) + " : "
        def uncond = ""
        def sep = "    "
        def assign(r: Reg, x: Node) = emitRef(r) + " := " + emitRef(x)
        def traverseMuxes(r: Reg, x: Node): List[String] = x match {
          case m: Mux => (cond(m.inputs(0)) + assign(r, m.inputs(1))) :: traverseMuxes(r, m.inputs(2))
          case _ => if (x eq r) Nil else List(uncond + assign(r, x))
        }
        if (!reg.next.isInstanceOf[Mux]) "    " + assign(reg, reg.next)
        else "    " + traverseMuxes(reg, reg.next).reduceLeft(_ + "\n    else : " + _) + "\n"

      case m: MemWrite =>
        if (m.mem.isInline) {
          "    when " + emitRef(m.cond) + " : " + emitRef(m) + " := " + emitRef(m.data) + "\n"
        } else {
          ""
        }
      case _ =>
        ""
    }
  }

  def emitDecs(c: Module): StringBuilder = {
    //c.nodes.map(emitDec(_)).addString(new StringBuilder)
    val first = new StringBuilder()
    val second = new StringBuilder()
    for ( x <- c.nodes) {
      x match {
        case m:MemAccess => second.append(emitDec(m))
        case other => first.append(emitDec(other))
      }
    }
    second.addString(first)
  }
      

  //def emitInits(c: Module): StringBuilder = {
    //val sb = new StringBuilder
    //c.nodes.map(emitInit(_)).addString(sb)

    //val res = new StringBuilder
    //if (!sb.isEmpty) {
      //res append if_not_synthesis
      //res append "  integer initvar;\n"
      //res append "  initial begin\n"
      //res append "    #0.002;\n"
      //res append sb
      //res append "  end\n"
      //res append endif_not_synthesis
    //}
    //res
  //}

  def emitModuleText(c: Module): String = {
    if (c.isInstanceOf[BlackBox])
      return ""

    val res = new StringBuilder()
    var first = true;
    var nl = "";
    if (c.clocks.length > 0) 
      res.append((c.clocks).map(x => "    input " + emitRef(x) + " : Clock\n").reduce(_ + _))
    if (c.resets.size > 0)
      res.append((c.resets.values.toList).map(x => "    input " + emitRef(x) + " : UInt<1>\n").reduce(_ + _))
    val ports = new ArrayBuffer[StringBuilder]
    val pruned = new ArrayBuffer[StringBuilder]
    for ((n, w) <- c.wires) {
      // if(first && !hasReg) {first = false; nl = "\n"} else nl = ",\n";
      w match {
        case io: Bits => {
          //val prune = if (io.prune && c != Driver.topComponent) "//" else ""
          if (io.prune) {
            pruned += new StringBuilder("    " + emitRef(io) + " := UInt(0)")
          }
          val prune = ""
          if (io.dir == INPUT) {
            ports += new StringBuilder(nl + "    " + prune + "input " +
                                       emitRef(io) + " : UInt" + emitWidth(io));
          } else if(io.dir == OUTPUT) {
            ports += new StringBuilder(nl + "    " + prune + "output " +
                                       emitRef(io) + " : UInt" + emitWidth(io));
          }
        }
      };
    }
    val uncommentedPorts = ports.filter(!_.result.contains("//"))
    //uncommentedPorts.slice(0, uncommentedPorts.length-1).map(_.append(","))
    //if (c.clocks.length > 0 || c.resets.size > 0) res.append(",\n") else res.append("\n")
    res.append(ports.map(_.result + "\n").reduceLeft(_ + _))
    res.append("\n");
    res.append(pruned.map(_.result + "\n").reduceLeftOption(_ + _).getOrElse(""));
    res.append("\n");
    res.append(";DECLARATIONS\n");
    res.append("\n");
    res.append(emitDecs(c));
    res.append("\n");
    res.append(";DEFINITIONS\n");
    res.append("\n");
    //res.append(emitInits(c));
    //res.append("\n");
    res.append(emitDefs(c));
    res.append("\n");
    res.append(";REG UPDATES\n");
    res.append("\n");
    res.append(emitRegs(c))
    res.append("\n");
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
          className + "___" + index.toString;
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
      out.append(s"  module ${top.moduleName} :\n")
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
    out.append(s"circuit ${top.moduleName} :\n")
    emitChildren(top, defs, out, depth);
  }

  override def elaborate(c: Module) {
    super.elaborate(c)
    // execute addBindings only in the Verilog Backend
    addBindings
    nameBindings
    findConsumers(c)

    val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName)
    if (!Driver.onlyRunTester) {
      val out = createOutputFile(n + ".fir")
      doCompile(c, out, 0)

      // add external memories to output file
      out.write(memDec.result())

      ChiselError.checkpoint()
      out.close()
    }

    if (!memConfs.isEmpty) {
      val out_conf = createOutputFile(n + ".conf")
      out_conf.write(getMemConfString);
      out_conf.close();
    }
    if (Driver.isGenHarness) {
      genHarness(c, n);
    }
  }

  override def compile(c: Module, flags: String) {
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

  def genHarness(c: Module, name: String) {}
}

