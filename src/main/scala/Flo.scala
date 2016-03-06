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

class FloBackend extends Backend {
  // TODO: SHOULD BE IN ENV VAR
  val floDir = java.lang.System.getenv("DREAMER") + "/emulator/"
  override val needsLowering = Set("PriEnc", "OHToUInt")
  var isRnd = false

  object DreamerConfiguration {
    var numRows = 1
    var numCols = 1
  }

  override def emitDec(node: Node): String =
    emitRef(node) + " = "

  override def emitTmp(node: Node): String =
    emitRef(node)

  override def emitRef(node: Node): String = { emitRef(node, node) }

  def emitRef(node: Node, cnode: Node): String = {
    // if (node.litOpt == None) {
      node match {
        case x: Literal =>
          "" + x.value + "'" + cnode.needWidth()

        case x: Binding =>
          emitRef(x.inputs(0))

        case x: Bits =>
          if (!node.isInObject && node.inputs.length == 1) emitRef(node.inputs(0), cnode) else super.emitRef(node)

        case _ =>
          super.emitRef(node)
      }
    // } else {
    //   "" + node.litValue()
    // }
  }

  def emitRef(node: Node, sum_to: Node, other: Node): String = {
    // if (node.litOpt == None) {
      node match {
        case x: Literal =>
          "" + x.value + "'" + (sum_to.needWidth() - other.needWidth())

        case x: Binding =>
          emitRef(x.inputs(0))

        case x: Bits =>
          if (!node.isInObject && node.inputs.length == 1) emitRef(node.inputs(0)) else super.emitRef(node)

        case _ =>
          super.emitRef(node)
      }
    // } else {
    //   "" + node.litValue()
    // }
  }

  def emit(node: Node): String = {
    node match {
      case x: Mux =>
        emitDec(x) + "mux" + " " + emitRef(x.inputs(0)) + " " + emitRef(x.inputs(1), x) + " " + emitRef(x.inputs(2), x) + "\n"

      case o: Op =>
        emitDec(o) +
        (if (o.inputs.length == 1) {
          val gotWidth = node.inputs(0).needWidth()
          o.op match {
            case "~" => "not'" + gotWidth + " " + emitRef(node.inputs(0))
            case "^" => "xorr'" + gotWidth + " " + emitRef(node.inputs(0))
            case "Log2" => "log2'" + node.needWidth() + " " + emitRef(node.inputs(0))
          }
         } else {
           val gotWidth = node.inputs(0).needWidth()
           o.op match {
             case "<"  => "lt'"   + gotWidth + " " + emitRef(node.inputs(0), node.inputs(1)) + " " + emitRef(node.inputs(1), node.inputs(0))
             case "s<" => "rsh'1 " + emitRef(node.inputs(0)) + " " + (gotWidth-1)
             case ">=" => "gte'"  + gotWidth + " " + emitRef(node.inputs(0), node.inputs(1)) + " " + emitRef(node.inputs(1), node.inputs(0))
             case "<=" => "gte'"  + gotWidth + " " + emitRef(node.inputs(1), node.inputs(0)) + " " + emitRef(node.inputs(0), node.inputs(1))
             case ">"  => "lt'"   + gotWidth + " " + emitRef(node.inputs(1), node.inputs(0)) + " " + emitRef(node.inputs(0), node.inputs(1))
             case "+"  => "add'" + node.needWidth() + " " + emitRef(node.inputs(0), node) + " " + emitRef(node.inputs(1), node)
             case "-"  => "sub'" + node.needWidth() + " " + emitRef(node.inputs(0), node) + " " + emitRef(node.inputs(1), node)
             case "*"  => "mul'" + node.needWidth() + " " + emitRef(node.inputs(0), node, node.inputs(1)) + " " + emitRef(node.inputs(1), node, node.inputs(0))
             case "/"  => "div'" + node.needWidth() + " " + emitRef(node.inputs(0), node) + " " + emitRef(node.inputs(1), node)
             case "<<" => "lsh'" + node.needWidth() + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case ">>" => "rsh'" + node.needWidth() + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "s>>" => "arsh'" + node.needWidth() + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "##" => "cat'" + node.inputs(1).needWidth() + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1))
             case "|"  => "or" + " " + emitRef(node.inputs(0), node) + " " + emitRef(node.inputs(1), node)
             case "&"  => "and" + " " + emitRef(node.inputs(0), node) + " " + emitRef(node.inputs(1), node)
             case "^"  => "xor" + " " + emitRef(node.inputs(0), node) + " " + emitRef(node.inputs(1), node)
             case "==" => "eq" + " " + emitRef(node.inputs(0), node.inputs(1)) + " " + emitRef(node.inputs(1), node.inputs(0))
             case "!=" => "neq" + " " + emitRef(node.inputs(0), node.inputs(1)) + " " + emitRef(node.inputs(1), node.inputs(0))
           }
         }) + "\n"

      case x: Extract =>
        if (node.inputs.length == 2)
          emitDec(node) + "rsh'" + node.needWidth() + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(1)) + "\n"
        else
          emitDec(node) + "rsh'" + node.needWidth() + " " + emitRef(node.inputs(0)) + " " + emitRef(node.inputs(2)) + "\n"

      case x: Bits =>
        if (x.inputs.length == 1) {
          // println("NAME " + x.name + " DIR " + x.dir + " COMP " + x.componentOf + " TOP-COMP " + topMod)
          if (node.isInObject && x.inputs.length == 1) {
            // ((x.consumers.length > 1 && x.consumers.forall(x => x.componentOf == topMod)) ||
            // TODO: SHOULD HANDLE TOP OUTPUTS THAT ARE ALSO FANNED OUT -- NEED EXTRA NODE
            if (x.dir == OUTPUT && x.component == topMod)
              emitDec(x) + (if (isRnd) "eat" else ("out'" + x.needWidth()))  + " " + emitRef(x.inputs(0)) + "\n"
            else
              emitDec(x) + "mov" + " " + emitRef(x.inputs(0)) + "\n"
          } else if (!node.isInObject && x.inputs.length == 0) {
            emitDec(x) + "rnd'" + x.needWidth() + "\n"
          } else {
            ""
          }
          // println("--> NO CONSUMERS " + x + " = " + x.consumers.length);
          // ""
        } else
          emitDec(x) + (if (x.name == "reset") "rst" else ((if (isRnd) "rnd'" else "in'")) + x.needWidth()) + "\n"

      case m: Mem[_] =>
        emitDec(m) + "mem'" + m.needWidth() + " " + m.n + "\n"
        // emitDec(m) + "mem " + m.n + "\n" + trueAll(emitRef(m) + "__is_all_read", m.reads)

      case m: ROMData =>
        val res = new StringBuilder
        res append emitDec(m) + "mem'" + m.needWidth() + " " + m.n + "\n"
        // emitDec(m) + "mem " + m.n + "\n" + trueAll(emitRef(m) + "__is_all_read", m.reads)
        for ((i, v) <- m.sparseLits)
          res append "init " + emitRef(m) + " " + i + " " + emitRef(v) + "\n"
        res.toString

      case m: MemRead =>
        // emitDec(m) + "rd'" + node.needWidth() + " " + emitRef(m.cond) + " " + emitRef(m.mem) + " " + emitRef(m.addr) + "\n"
        emitDec(m) + "rd'" + node.needWidth() + " 1 " + emitRef(m.mem) + " " + emitRef(m.addr) + "\n"

      case m: ROMRead =>
        emitDec(m) + "rd'" + node.needWidth() + " 1 " + emitRef(m.rom) + " " + emitRef(m.addr) + "\n"

      case m: MemWrite =>
        if (m.inputs.length == 2)
          return ""
        emitDec(m) + "wr'" + m.data.needWidth() + " " + emitRef(m.cond) + " " + emitRef(m.mem) + " " + emitRef(m.addr) + " " + emitRef(m.data) + "\n"
      case x: Reg => // TODO: need resetData treatment
        emitDec(x) + "reg'" + x.needWidth() + " 1 " + emitRef(x.next) + "\n"

      case l: Literal =>
        ""
      case _ =>
        println("NO EMITTER FOR " + node)
        ""
    }
  }

  override def elaborate(c: Module): Unit = {
    super.elaborate(c)

    flattenAll
    ChiselError.checkpoint()

    renameNodes(Driver.orderedNodes, ":")
    if (Driver.isReportDims) {
      val (numNodes, maxWidth, maxDepth) = findGraphDims
      // ChiselError.info("NUM " + numNodes + " MAX-WIDTH " + maxWidth + " MAX-DEPTH " + maxDepth);
    }

    // Write the generated code to the output file
    val out = createOutputFile(c.name + ".flo");
    for (m <- Driver.orderedNodes)
      out.write(emit(m));
    out.close();
  }

  override def compile(c: Module, flagsIn: Option[String]) {
    val flags = flagsIn getOrElse "-O2"
    val allFlags = flags + " -I../ -I" + chiselENV + "/csrc/"
    val dir = Driver.targetDir + "/"
    def build(name: String) {
      val mweCmd = List("flo-mwe", "--width", "32", "--depth", "64", "--input", dir + name + ".flo", "--output", dir + name + ".mwe.flo")
      println("EXPANDING WITH " + mweCmd)
      run(mweCmd.mkString(" "))
      val cmd = List(floDir + "lay", "-is-console") ++
        List(":dims", DreamerConfiguration.numCols.toString() + "," + DreamerConfiguration.numRows.toString()) ++
        List("<", dir + name + ".mwe.flo", "|") ++
        List(floDir + "fix-sched", ">", dir + name + ".hex")
      val cmdString = cmd.mkString(" ")
      println("BUILDING " + cmdString)
      if (!run(cmdString)) throwException("failed to build flo")
    }
    build(c.name)
  }


}
