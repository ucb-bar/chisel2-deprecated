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

class VcdBackend(top: Module) extends Backend {
  val keywords = Set[String]()

  override def emitTmp(node: Node): String =
    emitRef(node)
  override def emitRef(node: Node): String =
    node match {
      case _: Literal =>
        node.name
      case _: Reg =>
        if (node.named) node.name else "R" + node.emitIndex
      case _ =>
        if (node.named) node.name else "T" + node.emitIndex
    }

  private def emitDefUnconditional(node: Node, index: Int) =
    "  dat_dump<" + varNameLength(index) + ">(f, " + emitRef(node) + ", 0x" + varNumber(index).toHexString + ");\n"

  private def emitDefUnconditional(node: Node, offset: Int, index: Int) =
    "  dat_dump<" + varNameLength(index) + ">(f, " + emitRef(node) + ".get(0x" + offset.toHexString +"), 0x" + varNumber(index).toHexString + ");\n"

  private def emitDef1(node: Node, index: Int, toZero: Boolean = false) = (node match {
    case _: Clock => "  if (" + emitRef(node) + ".len == "  + emitRef(node) + ".cnt)" 
    case _        => "  if (" + emitRef(node) + " != " + emitRef(node) + "__prev)" }) + 
    "  goto " + (if (toZero) "Z" else "L") + index + ";\n" + (if (toZero) "C" else "K") + index + ":"

  private def emitDef1(node: Node, offset: Int, index: Int) =
    "  if (" + emitRef(node) + ".get(0x" + offset.toHexString +") != " + emitRef(node) +
    "__prev.get(0x" + offset.toHexString + "))" + "  goto L" + index + ";\n" + "K" + index + ":"

  private def emitDef2(node: Node, index: Int, isZero: Boolean = false) =
    (if (isZero) "Z" else "L") + index + ":\n" +
    (node match { 
      case _: Clock => "  " + emitRef(node) + ".values[0] = %d;\n".format(if (isZero) 0 else 1)
      case _        => "  " + emitRef(node) + "__prev = " + emitRef(node) + ";\n" }) +
    emitDefUnconditional(node, index) +
    "  goto " + (if (isZero) "C" else "K") + index + ";\n"

  private def emitDef2(node: Node, offset: Int, index: Int) =
    "L" + index + ":\n" +
    "  " + emitRef(node) + "__prev.put(0x" + offset.toHexString + ", " +
    emitRef(node) + ".get(0x" + offset.toHexString + "));\n" +
    emitDefUnconditional(node, offset, index) +
    "  goto K" + index + ";\n"

  private def emitDefInline(node: Node, index: Int, isZero: Boolean = false) = (node match {
    case _: Clock =>
      "  if (" + emitRef(node) + ".len == " + emitRef(node) + ".cnt) {\n" +
      "    " + emitRef(node) + ".values[0] = %d;\n".format(if (isZero) 0 else 1)
    case _        =>
      "  if (" + emitRef(node) + " != " + emitRef(node) + "__prev) {\n" +
      "    " + emitRef(node) + "__prev = " + emitRef(node) + ";\n" }) +
      "  " + emitDefUnconditional(node, index) + "  }\n"
 

  private def emitDefInline(node: Node, offset: Int, index: Int) =
    "  if (" + emitRef(node) + ".get(0x" + offset.toHexString +") != " + emitRef(node) +
    "__prev.get(0x" + offset.toHexString + ")) {\n" +
    "    " + emitRef(node) + "__prev.put(0x" + offset.toHexString + ", " +
    "    " + emitRef(node) + ".get(0x" + offset.toHexString + "));\n" +
    "    " + emitDefUnconditional(node, offset, index) +
    "  }\n"

  override def emitDec(node: Node): String = node match {
    case m: Mem[_] =>
      "  mem_t<" + m.needWidth() + "," + m.n + "> " + emitRef(node) + "__prev;\n"
    case r: ROMData =>
      "  mem_t<" + r.needWidth() + "," + r.lits.size + "> " + emitRef(node) + "__prev;\n"
    case _ =>
      "  dat_t<" + node.needWidth() + "> " + emitRef(node) + "__prev;\n"
  } 

  def dumpVCDScope(c: Module, write: String => Unit): Unit = {
    write("  fputs(\"" + "$scope module " + c.name + " $end" + "\\n\", f);\n")
    if (c == topMod) {
      for ((clk, i) <- Driver.clocks.zipWithIndex) {
        write("  fputs(\"$var wire 1 " + varName(i) + " " + clk.name + " $end\\n\", f);\n")
      }
    }
    var baseIdx = Driver.clocks.size
    for ((mod, i) <- sortedMods.zipWithIndex if c == mod.component && !mod.name.isEmpty) {
      write("  fputs(\"$var wire " + mod.needWidth() + " " + varName(baseIdx + i) + " " + top.stripComponent(emitRef(mod)) + " $end\\n\", f);\n")
    }
    baseIdx += sortedMods.size
    for (mem <- sortedMems if c == mem.component && !mem.name.isEmpty) {
      for (offset <- 0 until mem.n) {
        write("  fputs(\"$var wire " + mem.needWidth() + " " + varName(baseIdx + offset) + " " +
          top.stripComponent(emitRef(mem)) + "[%d] $end\\n\", f);\n".format(offset))
      }
      baseIdx += mem.n
    }
    for (rom <- sortedROMs if c == rom.component && !rom.name.isEmpty) {
      for (offset <- 0 until rom.lits.size) {
        write("  fputs(\"$var wire " + rom.needWidth() + " " + varName(baseIdx + offset) + " " +
          top.stripComponent(emitRef(rom)) + "[%d] $end\\n\", f);\n".format(offset))
      }
      baseIdx += rom.lits.size
    }
    for (child <- c.children) dumpVCDScope(child, write)
    write("  fputs(\"$upscope $end\\n\", f);\n")
  }

  def dumpScopeForTemps(write: String => Unit): Unit = {
    write("  fputs(\"" + "$scope module _chisel_temps_ $end" + "\\n\", f);\n")
    for ((mod, i) <- sortedMods.zipWithIndex if mod.name.isEmpty) {
      write("  fputs(\"$var wire " + mod.needWidth() + " " + varName(i) + " " + top.stripComponent(emitRef(mod)) + " $end\\n\", f);\n")
    }
    var baseIdx = sortedMods.size
    for (mem <- sortedMems if mem.name.isEmpty) {
      for (offset <- 0 until mem.n) {
        write("  fputs(\"$var wire " + mem.needWidth() + " " + varName(baseIdx + offset) + " " +
          top.stripComponent(emitRef(mem)) + "[%d] $end\\n\", f);\n".format(offset))
      }
      baseIdx += mem.n
    }
    for (rom <- sortedROMs if rom.name.isEmpty) {
      for (offset <- 0 until rom.lits.size) {
        write("  fputs(\"$var wire " + rom.needWidth() + " " + varName(baseIdx + offset) + " " +
          top.stripComponent(emitRef(rom)) + "[%d] $end\\n\", f);\n".format(offset))
      }
      baseIdx += rom.lits.size
    }
    write("  fputs(\"$upscope $end\\n\", f);\n")
  }

  def dumpVCDInit(write: String => Unit): Unit = if (Driver.isVCD) {
    write("  fputs(\"$timescale %dps $end\\n\", f);\n".format(Driver.implicitClock.period.round))
    dumpVCDScope(top, write)
    if (Driver.emitTempNodes) dumpScopeForTemps(write)
    write("  fputs(\"$enddefinitions $end\\n\", f);\n")
    write("  fputs(\"$dumpvars\\n\", f);\n")
    write("  fputs(\"$end\\n\", f);\n")
    write("  fputs(\"#0\\n\", f);\n")
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDefUnconditional(clk, i))
    }
    var baseIdx = Driver.clocks.size
    for ((mod, i) <- sortedMods.zipWithIndex) {
      write(emitDefUnconditional(mod, baseIdx + i))
      val ref = emitRef(mod)
      write("  " + ref + "__prev = " + ref +";\n");
    }
    baseIdx += sortedMods.size
    for (mem <- sortedMems) {
      for (offset <- 0 until mem.n)
        write(emitDefUnconditional(mem, offset, baseIdx + offset))
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      for (offset <- 0 until rom.lits.size)
        write(emitDefUnconditional(rom, offset, baseIdx + offset))
      baseIdx += rom.lits.size
    }
  }

  def dumpModsInline(write: String => Unit) {
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDefInline(clk, i))
    }
    var baseIdx = Driver.clocks.size
    for ((mod, i) <- sortedMods.zipWithIndex)
      write(emitDefInline(mod, baseIdx + i))
    baseIdx += sortedMods.size
    for (mem <- sortedMems) {
      for (offset <- 0 until mem.n)
        write(emitDefInline(mem, offset, baseIdx + offset))
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      for (offset <- 0 until rom.lits.size)
        write(emitDefInline(rom, offset, baseIdx + offset))
      baseIdx += rom.lits.size
    }
    write("  fprintf(f, \"#%d\\n\", (t << 1) + 1);\n")
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDefInline(clk, i, true))
    }
    write("  return;\n")
  }

  def dumpModsGoTos(write: String => Unit) {
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDef1(clk, i))
    }
    var baseIdx = Driver.clocks.size
    for ((mod, i) <- sortedMods.zipWithIndex)
      write(emitDef1(mod, baseIdx + i))
    baseIdx += sortedMods.size
    for (mem <- sortedMems) {
      for (offset <- 0 until mem.n)
        write(emitDef1(mem, offset, baseIdx + offset))
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      for (offset <- 0 until rom.lits.size)
        write(emitDef1(rom, offset, baseIdx + offset))
      baseIdx += rom.lits.size
    }
    write("  fprintf(f, \"#%d\\n\", (t << 1) + 1);\n")
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDef1(clk, i, true))
    }
    write("  return;\n")
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDef2(clk, i))
    }
    baseIdx = Driver.clocks.size
    for ((mod, i) <- sortedMods.zipWithIndex)
      write(emitDef2(mod, baseIdx + i))
    baseIdx += sortedMods.size
    for (mem <- sortedMems) {
      for (offset <- 0 until mem.n)
        write(emitDef2(mem, offset, baseIdx + offset))
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      for (offset <- 0 until rom.lits.size)
        write(emitDef2(rom, offset, baseIdx + offset))
      baseIdx += rom.lits.size
    }
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDef2(clk, i, true))
    }
  }

  def dumpVCD(write: String => Unit): Unit = {
    if (Driver.isVCDinline) {
      dumpModsInline(write)
    } else {
      dumpModsGoTos(write)
    }
  }

  private lazy val sortedMods = Driver.orderedNodes filter {
    case _: Mem[_] => false case _: ROMData => false case node => node.isInVCD
  } sortWith (_.width < _.width)

  private lazy val sortedMems: Seq[Mem[_]] = Driver.orderedNodes filter {
    case m: Mem[_] => m.isInVCD case _ => false 
  } map (_.asInstanceOf[Mem[_]]) sortWith (_.widthW < _.widthW)

  private lazy val sortedROMs: Seq[ROMData] = Driver.orderedNodes filter {
    case r: ROMData => r.isInVCD case _ => false
  } map (_.asInstanceOf[ROMData]) sortWith (_.width < _.width)

  private val (lo, hi) = ('!'.toInt, '~'.toInt)
  private val range = hi - lo + 1
  private def varName(x: Int): String =
    ("\\x" + (lo + (x % range)).toHexString) + (if (x >= range) varName(x / range) else "")
  private def varNumber(x: Int): Long =
    (if (x >= range) varNumber(x / range) * 256 else 0) + (lo + (x % range))
  private def varNameLength(x: Int): Int =
    1 + (if (x >= range) varNameLength(x / range) else 0)
}
