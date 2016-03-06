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

class VcdBackend(top: Module) extends Backend {
  override def emitTmp(node: Node): String =
    emitRef(node)
  override def emitRef(node: Node): String =
    node match {
      case _: Literal =>
        node.name
      case _: Reg =>
        if (node.named) node.name else s"R${node.emitIndex}"
      case _ =>
        if (node.named) node.name else s"T${node.emitIndex}"
    }

  private def emitDefUnconditional(index: Int) =
    s"dat_dump<1>(f, reset, 0x%x);".format(varNumber(index))

  private def emitDefUnconditional(node: Node, index: Int) =
    s"dat_dump<${varNameLength(index)}>(f, ${emitRef(node)}, 0x%x);".format(varNumber(index))

  private def emitDefUnconditional(node: Node, offset: Int, index: Int) =
    s"dat_dump<${varNameLength(index)}>(f, ${emitRef(node)}.get(0x%x), 0x%x);".format(offset, varNumber(index))

  private def emitDef1(clk: Clock, index: Int, toZero: Boolean) = List(
   s"  if (${emitRef(clk)}.cnt == 0)  goto ${if (toZero) "Z" else "L"}${index};",
   s"${if (toZero) "C" else "K"}${index}:") mkString "\n"

  private def emitDef1(index: Int) = List(
   s"  if (reset != reset__prev)  goto L${index};",
   s"K${index}:") mkString "\n"

  private def emitDef1(node: Node, index: Int) = List(
   s"  if (${emitRef(node)} != ${emitRef(node)}__prev)  goto L${index};",
   s"K${index}:") mkString "\n"

  private def emitDef1(node: Node, offset: Int, index: Int) = List(
   s"  if (${emitRef(node)}.get(0x%x) != ${emitRef(node)}__prev.get(0x%x))  goto L${index};".format(offset, offset),
   s"K${index}:") mkString "\n"

  private def emitDef2(clk: Clock, index: Int, isZero: Boolean) = List(
   s"${if (isZero) "Z" else "L"}${index}:",
   s"  ${emitRef(clk)}.values[0] = ${if (isZero) 0 else 1};",
   s"  ${emitDefUnconditional(clk, index)}",
   s"  goto ${if (isZero) "C" else "K"}${index};\n") mkString "\n"

  private def emitDef2(index: Int) = List(
   s"L${index}:",
    "  reset__prev = reset;",
   s"  ${emitDefUnconditional(index)}",
   s"  goto K${index};\n") mkString "\n"

  private def emitDef2(node: Node, index: Int) = List(
   s"L${index}:",
   s"  ${emitRef(node)}__prev = ${emitRef(node)};",
   s"  ${emitDefUnconditional(node, index)}",
   s"  goto K${index};\n") mkString "\n"

  private def emitDef2(node: Node, offset: Int, index: Int) = List(
   s"L${index}:",
   s"  ${emitRef(node)}__prev.put(0x%x, ${emitRef(node)}.get(0x%x));".format(offset, offset),
   s"  ${emitDefUnconditional(node, offset, index)}",
   s"  goto K${index};\n") mkString "\n"

  private def emitDefInline(clk: Clock, index: Int, isZero: Boolean) = List(
   s"  if (${emitRef(clk)}.cnt == 0) {",
   s"    ${emitRef(clk)}.values[0] = ${if (isZero) 0 else 1};",
   s"    ${emitDefUnconditional(clk, index)}", "  }\n") mkString "\n"

  private def emitDefInline(index: Int) = List(
    "  if (reset != reset__prev) {",
    "    reset__prev = reset;",
   s"    ${emitDefUnconditional(index)}", "  }\n") mkString "\n"

  private def emitDefInline(node: Node, index: Int) = List(
   s"  if (${emitRef(node)} != ${emitRef(node)}__prev) {",
   s"    ${emitRef(node)}__prev = ${emitRef(node)};",
   s"    ${emitDefUnconditional(node, index)}", "  }\n") mkString "\n"

  private def emitDefInline(node: Node, offset: Int, index: Int) = List(
   s"  if (${emitRef(node)}.get(0x${offset.toHexString}) != ${emitRef(node)}__prev.get(0x${offset.toHexString})) {",
   s"    ${emitRef(node)}__prev.put(0x${offset.toHexString}, ${emitRef(node)}.get(0x${offset.toHexString}));",
   s"    ${emitDefUnconditional(node, offset, index)}", "  }\n") mkString "\n"

  override def emitDec(node: Node): String = node match {
    case m: Mem[_] =>
      s"  mem_t<${m.needWidth},${m.n}> ${emitRef(node)}__prev;\n"
    case r: ROMData =>
      s"  mem_t<${r.needWidth},${r.n}> ${emitRef(node)}__prev;\n"
    case _ =>
      s"  dat_t<${node.needWidth}> ${emitRef(node)}__prev;\n"
  }

  def dumpVCDScope(c: Module, write: String => Unit): Unit = {
    write("  fputs(\"$scope module %s $end\\n\", f);\n".format(c.name))
    if (c == topMod) {
      for ((clk, i) <- Driver.clocks.zipWithIndex) {
        write("  fputs(\"$var wire 1 %s %s $end\\n\", f);\n".format(varName(i), clk.name))
      }
    }
    var baseIdx = Driver.clocks.size
    if (c == topMod) write("  fputs(\"$var wire 1 %s reset $end\\n\", f);\n".format(varName(baseIdx)))
    baseIdx += 1
    for ((node, i) <- sortedNodes.zipWithIndex if c == node.component && !node.name.isEmpty) {
      write("  fputs(\"$var wire %d %s %s $end\\n\", f);\n".format(
        node.needWidth, varName(baseIdx + i), top.stripComponent(emitRef(node))))
    }
    baseIdx += sortedNodes.size
    for (mem <- sortedMems) {
      if (mem.component == c && !mem.name.isEmpty) {
        for (offset <- 0 until mem.n) {
          write("  fputs(\"$var wire %d %s %s[%d] $end\\n\", f);\n".format(
            mem.needWidth, varName(baseIdx + offset), top.stripComponent(emitRef(mem)), offset))
        }
      }
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      if (rom.component == c && !rom.name.isEmpty) {
        for (offset <- 0 until rom.n) {
          write("  fputs(\"$var wire %d %s %s[%d] $end\\n\", f);\n".format(
            rom.needWidth, varName(baseIdx + offset), top.stripComponent(emitRef(rom)), offset))
        }
      }
      baseIdx += rom.n
    }
    for (child <- c.children) dumpVCDScope(child, write)
    write("  fputs(\"$upscope $end\\n\", f);\n")
  }

  def dumpScopeForTemps(write: String => Unit): Unit = {
    write("  fputs(\"$scope module _chisel_temps_ $end\\n\", f);\n")
    var baseIdx = Driver.clocks.size + 1
    for ((node, i) <- sortedNodes.zipWithIndex if node.name.isEmpty) {
      write("  fputs(\"$var wire %d %s %s $end\\n\", f);\n".format(
        node.needWidth, varName(baseIdx + i), top.stripComponent(emitRef(node))))
    }
    baseIdx += sortedNodes.size
    for (mem <- sortedMems) {
      if (mem.name.isEmpty) {
        for (offset <- 0 until mem.n) {
          write("  fputs(\"$var wire %d %s %s[%d] $end\\n\", f);\n".format(
            mem.needWidth, varName(baseIdx + offset), top.stripComponent(emitRef(mem)), offset))
        }
      }
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      if (rom.name.isEmpty) {
        for (offset <- 0 until rom.n) {
          write("  fputs(\"$var wire %d %s %s[%d] $end\\n\", f);\n".format(
            rom.needWidth, varName(baseIdx + offset), top.stripComponent(emitRef(rom)), offset))
        }
      }
      baseIdx += rom.n
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
      write(emitDefInline(clk, i, false))
    }
    var baseIdx = Driver.clocks.size
    write( "  dat_t<1> reset = LIT<1>(1);\n")
    write(s"  ${emitDefUnconditional(baseIdx)}\n")
    baseIdx += 1
    for ((node, i) <- sortedNodes.zipWithIndex) {
      write(s"  ${emitDefUnconditional(node, baseIdx + i)}\n")
      write(s"  ${emitRef(node)}__prev = ${emitRef(node)};\n")
    }
    baseIdx += sortedNodes.size
    for (mem <- sortedMems) {
      for (offset <- 0 until mem.n)
        write(s"  ${emitDefUnconditional(mem, offset, baseIdx + offset)}\n")
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      for (offset <- 0 until rom.n)
        write(s"  ${emitDefUnconditional(rom, offset, baseIdx + offset)}\n")
      baseIdx += rom.n
    }
    write("  fputs(\"#1\\n\", f);\n")
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDefInline(clk, i, true))
    }
  }

  def dumpNodesInline(write: String => Unit) {
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDefInline(clk, i, false))
    }
    // reset
    var baseIdx = Driver.clocks.size
    write(emitDefInline(baseIdx))
    baseIdx += 1
    for ((node, i) <- sortedNodes.zipWithIndex)
      write(emitDefInline(node, baseIdx + i))
    baseIdx += sortedNodes.size
    for (mem <- sortedMems) {
      for (offset <- 0 until mem.n)
        write(emitDefInline(mem, offset, baseIdx + offset))
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      for (offset <- 0 until rom.n)
        write(emitDefInline(rom, offset, baseIdx + offset))
      baseIdx += rom.n
    }
    write("  fprintf(f, \"#%d\\n\", (t << 1) + 1);\n")
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDefInline(clk, i, true))
    }
    write("  return;\n")
  }

  def dumpNodesGoTos(write: String => Unit) {
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDef1(clk, i, false))
    }
    var baseIdx = Driver.clocks.size
    write(emitDef1(baseIdx))
    baseIdx += 1
    for ((node, i) <- sortedNodes.zipWithIndex)
      write(emitDef1(node, baseIdx + i))
    baseIdx += sortedNodes.size
    for (mem <- sortedMems) {
      for (offset <- 0 until mem.n)
        write(emitDef1(mem, offset, baseIdx + offset))
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      for (offset <- 0 until rom.n)
        write(emitDef1(rom, offset, baseIdx + offset))
      baseIdx += rom.n
    }
    write("  fprintf(f, \"#%d\\n\", (t << 1) + 1);\n")
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDef1(clk, i, true))
    }
    write("  return;\n")
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDef2(clk, i, false))
    }
    baseIdx = Driver.clocks.size
    write(emitDef2(baseIdx))
    baseIdx += 1
    for ((node, i) <- sortedNodes.zipWithIndex)
      write(emitDef2(node, baseIdx + i))
    baseIdx += sortedNodes.size
    for (mem <- sortedMems) {
      for (offset <- 0 until mem.n)
        write(emitDef2(mem, offset, baseIdx + offset))
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      for (offset <- 0 until rom.n)
        write(emitDef2(rom, offset, baseIdx + offset))
      baseIdx += rom.n
    }
    for ((clk, i) <- Driver.clocks.zipWithIndex) {
      write(emitDef2(clk, i, true))
    }
  }

  def dumpVCD(write: String => Unit): Unit = {
    if (Driver.isVCDinline) {
      dumpNodesInline(write)
    } else {
      dumpNodesGoTos(write)
    }
  }

  private lazy val sortedNodes = Driver.orderedNodes filter {
    case _: Mem[_] => false case _: ROMData => false case node => node.isInVCD
  } sortWith (_.widthW < _.widthW)

  private lazy val sortedMems: Seq[Mem[_]] = Driver.orderedNodes filter {
    case m: Mem[_] => m.isInVCD case _ => false
  } map (_.asInstanceOf[Mem[_]]) sortWith (_.widthW < _.widthW)

  private lazy val sortedROMs: Seq[ROMData] = Driver.orderedNodes filter {
    case r: ROMData => r.isInVCD case _ => false
  } map (_.asInstanceOf[ROMData]) sortWith (_.widthW < _.widthW)

  private val (lo, hi) = ('!'.toInt, '~'.toInt)
  private val range = hi - lo + 1
  private def varName(x: Int): String =
    ("\\x" + (lo + (x % range)).toHexString) + (if (x >= range) varName(x / range) else "")
  private def varNumber(x: Int): Long =
    (if (x >= range) varNumber(x / range) * 256 else 0) + (lo + (x % range))
  private def varNameLength(x: Int): Int =
    1 + (if (x >= range) varNameLength(x / range) else 0)
}
