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
import Reg._
import Literal._
import ChiselError._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

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

  private def emitDef1(node: Node, index: Int) =
    "  if (" + emitRef(node) + " != " + emitRef(node) + "__prev)\n" +
    "    goto L" + index + ";\n" +
    "K" + index + ":\n"

  private def emitDef1(node: Node, offset: Int, index: Int) =
    "  if (" + emitRef(node) + ".get(0x" + offset.toHexString +") != " + emitRef(node) +
    "__prev.get(0x" + offset.toHexString + "))\n" +
    "    goto L" + index + ";\n" +
    "K" + index + ":\n"

  private def emitDef2(node: Node, index: Int) =
    "L" + index + ":\n" +
    "  " + emitRef(node) + "__prev = " + emitRef(node) + ";\n" +
    emitDefUnconditional(node, index) +
    "  goto K" + index + ";\n"

  private def emitDef2(node: Node, offset: Int, index: Int) =
    "L" + index + ":\n" +
    "  " + emitRef(node) + "__prev.get(0x" + offset.toHexString + ") = " +
    emitRef(node) + ".get(0x" + offset.toHexString + ");\n" +
    emitDefUnconditional(node, offset, index) +
    "  goto K" + index + ";\n"

  private def emitDefInline(node: Node, index: Int) =
    "  if (" + emitRef(node) + " != " + emitRef(node) + "__prev) {\n" +
    "    " + emitRef(node) + "__prev = " + emitRef(node) + ";\n" +
    "    " + emitDefUnconditional(node, index) +
    "  }\n"

  private def emitDefInline(node: Node, offset: Int, index: Int) =
    "  if (" + emitRef(node) + ".get(0x" + offset.toHexString +") != " + emitRef(node) +
    "__prev.get(0x" + offset.toHexString + ")) {\n" +
    "    " + emitRef(node) + "__prev.get(0x" + offset.toHexString + ") = " +
    "    " + emitRef(node) + ".get(0x" + offset.toHexString + ");\n" +
    "    " + emitDefUnconditional(node, offset, index) +
    "  }\n"

  override def emitDec(node: Node): String =
    if (Driver.isVCD && node.isInVCD) {
      node match {
        case m: Mem[_] =>
          "  mem_t<" + m.needWidth() + "," + m.n + "> " + emitRef(node) + "__prev" + ";\n"
        case r: ROMData =>
          "  mem_t<" + r.needWidth() + "," + r.lits.size + "> " + emitRef(node) + "__prev" + ";\n"
        case _ =>
          "  dat_t<" + node.needWidth() + "> " + emitRef(node) + "__prev" + ";\n"
      }
    }
    else ""

  def dumpVCDScope(c: Module, write: String => Unit): Unit = {
    write("  fputs(\"" + "$scope module " + c.name + " $end" + "\\n\", f);\n")
    write("  fputs(\"$var wire 1 " + clockName(Driver.implicitClock) + " " + emitRef(Driver.implicitClock) + " $end\\n\", f);\n") 
    for (clk <- c.clocks) {
      if (clk != Driver.implicitClock) {
        write("  fputs(\"$var wire 1 " + clockName(clk) + " " + emitRef(clk) + " $end\\n\", f);\n") 
      }
    }
    for (i <- 0 until sortedMods.length) {
      val mod = sortedMods(i)
      if (mod.component == c && !mod.name.isEmpty)
        write("  fputs(\"$var wire " + mod.needWidth() + " " + varName(i) + " " + top.stripComponent(emitRef(mod)) + " $end\\n\", f);\n")
    }
    var baseIdx = sortedMods.length
    for (mem <- sortedMems) {
      if (mem.component == c && !mem.name.isEmpty) {
        for (offset <- 0 until mem.n) {
          write("  fputs(\"$var wire " + mem.needWidth() + " " + varName(baseIdx + offset) + " " +
            top.stripComponent(emitRef(mem)) + "[%d] $end\\n\", f);\n".format(offset))
        }
      }
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      if (rom.component == c && !rom.name.isEmpty) {
        for (offset <- 0 until rom.lits.size) {
          write("  fputs(\"$var wire " + rom.needWidth() + " " + varName(baseIdx + offset) + " " +
            top.stripComponent(emitRef(rom)) + "[%d] $end\\n\", f);\n".format(offset))
        }
      }
      baseIdx += rom.lits.size
    }
    for (child <- c.children) {
      dumpVCDScope(child, write)
    }
    write("  fputs(\"$upscope $end\\n\", f);\n")
  }

  def dumpScopeForTemps(write: String => Unit): Unit = {
    write("  fputs(\"" + "$scope module _chisel_temps_ $end" + "\\n\", f);\n")
    for (i <- 0 until sortedMods.length) {
      val mod = sortedMods(i)
      if (mod.name.isEmpty)
        write("  fputs(\"$var wire " + mod.needWidth() + " " + varName(i) + " " + top.stripComponent(emitRef(mod)) + " $end\\n\", f);\n")
    }
    var baseIdx = sortedMods.length
    for (mem <- sortedMems) {
      if (mem.name.isEmpty) {
        for (offset <- 0 until mem.n) {
          write("  fputs(\"$var wire " + mem.needWidth() + " " + varName(baseIdx + offset) + " " +
            top.stripComponent(emitRef(mem)) + "[%d] $end\\n\", f);\n".format(offset))
        }
      }
      baseIdx += mem.n
    }
    for (rom <- sortedROMs) {
      if (rom.name.isEmpty) {
        for (offset <- 0 until rom.lits.size) {
          write("  fputs(\"$var wire " + rom.needWidth() + " " + varName(baseIdx + offset) + " " +
            top.stripComponent(emitRef(rom)) + "[%d] $end\\n\", f);\n".format(offset))
        }
      }
      baseIdx += rom.lits.size
    }
    write("  fputs(\"$upscope $end\\n\", f);\n")
  }

  def dumpVCDInit(write: String => Unit): Unit = {
    if (Driver.isVCD) {
      write("  fputs(\"$timescale " + Driver.implicitClock.period + " $end\\n\", f);\n")
      dumpVCDScope(top, write)
      if (Driver.emitTempNodes)
        dumpScopeForTemps(write)
      write("  fputs(\"$enddefinitions $end\\n\", f);\n")
      write("  fputs(\"$dumpvars\\n\", f);\n")
      write("  fputs(\"$end\\n\", f);\n")
      write("  fputs(\"#0\\n\", f);\n")
      for (clock <- Driver.clocks) {
        write("  fputs(\"0" + clockName(clock) + "\\n\", f);\n")
      }
      for (i <- 0 until sortedMods.length) {
        write(emitDefUnconditional(sortedMods(i), i))
        val ref = emitRef(sortedMods(i))
        write("  " + ref + "__prev = " + ref +";\n");
      }
      var baseIdx = sortedMods.length
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
  }

  def dumpModsInline(write: String => Unit) {
    for (i <- 0 until sortedMods.length)
      write(emitDefInline(sortedMods(i), i))
    var baseIdx = sortedMods.length
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
    write("  return;\n")
  }

  def dumpModsGoTos(write: String => Unit) {
    for (i <- 0 until sortedMods.length)
      write(emitDef1(sortedMods(i), i))
    var baseIdx = sortedMods.length
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
    write("  return;\n")
    for (i <- 0 until sortedMods.length)
      write(emitDef2(sortedMods(i), i))
    baseIdx = sortedMods.length
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
  }

  def dumpVCD(write: String => Unit): Unit = {
    if (Driver.isVCDinline) {
      dumpModsInline(write)
    } else {
      dumpModsGoTos(write)
    }
  }

  private val sortedMods = (Driver.orderedNodes foldLeft Array[Node]()){
    case (array: Array[Node], mem: Mem[_]) => array
    case (array: Array[Node], rom: ROMData) => array
    case (array: Array[Node], node: Node) => if (node.isInVCD) array ++ Array(node) else array
  } sortWith (_.width < _.width)

  private val sortedMems = (Driver.orderedNodes foldLeft Array[Mem[_]]()){
    case (array: Array[Mem[_]], mem: Mem[_]) => if (mem.isInVCD) array ++ Array(mem) else array
    case (array: Array[Mem[_]], node: Node) => array
  } sortWith (_.widthW < _.widthW)

  private val sortedROMs = (Driver.orderedNodes foldLeft Array[ROMData]()){
    case (array: Array[ROMData], rom: ROMData) => if (rom.isInVCD) array ++ Array(rom) else array
    case (array: Array[ROMData], node: Node) => array
  } sortWith (_.width < _.width)

  private val (lo, hi) = ('!'.toInt, '~'.toInt)
  private val range = hi - lo + 1
  def clockName(clock : Clock) : String = {
      ("\\x" + (lo + (clock.id % range)).toHexString) + (if (clock.id >= range) varName(clock.id / range) else "")
  }
  private def varName(x: Int): String = {
    val index = x + Driver.clocks.length	// Reserve identifiers for clocks
    ("\\x" + (lo + (index % range)).toHexString) + (if (index >= range) varName(index / range) else "")
  }
  private def varNumber(x: Int): Long = {
    val index = x + Driver.clocks.length	
    (if (index >= range) varNumber(index / range) * 256 else 0) + (lo + (index % range))
  }
  private def varNameLength(x: Int): Int = {
    val index = x + Driver.clocks.length
    1 + (if (index >= range) varNameLength(index / range) else 0)
  }
}
