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
import Node._
import Reg._
import Literal._
import ChiselError._
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

class VcdBackend(top: Module) extends Backend {
  val keywords = new HashSet[String]()

  override def emitTmp(node: Node): String =
    emitRef(node)

  private def emitDefUnconditional(node: Node, index: Int) =
    "  dat_dump<" + varNameLength(index) + ">(f, " + emitRef(node) + ", 0x" + varNumber(index).toHexString + ");\n"

  private def emitDef1(node: Node, index: Int) =
    "  if (" + emitRef(node) + " != " + emitRef(node) + "__prev)\n" +
    "    goto L" + index + ";\n" +
    "K" + index + ":\n"

  private def emitDef2(node: Node, index: Int) =
    "L" + index + ":\n" +
    "  " + emitRef(node) + "__prev = " + emitRef(node) + ";\n" +
    emitDefUnconditional(node, index) +
    "  goto K" + index + ";\n"

  override def emitDec(node: Node): String =
    if (Driver.isVCD && node.isInVCD) "  dat_t<" + node.width + "> " + emitRef(node) + "__prev" + ";\n" else ""

  def dumpVCDScope(c: Module, write: String => Unit): Unit = {
    write("  fputs(\"" + "$scope module " + c.name + " $end" + "\\n\", f);\n")
    for (i <- 0 until sortedMods.length) {
      val mod = sortedMods(i)
      if (mod.component == c && !mod.name.isEmpty)
        write("  fputs(\"$var wire " + mod.width + " " + varName(i) + " " + top.stripComponent(emitRef(mod)) + " $end\\n\", f);\n")
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
        write("  fputs(\"$var wire " + mod.width + " " + varName(i) + " " + top.stripComponent(emitRef(mod)) + " $end\\n\", f);\n")
    }
    write("  fputs(\"$upscope $end\\n\", f);\n")
  }

  def dumpVCDInit(write: String => Unit): Unit = {
    write("void " + top.name + "_t::dump_init(FILE *f) {\n")
    if (Driver.isVCD) {
      write("  fputs(\"$timescale 1ps $end\\n\", f);\n")
      dumpVCDScope(top, write)
      if (Module.emitTempNodes)
        dumpScopeForTemps(write)
      write("  fputs(\"$enddefinitions $end\\n\", f);\n")
      write("  fputs(\"$dumpvars\\n\", f);\n")
      write("  fputs(\"$end\\n\", f);\n")
      write("  fputs(\"#0\\n\", f);\n")
      for (i <- 0 until sortedMods.length)
        write(emitDefUnconditional(sortedMods(i), i))
    }
    write("}\n")
  }

  def dumpVCD(write: String => Unit): Unit = {
    write("void " + top.name + "_t::dump(FILE *f, int t) {\n")
    if (Driver.isVCD) {
      write("  if (t == 0) return dump_init(f);\n")
      write("  fprintf(f, \"#%d\\n\", t);\n")
      for (i <- 0 until sortedMods.length)
        write(emitDef1(sortedMods(i), i))
      write("  return;\n")
      for (i <- 0 until sortedMods.length)
        write(emitDef2(sortedMods(i), i))
    }
    write("}\n")
  }

  private val sortedMods = top.omods.filter(_.isInVCD).sortWith(_.width < _.width)

  private val (lo, hi) = ('!'.toInt, '~'.toInt)
  private val range = hi - lo + 1
  private def varName(x: Int): String =
    ("\\x" + (lo + (x % range)).toHexString) + (if (x >= range) varName(x / range) else "")
  private def varNumber(x: Int): Long =
    (if (x >= range) varNumber(x / range) * 256 else 0) + (lo + (x % range))
  private def varNameLength(x: Int): Int =
    1 + (if (x >= range) varNameLength(x / range) else 0)
}
