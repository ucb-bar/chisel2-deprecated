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

class VcdBackend extends Backend {
  val keywords = new HashSet[String]();

  override def emitTmp(node: Node): String =
    emitRef(node)

  def emitDef(node: Node, vcdname: String): String = { // vcdname: String
    ("  if (t == 0 || ("
      + emitRef(node) + " != " + emitRef(node) + "__prev).to_bool())\n" +
    "    dat_dump(f, " + emitRef(node) + ", \"" + vcdname + "\");\n" +
    "  " + emitRef(node) + "__prev = " + emitRef(node) + ";\n")
  }

  override def emitDec(node: Node): String =
    if (Module.isVCD && node.isInVCD) "  dat_t<" + node.width + "> " + emitRef(node) + "__prev" + ";\n" else ""

  def dumpVCDScope(c: Module, write: String => Unit, top: Module, names: HashMap[Node, String]): Unit = {
    write("    fprintf(f, \"" + "$scope module " + c.name + " $end" + "\\n\");\n");
    for (mod <- top.omods) {
      if (mod.component == c && mod.isInVCD && !mod.name.isEmpty) {
        write("    fprintf(f, \"$var wire " + mod.width + " " + names(mod) + " " + top.stripComponent(emitRef(mod)) + " $end\\n\");\n");
      }
    }
    for (child <- c.children) {
      dumpVCDScope(child, write, top, names);
    }
    write("    fprintf(f, \"$upscope $end\\n\");\n");
  }

  def dumpScopeForTemps(c: Module, write: String => Unit, names: HashMap[Node, String]): Unit = {
    write("    fprintf(f, \"$scope module _chisel_temps_ $end\\n\");\n");
    for ((mod, name) <- names) {
      if (mod.name.isEmpty) {
        write("    fprintf(f, \"$var wire " + mod.width + " " + names(mod) + " " + c.stripComponent(emitRef(mod)) + " $end\\n\");\n");
      }
    }
    write("    fprintf(f, \"$upscope $end\\n\");\n");
  }

  def dumpVCD(c: Module, write: String => Unit): Unit = {
    var num = 0;
    val names = new HashMap[Node, String];
    for (mod <- c.omods) {
      if (mod.isInVCD) {
        names(mod) = "N" + num;
        num += 1;
      }
    }
    write("void " + c.name + "_t::dump(FILE *f, int t) {\n");
    if (Module.isVCD) {
      write("  if (t == 0) {\n");
      write("    fprintf(f, \"$timescale 1ps $end\\n\");\n");
      dumpVCDScope(c, write, c, names);
      dumpScopeForTemps(c, write, names);
      write("    fprintf(f, \"$enddefinitions $end\\n\");\n");
      write("    fprintf(f, \"$dumpvars\\n\");\n");
      write("    fprintf(f, \"$end\\n\");\n");
      write("  }\n");
      write("  fprintf(f, \"#%d\\n\", t);\n");
      for (mod <- c.omods) {
        if (mod.isInVCD && mod.name != "reset") {
          write(emitDef(mod, names(mod)));
        }
      }
    }
    write("}\n");
  }

}

