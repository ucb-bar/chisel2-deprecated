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

import scala.collection.mutable.{ArrayBuffer, HashSet, ListBuffer, Queue=>ScalaQueue, Stack}
import PartitionIslands._

class PartionedCppBackend extends CppBackend {

  override def elaborate(c: Module): Unit = {
    backendElaborate(c)
    c.findConsumers()
    ChiselError.checkpoint()

    val vcd = new VcdBackend(c)
    val islands = createIslands(c)
    for (chain <- islands) {
      renameNodes(c, chain.nodes.toBuffer)
      val cName = c.name + "chain_" + chain.islandId.toString
      val out_h = createOutputFile(cName + ".h")
      val out_c = createOutputFile(cName + ".cpp")
      out_h.write("#ifndef __" + c.name + "__\n");
      out_h.write("#define __" + c.name + "__\n\n");
      out_h.write("#include \"emulator.h\"\n\n");
      out_h.write("class " + c.name + "_t : public mod_t {\n");
      out_h.write(" public:\n");
      for (m <- chain.nodes) {
        if(m.name != "reset") {
          if (m.isInObject) {
            out_h.write(emitDec(m));
          }
          if (m.isInVCD) {
            out_h.write(vcd.emitDec(m));
          }
        }
      }

      out_h.write("\n");
      out_h.write("  void init ( val_t rand_init = false );\n");
      for ((vertex, i) <- islands.zipWithIndex) {
        out_h.write("  void clock_lo_" + i + " ( dat_t<1> reset );\n")
      }
      out_h.write("  void clock_lo ( dat_t<1> reset );\n")
      out_h.write("  void clock_hi ( dat_t<1> reset );\n")
      out_h.write("  void print ( FILE* f );\n");
      out_h.write("  void dump ( FILE* f, int t );\n");
      out_h.write("  void dump_init ( FILE* f );\n");
      out_h.write("};\n\n");
      out_h.write("#endif\n");
      out_h.close();
  
      out_c.write("#include \"" + cName + ".h\"\n");
      for (str <- Driver.includeArgs) out_c.write("#include \"" + str + "\"\n")
      out_c.write("\n");
      out_c.write("void " + c.name + "_t::init ( val_t rand_init ) {\n");
      for (m <- chain.nodes) {
        out_c.write(emitInit(m));
      }
      out_c.write("}\n");
  
      out_c.write("void " + c.name + "_t::clock_lo_" + chain.islandId + " ( dat_t<1> reset ) {\n")
      for (m <- chain.nodes) {
        out_c.write(emitDefLo(m))
      }
      out_c.write("}\n")
  
      out_c.write("void " + c.name + "_t::clock_lo ( dat_t<1> reset ) {\n")
      out_c.write("  clock_lo_" + chain.islandId + "( reset );\n")
      out_c.write("}\n")
      out_c.write("void " + c.name + "_t::clock_hi ( dat_t<1> reset ) {\n")
      for (m <- chain.nodes) {
        out_c.write(emitInitHi(m))
      }
      for (m <- chain.nodes) {
        out_c.write(emitDefHi(m))
      }
      out_c.write("}\n")
  
      out_c.write("void " + c.name + "_t::print ( FILE* f ) {\n");
      for (cc <- Driver.components; p <- cc.printfs)
        out_c.write("#if __cplusplus >= 201103L\n"
          + "  if (" + emitLoWordRef(p.cond)
          + ") dat_fprintf<" + p.width + ">(f, "
          + p.args.map(emitRef _).foldLeft(CString(p.format))(_ + ", " + _)
          + ");\n"
          + "#endif\n")
      out_c.write("}\n");
      vcd.dumpVCDInit(out_c.write);
      vcd.dumpVCD(out_c.write);
      out_c.close();
    }

    /* Copy the emulator.h file into the targetDirectory. */
    val resourceStream = getClass().getResourceAsStream("/emulator.h")
    if( resourceStream != null ) {
      val classFile = createOutputFile("emulator.h")
      while(resourceStream.available > 0) {
        classFile.write(resourceStream.read())
      }
      classFile.close()
      resourceStream.close()
    }
  }
}
