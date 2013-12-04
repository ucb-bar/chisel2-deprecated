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

import scala.collection.mutable.ArrayBuffer
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After
import org.junit.rules.TemporaryFolder;

import Chisel._

/** This testsuite checks features dealing with debugging Chisel code.
*/
class VerifSuite extends AssertionsForJUnit {

  val tmpdir = new TemporaryFolder();

  @Before def initialize() {
    tmpdir.create()
  }

  @After def done() {
    tmpdir.delete()
  }

  def assertFile( filename: String, content: String ) {
    val source = scala.io.Source.fromFile(filename, "utf-8")
    val lines = source.mkString
    source.close()
    assert(lines === content)
  }

  @Test def testAssertCpp() {
    println("testAssertCpp:")
    class CppAssertComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 8)
        val y = UInt(INPUT, 8)
        val z = UInt(OUTPUT)
      }
      assert(Bool(io.x == io.y), "failure")
      io.z := Cat(io.x, io.y)
    }

    chiselMain(Array[String]("--backend", "c",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new CppAssertComp()))
    assertFile(tmpdir.getRoot() + "/VerifSuite_CppAssertComp_1.cpp",
"""#include "VerifSuite_CppAssertComp_1.h"

void VerifSuite_CppAssertComp_1_t::init ( bool rand_init ) {
}
void VerifSuite_CppAssertComp_1_t::clock_lo_clk ( dat_t<1> reset ) {
  val_t T1__w0;
  { T1__w0 = VerifSuite_CppAssertComp_1__io_y.values[0] | VerifSuite_CppAssertComp_1__io_x.values[0] << 8; }
  { VerifSuite_CppAssertComp_1__io_z.values[0] = T1__w0; }
  ASSERT(reset.values[0], "failure");
}
void VerifSuite_CppAssertComp_1_t::clock_hi_clk ( dat_t<1> reset ) {
}
int VerifSuite_CppAssertComp_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk_cnt < min) min = clk_cnt;
  clk_cnt-=min;
  if (clk_cnt == 0) clock_lo_clk( reset );
  if (clk_cnt == 0) clock_hi_clk( reset );
  if (clk_cnt == 0) clk_cnt = clk;
  return min;
}
void VerifSuite_CppAssertComp_1_t::print ( FILE* f ) {
}
bool VerifSuite_CppAssertComp_1_t::scan ( FILE* f ) {
  return(!feof(f));
}
void VerifSuite_CppAssertComp_1_t::dump(FILE *f, int t) {
}
""")
  }

  @Test def testAssertVerilog() {

    class VerilogAssertComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 8)
        val y = UInt(INPUT, 8)
        val z = UInt(OUTPUT)
      }
      assert(Bool(io.x == io.y), "failure")
      io.z := Cat(io.x, io.y)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new VerilogAssertComp()))
  }

  @Test def testPrintfCpp() {

    class CppPrintfComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 8)
        val y = UInt(INPUT, 8)
        val z = UInt(OUTPUT)
      }
      printf("display %x %x", io.x, io.y)
      io.z := Cat(io.x, io.y)
    }
    /** XXX Can't run CppBackend back-to-back in the same process
      because the emulator resource is closed. 
    chiselMain(Array[String]("--backend", "c",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new CppPrintfComp()))
    assertFile(tmpdir.getRoot() + "/VerifSuite_CppPrintfComp_1.cpp",
"""#include "VerifSuite_CppPrintfComp_1.h"

void VerifSuite_CppPrintfComp_1_t::init ( bool rand_init ) {
}
void VerifSuite_CppPrintfComp_1_t::clock_lo ( dat_t<1> reset ) {
  val_t T2__w0;
  { T2__w0 = VerifSuite_CppPrintfComp_1__io_y.values[0] | VerifSuite_CppPrintfComp_1__io_x.values[0] << 8; }
  { VerifSuite_CppPrintfComp_1__io_z.values[0] = T2__w0; }
  T0.values[0] = !reset.values[0];
}
void VerifSuite_CppPrintfComp_1_t::clock_hi ( dat_t<1> reset ) {
}
void VerifSuite_CppPrintfComp_1_t::print ( FILE* f ) {
  if (T0.values[0]) dat_fprintf(f, "display %h %h", VerifSuite_CppPrintfComp_1__io_x, VerifSuite_CppPrintfComp_1__io_y);
}
bool VerifSuite_CppPrintfComp_1_t::scan ( FILE* f ) {
  return(!feof(f));
}
void VerifSuite_CppPrintfComp_1_t::dump(FILE *f, int t) {
}
""")
      */
  }

  @Test def testPrintfVerilog() {

    class VerilogPrintfComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 8)
        val y = UInt(INPUT, 8)
        val z = UInt(OUTPUT)
      }

      val tsc_reg = Reg(init=UInt(0, width=32))
      tsc_reg := tsc_reg + UInt(1/*, width=32*/)

      printf("Cyc= %d io: %x %x", tsc_reg(31,0), io.x, io.y)
      io.z := Cat(io.x, io.y)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new VerilogPrintfComp()))
  }
}
