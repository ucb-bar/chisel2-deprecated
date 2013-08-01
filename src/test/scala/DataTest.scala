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


/*
Node Hierarchy:

nameable                (src/main/hcl.scala)
  Node                  (src/main/Node.scala)
    Delay               (src/main/hcl.scala)
      Reg               (src/main/Reg.scala)
      AccessTracker     (src/main/Mem.scala)
        Mem             (src/main/Mem.scala)
    Data                (src/main/Data.scala)
      Bits with proc    (src/main/Bits.scala, src/main/scala/hcl.scala)
        SInt             (src/main/SInt.scala)
        UInt            (src/main/UInt.scala)
          Bool          (src/main/Bool.scala)
      CompositeData     (src/main/Data.scala)
        Vec             (src/main/Vec.scala)
        Bundle          (src/main/Bundle.scala)
*/

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After
import org.junit.rules.TemporaryFolder;

import Chisel._


class DataSuite extends AssertionsForJUnit {

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

  @Test def testBoolFromValue() {
    val tested = Bool(true);
    assertTrue( tested.dir == OUTPUT );
    assertTrue( tested.assigned );
    assertFalse( tested.named );
  }

  @Test def testBoolFromDir() {
    val tested = Bool(dir = INPUT);
    assertTrue( tested.dir == INPUT );
    assertFalse( tested.assigned );
    assertFalse( tested.named );
  }

  @Test def testBoolFromDefault() {
    val tested = Bool();
    /* XXX In the same situation SInt direction shows up as INPUT */
    assertTrue( tested.dir == null );
    assertFalse( tested.assigned );
    assertFalse( tested.named );
  }

  @Test def testSIntFromLit() {
    val fixFromLit = SInt(42);

    assertTrue( fixFromLit.dir == OUTPUT );
    assertTrue( fixFromLit.isSigned ); /* XXX why defined in Node? */
    assertTrue( fixFromLit.assigned );
    assertFalse( fixFromLit.named );
  }

  @Test def testSIntFromLitWithWidth() {
    val fixFromLitWithWidth = SInt(42, width = 16);
    assertTrue( fixFromLitWithWidth.dir == OUTPUT );
    assertTrue( fixFromLitWithWidth.isSigned );
    assertTrue( fixFromLitWithWidth.assigned );
    assertFalse( fixFromLitWithWidth.named );
    /* XXX width is -1 here for some reason
    assertTrue( fixFromLitWithWidth.width == 16 );
     */
  }

  @Test def testSIntFromWidthDir() {
    val fixFromWidthDir = SInt(width = 8, dir = INPUT);
    assertTrue( fixFromWidthDir.width == 8 );
    assertTrue( fixFromWidthDir.dir == INPUT );
    assertTrue( fixFromWidthDir.isSigned );
    assertFalse( fixFromWidthDir.assigned );
    assertFalse( fixFromWidthDir.named );
  }

  // Testing the UInt factory methods

  @Test def testUIntVal() {
    // apply(x: Int): UInt
    val dat = UInt(5)
    assertTrue( dat.width == -1 ); // XXX ??
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  @Test def testUIntValWidth() {
    // def apply(x: Int, width: Int): UInt
    val dat = UInt(5, 4)
    assertTrue( dat.width == -1 ); // XXX ??
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  /* XXX This test interfers with others declared in NameTest.scala
  @Test def testUIntString() {
    // def apply(x: String): UInt
    val dat = UInt("1010")
    assertTrue( dat.width == -1 ); // XXX
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }
   */

  @Test def testUIntStringWidth() {
    // def apply(x: String, width: Int): UInt
    val dat = UInt("101", 4)
    assertTrue( dat.width == -1 ); // XXX ??
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  @Test def testUIntStringBaseBinary() {
    // def apply(x: String, base: Char): UInt
    val dat = UInt("1010", 'b')
    assertTrue( dat.width == -1 );
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  @Test def testUIntStringBaseOctal() {
    // def apply(x: String, base: Char): UInt
    val dat = UInt("644", 'o')
    assertTrue( dat.width == -1 );
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  /* XXX This test interfers with others declared in NameTest.scala
  @Test def testUIntStringBaseDec() {
    // def apply(x: String, base: Char): UInt
    val dat = UInt("199", 'd')
    assertTrue( dat.width == -1 );
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }
   */

  @Test def testUIntStringBaseHex() {
    // def apply(x: String, base: Char): UInt
    val dat = UInt("abc", 'h')
    assertTrue( dat.width == -1 );
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  @Test def testUIntDirWidth() {
    // def apply(dir: IODirection = null, width: Int = -1): UInt
    val dat = UInt(INPUT, 4)
    assertTrue( dat.width == 4 );
    assertTrue( dat.dir == INPUT );
    assertFalse( dat.isSigned );
    assertFalse( dat.assigned );
    assertFalse( dat.named );
  }

  /** The statement new Bool bypasses the width initialization resulting
    in incorrect code dat_t<0> which leads to incorrect VCD output.

    XXX Chisel should generate an error message!
    XXX Incorrect until we compute debug roots correctly.
    */
  @Test def testBypassData() {
    class BypassData(num_bypass_ports:Int) extends Bundle() {
      val data = UInt(INPUT, width=num_bypass_ports)
      val valid = Vec.fill(num_bypass_ports){Bool()}
        // XXX Module.findRoots does not support Vec as a graph root.
      def get_num_ports: Int = num_bypass_ports
    }

    class BypassDataComp extends Module {
      val io = new BypassData(3)

      io.valid := io.data | UInt(1)
      debug(io.valid)
    }

    chiselMain(Array[String]("--backend", "c",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new BypassDataComp))
    assertFile(tmpdir.getRoot() + "/DataSuite_BypassDataComp_1.h",
"""#ifndef __DataSuite_BypassDataComp_1__
#define __DataSuite_BypassDataComp_1__

#include "emulator.h"

class DataSuite_BypassDataComp_1_t : public mod_t {
 public:
  dat_t<0> DataSuite_BypassDataComp_1__io_valid;
  int clk;
  int clk_cnt;

  void init ( bool rand_init = false );
  void clock_lo ( dat_t<1> reset );
  void clock_hi ( dat_t<1> reset );
  int clock ( dat_t<1> reset );
  void print ( FILE* f );
  bool scan ( FILE* f );
  void dump ( FILE* f, int t );
};

#endif
""")
  }

}


