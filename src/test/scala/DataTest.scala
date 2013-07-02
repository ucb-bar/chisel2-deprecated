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
        Fix             (src/main/Fix.scala)
        UFix            (src/main/UFix.scala)
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
    /* XXX In the same situation Fix direction shows up as INPUT */
    assertTrue( tested.dir == null );
    assertFalse( tested.assigned );
    assertFalse( tested.named );
  }

  @Test def testFixFromLit() {
    val fixFromLit = Fix(42);

    assertTrue( fixFromLit.dir == OUTPUT );
    assertTrue( fixFromLit.isSigned ); /* XXX why defined in Node? */
    assertTrue( fixFromLit.assigned );
    assertFalse( fixFromLit.named );
  }

  @Test def testFixFromLitWithWidth() {
    val fixFromLitWithWidth = Fix(42, width = 16);
    assertTrue( fixFromLitWithWidth.dir == OUTPUT );
    assertTrue( fixFromLitWithWidth.isSigned );
    assertTrue( fixFromLitWithWidth.assigned );
    assertFalse( fixFromLitWithWidth.named );
    /* XXX width is -1 here for some reason
    assertTrue( fixFromLitWithWidth.width == 16 );
     */
  }

  @Test def testFixFromWidthDir() {
    val fixFromWidthDir = Fix(width = 8, dir = INPUT);
    assertTrue( fixFromWidthDir.width == 8 );
    assertTrue( fixFromWidthDir.dir == INPUT );
    assertTrue( fixFromWidthDir.isSigned );
    assertFalse( fixFromWidthDir.assigned );
    assertFalse( fixFromWidthDir.named );
  }

  // Testing the UFix factory methods

  @Test def testUFixVal() {
    // apply(x: Int): UFix
    val dat = UFix(5)
    assertTrue( dat.width == -1 ); // XXX ??
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  @Test def testUFixValWidth() {
    // def apply(x: Int, width: Int): UFix
    val dat = UFix(5, 4)
    assertTrue( dat.width == -1 ); // XXX ??
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  /* XXX This test interfers with others declared in NameTest.scala
  @Test def testUFixString() {
    // def apply(x: String): UFix
    val dat = UFix("1010")
    assertTrue( dat.width == -1 ); // XXX
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }
   */

  @Test def testUFixStringWidth() {
    // def apply(x: String, width: Int): UFix
    val dat = UFix("101", 4)
    assertTrue( dat.width == -1 ); // XXX ??
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  @Test def testUFixStringBaseBinary() {
    // def apply(x: String, base: Char): UFix
    val dat = UFix("1010", 'b')
    assertTrue( dat.width == -1 );
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  @Test def testUFixStringBaseOctal() {
    // def apply(x: String, base: Char): UFix
    val dat = UFix("644", 'o')
    assertTrue( dat.width == -1 );
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  /* XXX This test interfers with others declared in NameTest.scala
  @Test def testUFixStringBaseDec() {
    // def apply(x: String, base: Char): UFix
    val dat = UFix("199", 'd')
    assertTrue( dat.width == -1 );
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }
   */

  @Test def testUFixStringBaseHex() {
    // def apply(x: String, base: Char): UFix
    val dat = UFix("abc", 'h')
    assertTrue( dat.width == -1 );
    assertTrue( dat.dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }

  @Test def testUFixDirWidth() {
    // def apply(dir: IODirection = null, width: Int = -1): UFix
    val dat = UFix(INPUT, 4)
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
      val data = UFix(INPUT, width=num_bypass_ports)
      val valid = Vec.fill(num_bypass_ports){Bool()}
        // XXX Mod.findRoots does not support Vec as a graph root.
      def get_num_ports: Int = num_bypass_ports
    }

    class BypassDataComp extends Mod {
      val io = new BypassData(3)

      io.valid := io.data | UFix(1)
      debug(io.valid)
    }

    chiselMain(Array[String]("--c",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Mod(new BypassDataComp))
    assertFile(tmpdir.getRoot() + "/DataSuite_BypassDataComp_1.h",
"""#ifndef __DataSuite_BypassDataComp_1__
#define __DataSuite_BypassDataComp_1__

#include "emulator.h"

class DataSuite_BypassDataComp_1_t : public mod_t {
 public:
  dat_t<0> DataSuite_BypassDataComp_1__io_valid;

  void init ( bool rand_init = false );
  void clock_lo ( dat_t<1> reset );
  void clock_hi ( dat_t<1> reset );
  void print ( FILE* f );
  bool scan ( FILE* f );
  void dump ( FILE* f, int t );
};

#endif
""")
  }

}


