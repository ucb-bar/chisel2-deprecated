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
Data Hierarchy:

nameable                (src/main/hcl.scala)
    Data                (src/main/Data.scala)
      Bits              (src/main/Bits.scala)
        SInt            (src/main/SInt.scala)
        UInt            (src/main/UInt.scala)
          Bool          (src/main/Bool.scala)
      AggregateData     (src/main/Data.scala)
        Vec             (src/main/Vec.scala)
        Bundle          (src/main/Bundle.scala)
*/

import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test

import Chisel._


class DataSuite extends TestSuite {

  @Test def testBoolFromValue() {
    val tested = Bool(true);
    assertTrue( tested.node.isInstanceOf[Literal] );
    assertFalse( tested.named );
  }

  @Test def testBoolFromDir() {
    val tested = Bool(dir = INPUT);
    assertTrue( tested.node.asInstanceOf[IOBound].dir == INPUT );
    assertFalse( tested.named );
  }

  @Test def testBoolFromDefault() {
    val tested = Bool();
    assertTrue( tested.node.asInstanceOf[IOBound].dir == NODIRECTION );
    assertFalse( tested.named );
  }

  @Test def testSIntFromLit() {
    val fixFromLit = SInt(42);

    assertTrue( fixFromLit.node.isInstanceOf[Literal] );
    assertFalse( fixFromLit.named );
  }

  @Test def testSIntFromLitWithWidth() {
    val fixFromLitWithWidth = SInt(42, width = 16);
    assertTrue( fixFromLitWithWidth.node.isInstanceOf[Literal] );
    assertFalse( fixFromLitWithWidth.named );
    /* XXX width is -1 here for some reason
    assertTrue( fixFromLitWithWidth.width == 16 );
     */
  }

  @Test def testSIntFromWidthDir() {
    val fixFromWidthDir = SInt(width = 8, dir = INPUT);
    assertTrue( fixFromWidthDir.node.width == 8 );
    assertTrue( fixFromWidthDir.node.asInstanceOf[IOBound].dir == INPUT );
    assertFalse( fixFromWidthDir.named );
  }

  // Testing the UInt factory methods

  @Test def testUIntVal() {
    // apply(x: Int): UInt
    val dat = UInt(5)
    assertTrue( dat.node.width == 3 );
    assertTrue( dat.node.isInstanceOf[Literal] );
    assertFalse( dat.named );
  }

  @Test def testUIntValWidth() {
    // def apply(x: Int, width: Int): UInt
    val dat = UInt(5, 4)
    assertTrue( dat.node.width == 4 )
    assertTrue( dat.node.isInstanceOf[Literal] )
    assertFalse( dat.named );
  }

  /* XXX This test interfers with others declared in NameTest.scala
  @Test def testUIntString() {
    // def apply(x: String): UInt
    val dat = UInt("1010")
    assertTrue( dat.node.width == -1 ); // XXX
    assertTrue( dat.node.asInstanceOf[IOBound].dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }
   */

  @Test def testUIntStringWidth() {
    // def apply(x: String, width: Int): UInt
    val dat = UInt("101", 4)
    assertTrue( dat.node.width == 4 )
    assertTrue( dat.node.isInstanceOf[Literal] )
    assertFalse( dat.named )
  }

  @Test def testUIntStringBaseBinary() {
    // def apply(x: String, base: Char): UInt
    val dat = UInt("1010", 'b')
    assertTrue( dat.node.width == 4 )
    assertTrue( dat.node.isInstanceOf[Literal] )
    assertFalse( dat.named )
  }

  @Test def testUIntStringBaseOctal() {
    // def apply(x: String, base: Char): UInt
    val dat = UInt("644", 'o')
    assertTrue( dat.node.width == 9 );
    assertTrue( dat.node.isInstanceOf[Literal] )
    assertFalse( dat.named );
  }

  /* XXX This test interfers with others declared in NameTest.scala
  @Test def testUIntStringBaseDec() {
    // def apply(x: String, base: Char): UInt
    val dat = UInt("199", 'd')
    assertTrue( dat.node.width == -1 );
    assertTrue( dat.node.asInstanceOf[IOBound].dir == OUTPUT );
    assertFalse( dat.isSigned );
    assertTrue( dat.assigned );
    assertFalse( dat.named );
  }
   */

  @Test def testUIntStringBaseHex() {
    // def apply(x: String, base: Char): UInt
    val dat = UInt("abc", 'h')
    assertTrue( dat.node.width == 12 )
    assertTrue( dat.node.isInstanceOf[Literal] )
    assertFalse( dat.named )
  }

  @Test def testUIntDirWidth() {
    // def apply(dir: IODirection = null, width: Int = -1): UInt
    val dat = UInt(INPUT, 4)
    assertTrue( dat.node.width == 4 );
    assertTrue( dat.node.asInstanceOf[IOBound].dir == INPUT );
    assertFalse( dat.named );
  }

  /** The code used to bypass the width initialization resulting
    in incorrect code dat_t<0> which lead to incorrect VCD output.
    This is not the case anymore.

    A clock_hi and clock_lo used to be generated as well but since
    there are no registers nor memory in this circuit this seemed
    to be an error to so. This is fixed as well.
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
      "--targetDir", dir.getPath.toString()),
      () => Module(new BypassDataComp))
    assertFile("DataSuite_BypassDataComp_1.h")
  }

  /** Test case derived from issue #1 reported on github.
    Check an out-of-range bit extract throws an exception.
    */
  @Test def testBuildCarryChain() {
    try {
    class CarryChainComp(size: Int) extends Module {
      val io = new Bundle {
        val r = UInt(INPUT, width=size)
        val p = UInt(INPUT, width=size)
        val out = UInt(OUTPUT)
      }
      val grant_pass1 = ~io.r + io.p;
      val grant_pass2 = ~io.r + UInt(1, size);
      io.out := Mux(grant_pass1(size),
        io.r & grant_pass2(size-1, 0), io.r & grant_pass1(size-1, 0));
    }

    chiselMain(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString()),
      () => Module(new CarryChainComp(4)))
    } catch {
      case _ : Throwable => assertTrue(!ChiselError.ChiselErrors.isEmpty);
    }
  }

}


