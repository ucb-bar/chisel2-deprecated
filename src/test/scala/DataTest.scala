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
import scala.collection.mutable.HashMap
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._


class DataSuite extends TestSuite {

  @Test def testBoolFromValue() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val tested = Bool(true);
  //    assertTrue( tested.isInstanceOf[Literal] );
      assertFalse( tested.named );
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testBoolFromDir() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val tested = Bool(dir = INPUT);
      assertTrue( tested.dir == INPUT );
      assertFalse( tested.named );
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testBoolFromDefault() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val tested = Bool();
      assertTrue( tested.dir == NODIR );
      assertFalse( tested.named );
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testSIntFromLit() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val fixFromLit = SInt(42);

   //   assertTrue( fixFromLit.isInstanceOf[Literal] );
      assertFalse( fixFromLit.named );
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testSIntFromLitWithWidth() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val fixFromLitWithWidth = SInt(42, width = 16);
      // assertTrue( fixFromLitWithWidth.isInstanceOf[Literal] );
      assertFalse( fixFromLitWithWidth.named );
      /* XXX width was -1 here for some reason */
      assertTrue( fixFromLitWithWidth.getWidth() == 16 );
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testSIntFromWidthDir() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val fixFromWidthDir = SInt(width = 8, dir = INPUT);
      assertTrue( fixFromWidthDir.getWidth() == 8 );
      assertTrue( fixFromWidthDir.dir == INPUT );
      assertFalse( fixFromWidthDir.named );
    }
    val dummyInst = Module(new Dummy)
  }

  // Testing the UInt factory methods

  @Test def testUIntVal() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      // apply(x: Int): UInt
      val dat = UInt(5)
      assertTrue( dat.getWidth() == 3 );
      // assertTrue( dat.isInstanceOf[Literal] );
      assertFalse( dat.named );
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testUIntValWidth() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      // def apply(x: Int, width: Int): UInt
      val dat = UInt(5, 4)
      assertTrue( dat.getWidth() == 4 )
      // assertTrue( dat.isInstanceOf[Literal] )
      assertFalse( dat.named );
    }
    val dummyInst = Module(new Dummy)
  }

  /* XXX This test interfers with others declared in NameTest.scala
  @Test def testUIntString() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      // def apply(x: String): UInt
      val dat = UInt("1010")
      assertTrue( dat.width == -1 ); // XXX
      assertTrue( dat.dir == OUTPUT );
      assertFalse( dat.isSigned );
      assertTrue( dat.assigned );
      assertFalse( dat.named );
    }
    val dummyInst = Module(new Dummy)
  }
   */

  @Test def testUIntStringWidth() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      // def apply(x: String, width: Int): UInt
      val dat = UInt("101", 4)
      assertTrue( dat.getWidth() == 4 )
      // assertTrue( dat.isInstanceOf[Literal] )
      assertFalse( dat.named )
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testUIntStringBaseBinary() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      // def apply(x: String, base: Char): UInt
      val dat = UInt("1010", 'b')
      assertTrue( dat.getWidth() == 4 )
      // assertTrue( dat.isInstanceOf[Literal] )
      assertFalse( dat.named )
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testUIntStringBaseOctal() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      // def apply(x: String, base: Char): UInt
      val dat = UInt("644", 'o')
      assertTrue( dat.getWidth() == 9 );
      // assertTrue( dat.isInstanceOf[Literal] )
      assertFalse( dat.named );
    }
    val dummyInst = Module(new Dummy)
  }

  /* XXX This test interfers with others declared in NameTest.scala
  @Test def testUIntStringBaseDec() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      // def apply(x: String, base: Char): UInt
      val dat = UInt("199", 'd')
      assertFalse( dat.width.isSet );
      assertTrue( dat.dir == OUTPUT );
      assertFalse( dat.isSigned );
      assertTrue( dat.assigned );
      assertFalse( dat.named );
    }
    val dummyInst = Module(new Dummy)
  }
   */

  @Test def testUIntStringBaseHex() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      // def apply(x: String, base: Char): UInt
      val dat = UInt("abc", 'h')
      assertTrue( dat.getWidth() == 12 )
      // assertTrue( dat.isInstanceOf[Literal] )
      assertFalse( dat.named )
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testUIntDirWidth() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      // def apply(dir: IODirection = None, width: Int = -1): UInt
      val dat = UInt(INPUT, 4)
      assertTrue( dat.getWidth() == 4 );
      assertTrue( dat.dir == INPUT );
      assertFalse( dat.named );
    }
    val dummyInst = Module(new Dummy)
  }

  /** The code used to bypass the width initialization resulting
    in incorrect code dat_t<0> which lead to incorrect VCD output.
    This is not the case anymore.

    A clock_hi and clock_lo used to be generated as well but since
    there are no registers nor memory in this circuit this seemed
    to be an error to so. This is fixed as well.
    */
  @Test def testBypassData() {
    try {
    class BypassDataIO(num_bypass_ports:Int) extends Bundle() {
      val data = UInt(INPUT, width=num_bypass_ports)
      val valid = Vec(num_bypass_ports,  Bool() )
        // XXX Module.findRoots does not support Vec as a graph root.
      def get_num_ports: Int = num_bypass_ports
    }
    class BypassDataComp extends Module {
      val io = new BypassDataIO(3)
      io.valid := io.data | UInt(1)
    }
    chiselMain(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString()),
      () => Module(new BypassDataComp))
    // assertFile("DataSuite_BypassDataComp_1.h")
    } catch {
      case _ : Throwable => ;
    }
    assertTrue(ChiselError.hasErrors);
  }

  // tests assigning to non parent's outputs
  @Test def testAssignToChildOutput() {
    try {
    class Child extends Module {
      val io = new Bundle {
        val input  = Bits(INPUT, width = 8)
        val output = Bits(OUTPUT, width = 8)
      }
      io.output := io.input
    }

    class Parent extends Module {
      val io = new Bundle {
        val input  = Bits(INPUT, width = 8)
        val output = Bits(OUTPUT, width = 8)
      }
      val child = Module(new Child)
      // child.io.input := io.input
      child.io.output := io.input
      io.output := child.io.output
    }

    chiselMain(Array[String]("--backend", "v"), () => Module(new Parent()))

    } catch {
      case _ : Throwable => ;
    }
    assertTrue(ChiselError.hasErrors);
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
      case _ : Throwable => ;
    }
    assertTrue(ChiselError.hasErrors);
  }


  /* Vec width (#247) */
  @Test def testVecWidth() {
    val io = new Bundle{

      val in = Vec(4, UInt(INPUT,4))
      val out = Vec(4, UInt(OUTPUT,4))
    }

    assertTrue( io.in.getWidth() == 16 )
  }


  /** Infinite Width Inference #76
   *
   */
  @Test def widthInfinInfer() {
    println("\nwidthInfinInfer ...")
    class WidthInfinInfer extends Bundle {
      val num_entries = 2
      val debug = new Bundle
      {
         val entry = Vec(num_entries,   new Bundle {
            val valid = Bool()
            val eflags = UInt() // THIS IS THE CULPRIT
         })
       }.asOutput
    }
  }

}


