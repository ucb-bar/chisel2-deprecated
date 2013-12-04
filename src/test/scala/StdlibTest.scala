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

/** This testsuite checks the primitives of the standard library
  that will generate basic common graphs of *Node*.
*/
class StdlibSuite extends AssertionsForJUnit {

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

  /** test of simple operators */
  @Test def testOperators() {
try {
    class OperatorComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 8)
        val y = UInt(INPUT, 8)
        val ys = SInt(INPUT, 8)
        val z = UInt(OUTPUT)
        val zb = Bool(OUTPUT)
      }

      // apply(bit: Int): Bool
      val a = io.x(0).toBits

      // apply(hi: Int, lo: Int): UInt
      val b = io.x(4, 3)
      //val c = io.x(3, 4) XXX This will throw an Assertion failure instead of an error
      val d = io.x(3, 3)
      val e = io.x(9, -1)

      // apply(bit: UInt): Bool
      val a1 = io.x(UInt(0)).toBits

      // apply(hi: UInt, lo: UInt): UInt
      val b1 = io.x(UInt(4), UInt(3))
      //val c1 = io.x(UInt(3), UInt(4)) XXX This will throw an Assertion failure instead of an error
      val d1 = io.x(UInt(3), UInt(3))
      val e1 = io.x(UInt(9), UInt(-1))

      // apply(range: (Int, Int)): UInt
      val f = io.x((5, 3))

      // unary_-(): UInt
      val g = - io.x

      // unary_~(): UInt
      val h = ~io.x

      // andR(): Bool
      val i = io.x.andR.toBits

      // orR():  Bool
      val j = io.x.orR.toBits

      // xorR():  Bool
      val k = io.x.xorR.toBits

      // << (b: UInt): UInt
      val l = io.x << a

      // >> (b: UInt): UInt
      val m = io.x >> a

      // +  (b: UInt): UInt
      val n = io.x + io.y

      // *  (b: UInt): UInt
      val o = io.x * io.y

      // ^  (b: UInt): UInt
      val r = io.x ^ io.y

      // XXX disabled seems left over from previous attempts at implementing
      // Mux: ?  (b: UInt): UInt
      //val s = io.x ? io.y

      // -  (b: UInt): UInt
      val t = io.x - io.y

      // ## (b: UInt): UInt
      val u = io.x ## io.y

      // &  (b: UInt): UInt
      val ab = io.x & io.y

      // |  (b: UInt): UInt
      val ac = io.x | io.y

      io.z := (a | b | d
        | a1 | b1 | d1
        | f | g | h | i | j | k
        | l | m | n | o
        | r | u | ab | ac
        /* XXX Computing any of those signals throws an exception */
        /* c | c1 | e | t | e1 | s */
      ).toBits

      // -- result type is Bool --

      // ===(b: UInt): Bool
      val v = io.x === io.y

      // != (b: UInt): Bool
      val w = io.x != io.y

      // >  (b: UInt): Bool
      val x = io.x > io.y

      // <  (b: UInt): Bool
      val y = io.x < io.y

      // <= (b: UInt): Bool
      val z = io.x <= io.y

      // >= (b: UInt): Bool
      val aa = io.x >= io.y

      io.zb := (v | w | x | y | z | aa)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new OperatorComp()))
    } catch {
      case e => e.printStackTrace()
    }
  }

  /** Multiply an unsigned number by signed number */
  @Test def testMulUS() {
    println("\ntestMulUS ...")
    class MulUS extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 32)
        val y = SInt(INPUT, 32)
        val z = SInt(OUTPUT)
      }
      io.z := io.x * io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new MulUS()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_MulUS_1.v",
"""module StdlibSuite_MulUS_1(
    input [31:0] io_x,
    input [31:0] io_y,
    output [63:0] io_z
);

  wire [63:0] T0;
  wire [32:0] T1;

  assign io_z = T0;
  assign T0 = $signed(io_y) * $signed(T1);
  assign T1 = {1'h0/* 0*/, io_x};
endmodule

""")
  }

  /** Divide an unsigned number by signed number */
  @Test def testDivUS() {
    println("\ntestDivUS ...")
    class DivUS extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 32)
        val y = SInt(INPUT, 32)
        val z = SInt(OUTPUT)
      }
      io.z := io.x / io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new DivUS()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_DivUS_1.v",
"""module StdlibSuite_DivUS_1(
    input [31:0] io_x,
    input [31:0] io_y,
    output [31:0] io_z
);

  wire [31:0] T0;
  wire [32:0] T1;

  assign io_z = T0;
  assign T0 = $signed(T1) / $signed(io_y);
  assign T1 = {1'h0/* 0*/, io_x};
endmodule

""")
  }

  /** Remainer of an unsigned number by signed number */
  @Test def testRemUS() {
    println("\ntestRemUS ...")
    class RemUS extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 32)
        val y = SInt(INPUT, 32)
        val z = SInt(OUTPUT)
      }
      io.z := io.x % io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new RemUS()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_RemUS_1.v",
"""module StdlibSuite_RemUS_1(
    input [31:0] io_x,
    input [31:0] io_y,
    output [31:0] io_z
);

  wire [31:0] T0;
  wire [32:0] T1;

  assign io_z = T0;
  assign T0 = $signed(T1) % $signed(io_y);
  assign T1 = {1'h0/* 0*/, io_x};
endmodule

""")
  }

  /** Multiply an signed number by an unsigned number */
  @Test def testMulSU() {
    println("\ntestMulSU ...")
    class MulSU extends Module {
      val io = new Bundle {
        val x = SInt(INPUT, 32)
        val y = UInt(INPUT, 32)
        val z = SInt(OUTPUT)
      }
      io.z := io.x * io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new MulSU()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_MulSU_1.v",
"""module StdlibSuite_MulSU_1(
    input [31:0] io_x,
    input [31:0] io_y,
    output [63:0] io_z
);

  wire [63:0] T0;
  wire [32:0] T1;

  assign io_z = T0;
  assign T0 = $signed(io_x) * $signed(T1);
  assign T1 = {1'h0/* 0*/, io_y};
endmodule

""")
  }

  /** Divide a signed number by an unsigned number */
  @Test def testDivSU() {
    println("\ntestDivSU ...")
    class DivSU extends Module {
      val io = new Bundle {
        val x = SInt(INPUT, 32)
        val y = UInt(INPUT, 32)
        val z = SInt(OUTPUT)
      }
      io.z := io.x / io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new DivSU()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_DivSU_1.v",
"""module StdlibSuite_DivSU_1(
    input [31:0] io_x,
    input [31:0] io_y,
    output [31:0] io_z
);

  wire [31:0] T0;
  wire [32:0] T1;

  assign io_z = T0;
  assign T0 = $signed(io_x) / $signed(T1);
  assign T1 = {1'h0/* 0*/, io_y};
endmodule

""")
  }

  /** Remainer of a signed number by an unsigned number */
  @Test def testRemSU() {
    println("\ntestRemSU ...")
    class RemSU extends Module {
      val io = new Bundle {
        val x = SInt(INPUT, 32)
        val y = UInt(INPUT, 32)
        val z = SInt(OUTPUT)
      }
      io.z := io.x % io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new RemSU()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_RemSU_1.v",
"""module StdlibSuite_RemSU_1(
    input [31:0] io_x,
    input [31:0] io_y,
    output [31:0] io_z
);

  wire [31:0] T0;
  wire [32:0] T1;

  assign io_z = T0;
  assign T0 = $signed(io_x) % $signed(T1);
  assign T1 = {1'h0/* 0*/, io_y};
endmodule

""")
  }

  /** Assign a Bundle */
  @Test def testAssignBundle() {
    println("\ntestAssignBundle ...")
    class AssignBundle extends Bundle {
        val v = Vec.fill(2){UInt(INPUT, 2)}
    }
    class AssignBundleComp extends Module {
      val io = new Bundle {
        val in = new AssignBundle()
        val out = new AssignBundle().flip
      }

      io.out := io.in
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new AssignBundleComp()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_AssignBundleComp_1.v",
"""module StdlibSuite_AssignBundleComp_1(
    input [1:0] io_in_v_0,
    input [1:0] io_in_v_1,
    output [1:0] io_out_v_0,
    output [1:0] io_out_v_1
);


  assign io_out_v_0 = io_in_v_0;
  assign io_out_v_1 = io_in_v_1;
endmodule

""")
  }

  /** Concatenate two nodes X and Y in a node Z such that
    Z[0..wx+wy] = X[0..wx] :: Y[0..wy]. */
  @Test def testCat() {
    println("\ntestCat ...")
    class CatComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 8)
        val y = UInt(INPUT, 8)
        val z = UInt(OUTPUT)
      }
      io.z := io.x ## io.y
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new CatComp()))
assertFile(tmpdir.getRoot() + "/StdlibSuite_CatComp_1.v",
"""module StdlibSuite_CatComp_1(
    input [7:0] io_x,
    input [7:0] io_y,
    output [15:0] io_z
);

  wire [15:0] T0;

  assign io_z = T0;
  assign T0 = {io_x, io_y};
endmodule

""")
  }

  /** Generate a lookup into an array.
    XXX Lookup.scala, use different code based on instance of CppBackend. */
  @Test def testLookup() {
    println("\ntestLookup ...")
    class LookupComp extends Module {
      val io = new Bundle {
        val addr = UInt(INPUT, 8)
        val data = UInt(OUTPUT)
      }
      io.data := Lookup(io.addr, UInt(0), Array(
        (UInt(0), UInt(10)),
        (UInt(1), UInt(11))))
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new LookupComp()))
  }

  /** Generate a PopCount
    */
  @Test def testPopCount() {
    println("\ntestPopCount ...")
    class PopCountComp extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := PopCount(Array(Bool(true), Bool(false)))
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new PopCountComp()))
  }

  /** Generate a Reverse
    */
  @Test def testReverse() {
    println("\ntestReverse ...")
    class ReverseComp extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := Reverse(io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new ReverseComp()))
  }

  /** Generate a ShiftRegister
    */
  @Test def testShiftRegister() {
    println("\ntestShiftRegister ...")
    class ShiftRegisterComp extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := ShiftRegister(io.in, 2)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new ShiftRegisterComp()))
  }

  /** Generate a UIntToOH
    */
  @Test def testUIntToOH() {
    println("\ntestUIntToOH ...")
    class UIntToOHComp extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 8)
        val out0 = UInt(OUTPUT)
        val out1 = UInt(OUTPUT)
      }
      io.out0 := UIntToOH(io.in)
      io.out1 := UIntToOH(io.in, 4)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new UIntToOHComp()))
  }

  /** Generate a foldR
    */
  @Test def testfoldR() {
    println("\ntestfoldR ...")
    class foldRComp extends Module {
      val io = new Bundle {
        val in0 = UInt(INPUT, 8)
        val in1 = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := foldR(io.in0 :: io.in1 :: Nil){ _ + _ }
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new foldRComp()))
  }

  /** Generate an Arbiter such that first valid wins.

      Arbiter[T <: Data](gen: T,
           n: Int) extends LockingArbiter[T](gen, n, 1)
      LockingArbiter[T <: Data](gen: T,
           n: Int, count: Int, needsLock: Option[T => Bool] = None)
      LockingArbiterLike[T <: Data](gen: T,
           n: Int, count: Int, needsLock: Option[T => Bool] = None)
    */
  @Test def testArbiter() {
    println("\ntestArbiter ...")
try {
    class ArbiterTest extends Arbiter(SInt(width=8), 4) {
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new ArbiterTest()))
    } catch {
      case e => e.printStackTrace()
    }
    assertFile(tmpdir.getRoot() + "/StdlibSuite_ArbiterTest_1.v",
"""module StdlibSuite_ArbiterTest_1(
    output io_in_0_ready,
    input io_in_0_valid,
    input [7:0] io_in_0_bits,
    output io_in_1_ready,
    input io_in_1_valid,
    input [7:0] io_in_1_bits,
    output io_in_2_ready,
    input io_in_2_valid,
    input [7:0] io_in_2_bits,
    output io_in_3_ready,
    input io_in_3_valid,
    input [7:0] io_in_3_bits,
    input io_out_ready,
    output io_out_valid,
    output [7:0] io_out_bits,
    output [1:0] io_chosen
);

  wire T0;
  wire T1;
  wire T2;
  wire T3;
  wire T4;
  wire T5;
  wire T6;
  wire T7;
  wire T8;
  wire T9;
  wire T10;
  wire T11;
  wire T12;
  wire T13;
  wire T14;
  wire T15;
  wire [1:0] T16;
  wire [1:0] T17;
  wire [1:0] T18;
  wire [1:0] T19;
  wire T20;
  wire T21;
  wire T22;
  wire T23;
  wire [7:0] T24;
  wire T25;
  wire [7:0] T26;
  wire T27;
  wire [7:0] T28;
  wire T29;

  assign io_in_0_ready = T0;
  assign T0 = T1 && io_out_ready;
  assign T1 = 1'h1/* 1*/;
  assign io_in_1_ready = T2;
  assign T2 = T3 && io_out_ready;
  assign T3 = T4;
  assign T4 = ! io_in_0_valid;
  assign io_in_2_ready = T5;
  assign T5 = T6 && io_out_ready;
  assign T6 = T7;
  assign T7 = ! T8;
  assign T8 = io_in_0_valid || io_in_1_valid;
  assign io_in_3_ready = T9;
  assign T9 = T10 && io_out_ready;
  assign T10 = T11;
  assign T11 = ! T12;
  assign T12 = T13 || io_in_2_valid;
  assign T13 = io_in_0_valid || io_in_1_valid;
  assign io_out_valid = T14;
  assign T14 = T15 ? T20 : T22;
  assign T15 = T16[1'h1/* 1*/:1'h1/* 1*/];
  assign T16 = T17;
  assign T17 = io_in_0_valid ? 1'h0/* 0*/ : T18;
  assign T18 = io_in_1_valid ? 1'h1/* 1*/ : T19;
  assign T19 = io_in_2_valid ? 2'h2/* 2*/ : 2'h3/* 3*/;
  assign T20 = T21 ? io_in_3_valid : io_in_2_valid;
  assign T21 = T16[1'h0/* 0*/:1'h0/* 0*/];
  assign T22 = T23 ? io_in_1_valid : io_in_0_valid;
  assign T23 = T16[1'h0/* 0*/:1'h0/* 0*/];
  assign io_out_bits = T24;
  assign T24 = T25 ? T26 : T28;
  assign T25 = T16[1'h1/* 1*/:1'h1/* 1*/];
  assign T26 = T27 ? io_in_3_bits : io_in_2_bits;
  assign T27 = T16[1'h0/* 0*/:1'h0/* 0*/];
  assign T28 = T29 ? io_in_1_bits : io_in_0_bits;
  assign T29 = T16[1'h0/* 0*/:1'h0/* 0*/];
  assign io_chosen = T16;
endmodule

""")
  }

  /** XXX Generate an Arbiter that needs locking.

      Arbiter[T <: Data](gen: T,
           n: Int) extends LockingArbiter[T](gen, n, 1)
      LockingArbiter[T <: Data](gen: T,
           n: Int, count: Int, needsLock: Option[T => Bool] = None)
      LockingArbiterLike[T <: Data](gen: T,
           n: Int, count: Int, needsLock: Option[T => Bool] = None)

  @Test def testArbiterNeedsLock() {
    class ArbiterNeedsLock extends Arbiter(SInt(width=8), 4,
      needsLock=Some()) {
    }

    chiselMain(Array[String]("--v",
    "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new ArbiterNeedsLock()))
    assertFile(tmpdir.getRoot() + "/NeedsLock.v",
"""
""")
  }
    */

  /** Generate a Round-Robin arbiter.

      RRArbiter[T <: Data](gen: T,
           n: Int) extends LockingRRArbiter[T](gen, n, 1)
      LockingRRArbiter[T <: Data](gen: T,
           n: Int, count: Int, needsLock: Option[T => Bool] = None)
      LockingArbiterLike[T <: Data](gen: T,
           n: Int, count: Int, needsLock: Option[T => Bool] = None)
    */
  @Test def testRRArbiter() {
    class RRArbiterTest extends RRArbiter(SInt(width=8), 4) {
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new RRArbiterTest()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_RRArbiterTest_1.v",
"""module StdlibSuite_RRArbiterTest_1(input clk, input reset,
    output io_in_0_ready,
    input io_in_0_valid,
    input [7:0] io_in_0_bits,
    output io_in_1_ready,
    input io_in_1_valid,
    input [7:0] io_in_1_bits,
    output io_in_2_ready,
    input io_in_2_valid,
    input [7:0] io_in_2_bits,
    output io_in_3_ready,
    input io_in_3_valid,
    input [7:0] io_in_3_bits,
    input io_out_ready,
    output io_out_valid,
    output [7:0] io_out_bits,
    output [1:0] io_chosen
);

  wire T0;
  wire T1;
  wire T2;
  wire T3;
  reg [1:0] R4;
  wire [1:0] T5;
  wire T6;
  wire T7;
  wire T8;
  wire [1:0] T9;
  wire [1:0] T10;
  wire T11;
  wire T12;
  wire [1:0] T13;
  wire T14;
  wire T15;
  wire [1:0] T16;
  wire T17;
  wire T18;
  wire [1:0] T19;
  wire [1:0] T20;
  wire [1:0] T21;
  wire T22;
  wire T23;
  wire T24;
  wire T25;
  wire T26;
  wire T27;
  wire T28;
  wire T29;
  wire T30;
  wire T31;
  wire T32;
  wire T33;
  wire T34;
  wire T35;
  wire T36;
  wire T37;
  wire T38;
  wire T39;
  wire T40;
  wire T41;
  wire T42;
  wire T43;
  wire T44;
  wire T45;
  wire T46;
  wire T47;
  wire T48;
  wire T49;
  wire T50;
  wire T51;
  wire T52;
  wire T53;
  wire T54;
  wire T55;
  wire T56;
  wire T57;
  wire T58;
  wire T59;
  wire T60;
  wire T61;
  wire T62;
  wire T63;
  wire T64;
  wire T65;
  wire T66;
  wire T67;
  wire T68;
  wire T69;
  wire T70;
  wire T71;
  wire T72;
  wire T73;
  wire T74;
  wire T75;
  wire T76;
  wire [7:0] T77;
  wire T78;
  wire [7:0] T79;
  wire T80;
  wire [7:0] T81;
  wire T82;

  assign io_in_0_ready = T0;
  assign T0 = T1 && io_out_ready;
  assign T1 = T2;
  assign T2 = T3 || T26;
  assign T3 = 2'h0/* 0*/ > R4;
  assign T5 = T6 ? T9 : R4;
  assign T6 = io_out_ready && io_out_valid;
  assign io_out_valid = T7;
  assign T7 = T8 ? T22 : T24;
  assign T8 = T9[1'h1/* 1*/:1'h1/* 1*/];
  assign T9 = T10;
  assign T10 = T11 ? 1'h1/* 1*/ : T13;
  assign T11 = io_in_1_valid && T12;
  assign T12 = 2'h1/* 1*/ > R4;
  assign T13 = T14 ? 2'h2/* 2*/ : T16;
  assign T14 = io_in_2_valid && T15;
  assign T15 = 2'h2/* 2*/ > R4;
  assign T16 = T17 ? 2'h3/* 3*/ : T19;
  assign T17 = io_in_3_valid && T18;
  assign T18 = 2'h3/* 3*/ > R4;
  assign T19 = io_in_0_valid ? 1'h0/* 0*/ : T20;
  assign T20 = io_in_1_valid ? 1'h1/* 1*/ : T21;
  assign T21 = io_in_2_valid ? 2'h2/* 2*/ : 2'h3/* 3*/;
  assign T22 = T23 ? io_in_3_valid : io_in_2_valid;
  assign T23 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign T24 = T25 ? io_in_1_valid : io_in_0_valid;
  assign T25 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign T26 = ! T27;
  assign T27 = T28 || T36;
  assign T28 = T29 || T34;
  assign T29 = T30 || T32;
  assign T30 = io_in_0_valid && T31;
  assign T31 = 2'h0/* 0*/ > R4;
  assign T32 = io_in_1_valid && T33;
  assign T33 = 2'h1/* 1*/ > R4;
  assign T34 = io_in_2_valid && T35;
  assign T35 = 2'h2/* 2*/ > R4;
  assign T36 = io_in_3_valid && T37;
  assign T37 = 2'h3/* 3*/ > R4;
  assign io_in_1_ready = T38;
  assign T38 = T39 && io_out_ready;
  assign T39 = T40;
  assign T40 = T41 || T44;
  assign T41 = T42 && T43;
  assign T42 = ! T30;
  assign T43 = 2'h1/* 1*/ > R4;
  assign T44 = ! T45;
  assign T45 = T46 || io_in_0_valid;
  assign T46 = T47 || T36;
  assign T47 = T48 || T34;
  assign T48 = T30 || T32;
  assign io_in_2_ready = T49;
  assign T49 = T50 && io_out_ready;
  assign T50 = T51;
  assign T51 = T52 || T56;
  assign T52 = T53 && T55;
  assign T53 = ! T54;
  assign T54 = T30 || T32;
  assign T55 = 2'h2/* 2*/ > R4;
  assign T56 = ! T57;
  assign T57 = T58 || io_in_1_valid;
  assign T58 = T59 || io_in_0_valid;
  assign T59 = T60 || T36;
  assign T60 = T61 || T34;
  assign T61 = T30 || T32;
  assign io_in_3_ready = T62;
  assign T62 = T63 && io_out_ready;
  assign T63 = T64;
  assign T64 = T65 || T70;
  assign T65 = T66 && T69;
  assign T66 = ! T67;
  assign T67 = T68 || T34;
  assign T68 = T30 || T32;
  assign T69 = 2'h3/* 3*/ > R4;
  assign T70 = ! T71;
  assign T71 = T72 || io_in_2_valid;
  assign T72 = T73 || io_in_1_valid;
  assign T73 = T74 || io_in_0_valid;
  assign T74 = T75 || T36;
  assign T75 = T76 || T34;
  assign T76 = T30 || T32;
  assign io_out_bits = T77;
  assign T77 = T78 ? T79 : T81;
  assign T78 = T9[1'h1/* 1*/:1'h1/* 1*/];
  assign T79 = T80 ? io_in_3_bits : io_in_2_bits;
  assign T80 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign T81 = T82 ? io_in_1_bits : io_in_0_bits;
  assign T82 = T9[1'h0/* 0*/:1'h0/* 0*/];
  assign io_chosen = T9;

  always @(posedge clk) begin
    if(reset) begin
      R4 <= 2'h0/* 0*/;
    end else if(T6) begin
      R4 <= T5;
    end
  end
endmodule

""")
  }

  /** Generate a FillInterleaved
    */
  @Test def testFillInterleaved() {

    class FillInterleavedComp extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := FillInterleaved(4, io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new FillInterleavedComp()))
  }

  /** Generate a Counter
    */
  @Test def testCounter() {
    println("\ntestCounter ...")
    class CounterComp extends Module {
      val io = new Bundle {
        val in = Bool(INPUT)
        val out = UInt(OUTPUT)
        val wrap = Bool(OUTPUT)
      }
      val (count, wrap) = Counter(io.in, 5)
      io.out := count
      io.wrap := wrap
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new CounterComp()))
  }

  /* XXX. */
  @Test def testOHToUInt() {
    println("\ntestOHToUInt ...")
    class OHToUIntComp extends Module {
      val io = new Bundle {
        val in = Bool(INPUT)
        val out = UInt(OUTPUT)
      }
      io.out := OHToUInt(Bool(true) :: io.in :: Bool(false) :: io.in :: Nil)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new OHToUIntComp()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_OHToUIntComp_1.v",
"""module StdlibSuite_OHToUIntComp_1(
    input io_in,
    output [1:0] io_out
);

  wire [1:0] T0;
  wire T1;

  assign io_out = T0;
  assign T0 = {io_in, T1};
  assign T1 = io_in || io_in;
endmodule

""")
  }

  /* Delays an element by a fixed latency. */
  @Test def testPipe() {
    println("\ntestPipe ...")
    class PipeComp extends Pipe(UInt(width=8), 2) {
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new PipeComp()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_PipeComp_1.v",
"""module StdlibSuite_PipeComp_1(input clk, input reset,
    input io_enq_valid,
    input [7:0] io_enq_bits,
    output io_deq_valid,
    output [7:0] io_deq_bits
);

  wire T0;
  reg R1;
  reg R2;
  wire [7:0] T3;
  reg [7:0] R4;
  wire [7:0] T5;
  reg [7:0] R6;
  wire [7:0] T7;

  assign io_deq_valid = T0;
  assign T0 = R1;
  assign io_deq_bits = T3;
  assign T3 = R4;
  assign T5 = R2 ? R6 : R4;
  assign T7 = io_enq_valid ? io_enq_bits : R6;

  always @(posedge clk) begin
    R1 <= reset ? 1'h0/* 0*/ : R2;
    R2 <= reset ? 1'h0/* 0*/ : io_enq_valid;
    R4 <= T5;
    R6 <= T7;
  end
endmodule

""")
  }

  /** Generate a PriorityMux
    */
  @Test def testPriorityMux() {
    println("\ntestPriorityMux ...")
    class PriorityMuxComp extends Module {
      val io = new Bundle {
        val in0 = Bool(INPUT)
        val in1 = Bool(INPUT)
        val data0 = UInt(INPUT, 16)
        val data1 = UInt(INPUT, 16)
        val out0 = UInt(OUTPUT)
        val out1 = UInt(OUTPUT)
        val out2 = UInt(OUTPUT)
      }
      io.out0 := PriorityMux((io.in0, io.data0) :: (io.in1, io.data1) :: Nil)
      io.out1 := PriorityMux(io.in0 :: io.in1 :: Nil,
        io.data0 :: io.data1 :: Nil)
      io.out2 := PriorityMux(io.in0.toBits, io.data0 :: io.data1 :: Nil)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new PriorityMuxComp()))
  }

  /** Generate a PriorityEncoder
    */
  @Test def testPriorityEncoder() {
    println("\ntestPriorityEncoder ...")
    class PriorityEncoderComp extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := PriorityEncoder(io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new PriorityEncoderComp()))
  }

  /** Generate a PriorityEncoderOH
    */
  @Test def testPriorityEncoderOH() {
    println("\ntestPriorityEncoderOH ...")
    class PriorityEncoderOHComp extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := PriorityEncoderOH(io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new PriorityEncoderOHComp()))
  }

  /* Implements a queue of elements (first-in, first-out). */
  @Test def testQueue() {
    println("\ntestQueue ...")
    class QueueComp extends Module {
      val io = new Bundle {
        val req = Decoupled(UInt(width=8)).flip
        val resp = Decoupled(UInt(width=8))
      }
      io.resp <> Queue(io.req)
    }

    chiselMain(Array[String]("--inlineMem", "--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new QueueComp()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_QueueComp_1.v",
"""module Queue(input clk, input reset,
    output io_enq_ready,
    input io_enq_valid,
    input [7:0] io_enq_bits,
    input io_deq_ready,
    output io_deq_valid,
    output [7:0] io_deq_bits
);

  wire T0;
  wire full;
  wire ptr_match;
  reg enq_ptr;
  wire T1;
  wire do_enq;
  wire T2;
  wire T3;
  wire do_flow;
  wire T4;
  reg deq_ptr;
  wire T5;
  wire do_deq;
  wire T6;
  wire T7;
  wire empty;
  wire T8;
  reg maybe_full;
  wire T9;
  wire T10;
  wire T11;
  wire T12;
  wire [7:0] T13;
  reg [7:0] ram [1:0];
  wire [7:0] T14;
  wire [7:0] T15;

  assign io_enq_ready = T0;
  assign T0 = ! full;
  assign full = ptr_match && maybe_full;
  assign ptr_match = enq_ptr == deq_ptr;
  assign T1 = do_enq ? T4 : enq_ptr;
  assign do_enq = T2 && T3;
  assign T2 = io_enq_ready && io_enq_valid;
  assign T3 = ! do_flow;
  assign do_flow = 1'h0/* 0*/;
  assign T4 = enq_ptr + 1'h1/* 1*/;
  assign T5 = do_deq ? T12 : deq_ptr;
  assign do_deq = T6 && T11;
  assign T6 = io_deq_ready && io_deq_valid;
  assign io_deq_valid = T7;
  assign T7 = ! empty;
  assign empty = ptr_match && T8;
  assign T8 = ! maybe_full;
  assign T9 = T10 ? do_enq : maybe_full;
  assign T10 = do_enq != do_deq;
  assign T11 = ! do_flow;
  assign T12 = deq_ptr + 1'h1/* 1*/;
  assign io_deq_bits = T13;
  assign T15 = io_enq_bits;

  always @(posedge clk) begin
    if(reset) begin
      enq_ptr <= 1'h0/* 0*/;
    end else if(do_enq) begin
      enq_ptr <= T1;
    end
    if(reset) begin
      deq_ptr <= 1'h0/* 0*/;
    end else if(do_deq) begin
      deq_ptr <= T5;
    end
    if(reset) begin
      maybe_full <= 1'h0/* 0*/;
    end else if(T10) begin
      maybe_full <= T9;
    end
    if (do_enq)
      ram[enq_ptr] <= T15;
  end
endmodule

module StdlibSuite_QueueComp_1(input clk, input reset,
    output io_req_ready,
    input io_req_valid,
    input [7:0] io_req_bits,
    input io_resp_ready,
    output io_resp_valid,
    output [7:0] io_resp_bits
);

  wire Queue_io_enq_ready;
  wire Queue_io_deq_valid;
  wire [7:0] Queue_io_deq_bits;

  assign io_req_ready = Queue_io_enq_ready;
  assign io_resp_valid = Queue_io_deq_valid;
  assign io_resp_bits = Queue_io_deq_bits;
  Queue Queue(.clk(clk), .reset(reset),
       .io_enq_ready( Queue_io_enq_ready ),
       .io_enq_valid( io_req_valid ),
       .io_enq_bits( io_req_bits ),
       .io_deq_ready( io_resp_ready ),
       .io_deq_valid( Queue_io_deq_valid ),
       .io_deq_bits( Queue_io_deq_bits )
  );
endmodule

""")
  }

  /** Generate a Fill
    */
  @Test def testFill() {
    println("\ntestFill ...")
    class FillComp extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := Fill(4, io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new FillComp()))
  }

  /** Generate a Log2
    */
  @Test def testLog2() {
    println("\ntestLog2 ...")
    class Log2Comp extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := Log2(io.in, 2)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new Log2Comp()))
  }

  /** Generate a MuxLookup
    */
  @Test def testMuxLookup() {
    println("\ntestMuxLookup ...")
    class MuxLookupComp extends Module {
      val io = new Bundle {
        val key = UInt(INPUT, 8)
        val in0 = UInt(INPUT, 8)
        val in1 = UInt(INPUT, 8)
        val default = UInt(INPUT, 16)
        val data0 = UInt(INPUT, 16)
        val data1 = UInt(INPUT, 16)
        val out = UInt(OUTPUT)
      }
      io.out := MuxLookup(io.key, io.default,
        (io.in0, io.data0) :: (io.in1, io.data1) :: Nil)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new MuxLookupComp()))
  }

  /** Generate a MuxCase
    */
  @Test def testMuxCase() {
    println("\ntestMuxCase ...")
    class MuxCaseComp extends Module {
      val io = new Bundle {
        val default = UInt(INPUT, 8)
        val in0 = UInt(INPUT, 8)
        val in1 = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := MuxCase(io.default,
        (Bool(true), io.in0) :: (Bool(false), io.in1) :: Nil)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new MuxCaseComp()))
  }

  /** Generate a Multiplex
    */
  @Test def testMultiplex() {

    class MultiplexComp extends Module {
      val io = new Bundle {
        val t = UInt(INPUT, 1)
        val c = UInt(INPUT, 8)
        val a = UInt(INPUT, 8)
        val out = UInt(OUTPUT, 8)
      }
      // XXX Cannot figure out which code to write. Multiplex returns a Node.
      // val x = Multiplex(io.t, io.c, io.a)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new MultiplexComp()))
  }

  /** Generate a Mux
    */
  @Test def testMux() {
    println("\ntestMux ...")
    class MuxComp extends Module {
      val io = new Bundle {
        val t = Bool(INPUT)
        val c = UInt(INPUT, 8)
        val a = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := Mux(io.t, io.c, io.a)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new MuxComp()))
  }
}
