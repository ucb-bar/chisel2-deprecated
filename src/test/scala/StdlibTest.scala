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

    class OperatorComp extends Component {
      val io = new Bundle {
        val x = UFix(INPUT, 8)
        val y = UFix(INPUT, 8)
        val ys = Fix(INPUT, 8)
        val z = UFix(OUTPUT)
        val zb = Bool(OUTPUT)
      }

      // apply(bit: Int): Bool
      val a = io.x(0).toBits

      // apply(hi: Int, lo: Int): UFix
      val b = io.x(4, 3)
      val c = io.x(3, 4)
      val d = io.x(3, 3)
      val e = io.x(9, -1)

      // apply(bit: UFix): Bool
      val a1 = io.x(UFix(0)).toBits

      // apply(hi: UFix, lo: UFix): UFix
      val b1 = io.x(UFix(4), UFix(3))
      val c1 = io.x(UFix(3), UFix(4))
      val d1 = io.x(UFix(3), UFix(3))
      val e1 = io.x(UFix(9), UFix(-1))

      // apply(range: (Int, Int)): UFix
      val f = io.x((5, 3))

      // unary_-(): UFix
      val g = - io.x

      // unary_~(): UFix
      val h = ~io.x

      // andR(): Bool
      val i = io.x.andR.toBits

      // orR():  Bool
      val j = io.x.orR.toBits

      // xorR():  Bool
      val k = io.x.xorR.toBits

      // << (b: UFix): UFix
      val l = io.x << a

      // >> (b: UFix): UFix
      val m = io.x >> a

      // +  (b: UFix): UFix
      val n = io.x + io.y

      // *  (b: UFix): UFix
      val o = io.x * io.y

      // ^  (b: UFix): UFix
      val r = io.x ^ io.y

      // ?  (b: UFix): UFix
      val s = io.x ? io.y

      // -  (b: UFix): UFix
      val t = io.x - io.y

      // ## (b: UFix): UFix
      val u = io.x ## io.y

      // &  (b: UFix): UFix
      val ab = io.x & io.y

      // |  (b: UFix): UFix
      val ac = io.x | io.y

      io.z := (a | b | c | d
        | a1 | b1 | c1 | d1
        | f | g | h | i | j | k
        | l | m | n | o
        | r | u | ab | ac
        /* XXX Computing any of those signals throws an exception */
        /* | e | t | e1 | s */
      ).toBits

      // -- result type is Bool --

      // ===(b: UFix): Bool
      val v = io.x === io.y

      // != (b: UFix): Bool
      val w = io.x != io.y

      // >  (b: UFix): Bool
      val x = io.x > io.y

      // <  (b: UFix): Bool
      val y = io.x < io.y

      // <= (b: UFix): Bool
      val z = io.x <= io.y

      // >= (b: UFix): Bool
      val aa = io.x >= io.y

      io.zb := (v | w | x | y | z | aa)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new OperatorComp()))
  }

  /** Multiply an unsigned number by signed number */
  @Test def testMulUS() {
    class MulUS extends Component {
      val io = new Bundle {
        val x = UFix(INPUT, 32)
        val y = Fix(INPUT, 32)
        val z = Fix(OUTPUT)
      }
      io.z := io.x * io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new MulUS()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_MulUS_1.v",
"""module StdlibSuite_MulUS_1(
    input [31:0] io_x,
    input  signed [31:0] io_y,
    output signed [63:0] io_z);

  wire signed [63:0] T0;
  wire signed [32:0] T1;
  wire[32:0] T2;

  assign io_z = T0;
  assign T0 = $signed(io_y) * $signed(T1);
  assign T1 = T2;
  assign T2 = {1'h0/* 0*/, io_x};
endmodule

""")
  }

  /** Divide an unsigned number by signed number */
  @Test def testDivUS() {
    class DivUS extends Component {
      val io = new Bundle {
        val x = UFix(INPUT, 32)
        val y = Fix(INPUT, 32)
        val z = Fix(OUTPUT)
      }
      io.z := io.x / io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new DivUS()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_DivUS_1.v",
"""module StdlibSuite_DivUS_1(
    input [31:0] io_x,
    input  signed [31:0] io_y,
    output signed [31:0] io_z);

  wire signed [31:0] T0;
  wire signed [32:0] T1;
  wire[32:0] T2;

  assign io_z = T0;
  assign T0 = $signed(T1) / $signed(io_y);
  assign T1 = T2;
  assign T2 = {1'h0/* 0*/, io_x};
endmodule

""")
  }

  /** Remainer of an unsigned number by signed number */
  @Test def testRemUS() {
    class RemUS extends Component {
      val io = new Bundle {
        val x = UFix(INPUT, 32)
        val y = Fix(INPUT, 32)
        val z = Fix(OUTPUT)
      }
      io.z := io.x % io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new RemUS()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_RemUS_1.v",
"""module StdlibSuite_RemUS_1(
    input [31:0] io_x,
    input  signed [31:0] io_y,
    output signed [31:0] io_z);

  wire signed [31:0] T0;
  wire signed [32:0] T1;
  wire[32:0] T2;

  assign io_z = T0;
  assign T0 = $signed(T1) u%s $signed(io_y);
  assign T1 = T2;
  assign T2 = {1'h0/* 0*/, io_x};
endmodule

""")
  }

  /** Multiply an signed number by an unsigned number */
  @Test def testMulSU() {
    class MulSU extends Component {
      val io = new Bundle {
        val x = Fix(INPUT, 32)
        val y = UFix(INPUT, 32)
        val z = Fix(OUTPUT)
      }
      io.z := io.x * io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new MulSU()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_MulSU_1.v",
"""module StdlibSuite_MulSU_1(
    input  signed [31:0] io_x,
    input [31:0] io_y,
    output signed [63:0] io_z);

  wire signed [63:0] T0;
  wire signed [32:0] T1;
  wire[32:0] T2;

  assign io_z = T0;
  assign T0 = $signed(io_x) * $signed(T1);
  assign T1 = T2;
  assign T2 = {1'h0/* 0*/, io_y};
endmodule

""")
  }

  /** Divide a signed number by an unsigned number */
  @Test def testDivSU() {
    class DivSU extends Component {
      val io = new Bundle {
        val x = Fix(INPUT, 32)
        val y = UFix(INPUT, 32)
        val z = Fix(OUTPUT)
      }
      io.z := io.x / io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new DivSU()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_DivSU_1.v",
"""module StdlibSuite_DivSU_1(
    input  signed [31:0] io_x,
    input [31:0] io_y,
    output signed [31:0] io_z);

  wire signed [31:0] T0;
  wire signed [32:0] T1;
  wire[32:0] T2;

  assign io_z = T0;
  assign T0 = $signed(io_x) / $signed(T1);
  assign T1 = T2;
  assign T2 = {1'h0/* 0*/, io_y};
endmodule

""")
  }

  /** Remainer of a signed number by an unsigned number */
  @Test def testRemSU() {
    class RemSU extends Component {
      val io = new Bundle {
        val x = Fix(INPUT, 32)
        val y = UFix(INPUT, 32)
        val z = Fix(OUTPUT)
      }
      io.z := io.x % io.y
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new RemSU()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_RemSU_1.v",
"""module StdlibSuite_RemSU_1(
    input  signed [31:0] io_x,
    input [31:0] io_y,
    output signed [31:0] io_z);

  wire signed [31:0] T0;
  wire signed [32:0] T1;
  wire[32:0] T2;

  assign io_z = T0;
  assign T0 = $signed(io_x) s%u $signed(T1);
  assign T1 = T2;
  assign T2 = {1'h0/* 0*/, io_y};
endmodule

""")
  }

  /** Assign a Bundle */
  @Test def assignBundle() {
    class AssignBundle extends Bundle {
        val v = Vec.fill(2){UFix(INPUT, 2)}
    }
    class AssignBundleComp extends Component {
      val io = new Bundle {
        val in = new AssignBundle()
        val out = new AssignBundle().flip
      }

      io.out := io.in
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new AssignBundleComp()))
    assertFile(tmpdir.getRoot() + "/StdlibSuite_AssignBundleComp_1.v",
"""module StdlibSuite_AssignBundleComp_1(
    input [1:0] io_in_v_0,
    input [1:0] io_in_v_1,
    output[1:0] io_out_v_0,
    output[1:0] io_out_v_1);


  assign io_out_v_1 = io_in_v_1;
  assign io_out_v_0 = io_in_v_0;
endmodule

""")
  }

  /** Concatenate two nodes X and Y in a node Z such that
    Z[0..wx+wy] = X[0..wx] :: Y[0..wy]. */
  @Test def testCat() {

    class CatComp extends Component {
      val io = new Bundle {
        val x = UFix(INPUT, 8)
        val y = UFix(INPUT, 8)
        val z = UFix(OUTPUT)
      }
      io.z := Cat(io.x, io.y)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new CatComp()))
  }

  /** Generate a lookup into an array.
    XXX Lookup.scala, use different code based on instance of CppBackend. */
  @Test def testLookup() {

    class LookupComp extends Component {
      val io = new Bundle {
        val addr = Bits(INPUT, 8)
        val data = UFix(OUTPUT)
      }
      io.data := Lookup(io.addr, UFix(0), Array(
        (Bits(0), UFix(10)),
        (Bits(1), UFix(11))))
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new LookupComp()))
  }

  /** Generate a PopCount
    */
  @Test def testPopCount() {

    class PopCountComp extends Component {
      val io = new Bundle {
        val in = Bits(INPUT, 8)
        val out = UFix(OUTPUT)
      }
      io.out := PopCount(Array(Bool(true), Bool(false)))
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new PopCountComp()))
  }

  /** Generate a Reverse
    */
  @Test def testReverse() {

    class ReverseComp extends Component {
      val io = new Bundle {
        val in = Bits(INPUT, 8)
        val out = Bits(OUTPUT)
      }
      io.out := Reverse(io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new ReverseComp()))
  }

  /** Generate a ShiftRegister
    */
  @Test def testShiftRegister() {

    class ShiftRegisterComp extends Component {
      val io = new Bundle {
        val in = Bits(INPUT, 8)
        val out = Bits(OUTPUT)
      }
      io.out := ShiftRegister(2, io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new ShiftRegisterComp()))
  }

  /** Generate a UFixToOH
    */
  @Test def testUFixToOH() {

    class UFixToOHComp extends Component {
      val io = new Bundle {
        val in = Bits(INPUT, 8)
        val out0 = UFix(OUTPUT)
        val out1 = UFix(OUTPUT)
      }
      io.out0 := UFixToOH(io.in)
      io.out1 := UFixToOH(io.in, 4)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new UFixToOHComp()))
  }

  /** Generate a foldR
    */
  @Test def testfoldR() {

    class foldRComp extends Component {
      val io = new Bundle {
        val in0 = UFix(INPUT, 8)
        val in1 = UFix(INPUT, 8)
        val out = UFix(OUTPUT)
      }
      io.out := foldR(io.in0 :: io.in1 :: Nil){ _ + _ }
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new foldRComp()))
  }

  /** Generate a ArbiterCtrl
    */
  @Test def testArbiterCtrl() {

    class ArbiterCtrlComp extends Component {
      val io = new Bundle {
        val in0 = Bool(INPUT)
        val in1 = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
      val x = ArbiterCtrl(io.in0 :: io.in1 :: Nil)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new ArbiterCtrlComp()))
  }

  /** Generate a FillInterleaved
    */
  @Test def testFillInterleaved() {

    class FillInterleavedComp extends Component {
      val io = new Bundle {
        val in = Bits(INPUT, 8)
        val out = Bits(OUTPUT)
      }
      io.out := FillInterleaved(4, io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new FillInterleavedComp()))
  }

  /** Generate a Counter
    */
  @Test def testCounter() {

    class CounterComp extends Component {
      val io = new Bundle {
        val in = Bool(INPUT)
        val out = UFix(OUTPUT)
        val wrap = Bool(OUTPUT)
      }
      val (count, wrap) = Counter(io.in, 5)
      io.out := count
      io.wrap := wrap
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new CounterComp()))
  }

  /** Generate a PriorityMux
    */
  @Test def testPriorityMux() {

    class PriorityMuxComp extends Component {
      val io = new Bundle {
        val in0 = Bool(INPUT)
        val in1 = Bool(INPUT)
        val data0 = Bits(INPUT, 16)
        val data1 = Bits(INPUT, 16)
        val out0 = Bits(OUTPUT)
        val out1 = Bits(OUTPUT)
        val out2 = Bits(OUTPUT)
      }
      io.out0 := PriorityMux((io.in0, io.data0) :: (io.in1, io.data1) :: Nil)
      io.out1 := PriorityMux(io.in0 :: io.in1 :: Nil,
        io.data0 :: io.data1 :: Nil)
      io.out2 := PriorityMux(io.in0.toBits, io.data0 :: io.data1 :: Nil)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new PriorityMuxComp()))
  }

  /** Generate a PriorityEncoder
    */
  @Test def testPriorityEncoder() {

    class PriorityEncoderComp extends Component {
      val io = new Bundle {
        val in = Bits(INPUT, 8)
        val out = UFix(OUTPUT)
      }
      io.out := PriorityEncoder(io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new PriorityEncoderComp()))
  }

  /** Generate a PriorityEncoderOH
    */
  @Test def testPriorityEncoderOH() {

    class PriorityEncoderOHComp extends Component {
      val io = new Bundle {
        val in = Bits(INPUT, 8)
        val out = Bits(OUTPUT)
      }
      io.out := PriorityEncoderOH(io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new PriorityEncoderOHComp()))
  }

  /** Generate a Fill
    */
  @Test def testFill() {

    class FillComp extends Component {
      val io = new Bundle {
        val in = Bits(INPUT, 8)
        val out = Bits(OUTPUT)
      }
      io.out := Fill(4, io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new FillComp()))
  }

  /** Generate a Log2
    */
  @Test def testLog2() {

    class Log2Comp extends Component {
      val io = new Bundle {
        val in = UFix(INPUT, 8)
        val out = UFix(OUTPUT)
      }
      io.out := Log2(io.in, 2)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new Log2Comp()))
  }

  /** Generate a MuxLookup
    */
  @Test def testMuxLookup() {

    class MuxLookupComp extends Component {
      val io = new Bundle {
        val key = Bits(INPUT, 8)
        val in0 = Bits(INPUT, 8)
        val in1 = Bits(INPUT, 8)
        val default = Bits(INPUT, 16)
        val data0 = Bits(INPUT, 16)
        val data1 = Bits(INPUT, 16)
        val out = Bits(OUTPUT)
      }
      io.out := MuxLookup(io.key, io.default,
        (io.in0, io.data0) :: (io.in1, io.data1) :: Nil)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new MuxLookupComp()))
  }

  /** Generate a MuxCase
    */
  @Test def testMuxCase() {

    class MuxCaseComp extends Component {
      val io = new Bundle {
        val default = Bits(INPUT, 8)
        val in0 = Bits(INPUT, 8)
        val in1 = Bits(INPUT, 8)
        val out = Bits(OUTPUT)
      }
      io.out := MuxCase(io.default,
        (Bool(true), io.in0) :: (Bool(false), io.in1) :: Nil)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new MuxCaseComp()))
  }

  /** Generate a Multiplex
    */
  @Test def testMultiplex() {

    class MultiplexComp extends Component {
      val io = new Bundle {
        val t = Bits(INPUT, 1)
        val c = Bits(INPUT, 8)
        val a = Bits(INPUT, 8)
        val out = Bits(OUTPUT, 8)
      }
      // XXX Cannot figure out which code to write. Multiplex returns a Node.
      // val x = Multiplex(io.t, io.c, io.a)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new MultiplexComp()))
  }

  /** Generate a Mux
    */
  @Test def testMux() {

    class MuxComp extends Component {
      val io = new Bundle {
        val t = Bool(INPUT)
        val c = Bits(INPUT, 8)
        val a = Bits(INPUT, 8)
        val out = Bits(OUTPUT)
      }
      io.out := Mux(io.t, io.c, io.a)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new MuxComp()))
  }

}
