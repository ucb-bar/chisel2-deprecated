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

import org.scalatest._
import org.junit.Assert._
import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import org.junit.Ignore

import Chisel._

object ZeroWidthTest{
  val W0WparameterName = "W0W"

  @BeforeClass def checkSupport() {
    val W0Wparameter = System.getProperty(W0WparameterName)
    val W0Wenable = if (W0Wparameter == null) false else true
    org.junit.Assume.assumeTrue(W0Wenable)
  }
  
}
/** This testsuite checks operations using 0-width wires.
*/
class ZeroWidthTest extends TestSuite {
  val testArgs = Array("--W0W",
      "--backend", "dot",
      "--targetDir", dir.getPath.toString()
      )

  @Before override def initialize() {
    super.initialize()
    // Enable W0W support
    Driver.isSupportW0W = true
  }

  /** Generate a 0-width wire (explicitly) */
  @Test def testImplicit0Width() {
    val res = UInt(0,0)
    assertTrue( res.getWidth == 0 )
    assertTrue( res.litOf.value == 0 )
  }

  /** Generate a 0-width wire from a '>>' */
  @Test def testRSH0Width() {
    val res = UInt(3,2) >> UInt(2)
    assertTrue( res.getWidth == 0 )
    assertTrue( res.litOf.value == 0 )
  }

  /** Generate a 0-width wire from an extraction. */
  @Test def testExtract0Width() {
    val res = UInt(3)((0, 1))
    assertTrue( res.getWidth == 0 )
    assertTrue( res.litOf.value == 0 )
  }

  @Test def testOperators() {
  try {
    class OperatorComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 0)  /* explicitly set x to 0-width */
        val y = UInt(INPUT, 8)
        val ys = SInt(INPUT, 8)
        val z = UInt(OUTPUT)
        val zb = Bool(OUTPUT)
      }

      // apply(bit: Int): Bool
      // val a = io.x(0).toBits /* This should generate an extraction error. */

      // apply(hi: Int, lo: Int): UInt
      val c = io.x(3, 4) /* This should NOT throw an Assertion failure or error. */

      // apply(bit: UInt): Bool
      // val a1 = io.x(UInt(0)).toBits

      // apply(hi: UInt, lo: UInt): UInt
      val c1 = io.x(UInt(3), UInt(4)) /* This should NOT throw an Assertion failure or error. */

      // apply(range: (Int, Int)): UInt
      val f = io.x((3, 4))

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
      val l = io.x << c

      // >> (b: UInt): UInt
      val m = io.x >> c

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

      io.z := (/* a | a1
        |*/ f | g | h | i | j | k
        | l | m | n | o
        | r | u | ab | ac
        /* XXX Computing any of those signals throws an exception */
        | c | c1 | t 
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

    chiselMain(testArgs, () => Module(new OperatorComp()))
    } catch {
      case e: Throwable => e.printStackTrace()
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
    chiselMain(testArgs, () => Module(new MulUS()))
    assertFile("StdlibSuite_MulUS_1.v")
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
    chiselMain(testArgs, () => Module(new DivUS()))
    assertFile("StdlibSuite_DivUS_1.v")
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
    chiselMain(testArgs, () => Module(new RemUS()))
    assertFile("StdlibSuite_RemUS_1.v")
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
    chiselMain(testArgs, () => Module(new MulSU()))
    assertFile("StdlibSuite_MulSU_1.v")
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
    chiselMain(testArgs, () => Module(new DivSU()))
    assertFile("StdlibSuite_DivSU_1.v")
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
    chiselMain(testArgs, () => Module(new RemSU()))
    assertFile("StdlibSuite_RemSU_1.v")
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
    chiselMain(testArgs, () => Module(new AssignBundleComp()))
    assertFile("StdlibSuite_AssignBundleComp_1.v")
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

    chiselMain(testArgs, () => Module(new CatComp()))
    assertFile("StdlibSuite_CatComp_1.v")
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

    chiselMain(testArgs, () => Module(new LookupComp()))
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

    chiselMain(testArgs, () => Module(new PopCountComp()))
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

    chiselMain(testArgs, () => Module(new ReverseComp()))
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

    chiselMain(testArgs, () => Module(new ShiftRegisterComp()))
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

    chiselMain(testArgs, () => Module(new UIntToOHComp()))
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

    chiselMain(testArgs, () => Module(new foldRComp()))
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
      def gen = SInt(width=8)
      val n = 4
      class ArbiterTest extends Module {
        val io = new ArbiterIO(gen, n) {
          val fire = Bool(OUTPUT)
        }
        val arb = Module(new Arbiter(gen, n))
        io <> arb.io
        io.fire := arb.io.out.fire()
      }

      chiselMain(testArgs, () => Module(new ArbiterTest()))
    } catch {
      case e: Throwable => e.printStackTrace()
    }
    assertFile("StdlibSuite_ArbiterTest_1.v")
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

    chiselMain(testArgs, () => Module(new ArbiterNeedsLock()))
    assertFile(dir.getPath + "/NeedsLock.v",
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

    chiselMain(testArgs, () => Module(new RRArbiterTest()))
    assertFile("StdlibSuite_RRArbiterTest_1.v")
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

    chiselMain(testArgs, () => Module(new FillInterleavedComp()))
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
    chiselMain(testArgs, () => Module(new CounterComp()))
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

    chiselMain(testArgs, () => Module(new OHToUIntComp()))
    assertFile("StdlibSuite_OHToUIntComp_1.v")
  }

  /* Delays an element by a fixed latency. */
  @Test def testPipe() {
    println("\ntestPipe ...")
    class PipeComp extends Pipe(UInt(width=8), 2) {
    }

    chiselMain(testArgs, () => Module(new PipeComp()))
    assertFile("StdlibSuite_PipeComp_1.v")
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

    chiselMain(testArgs, () => Module(new PriorityMuxComp()))
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

    chiselMain(testArgs, () => Module(new PriorityEncoderComp()))
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

    chiselMain(testArgs, () => Module(new PriorityEncoderOHComp()))
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

    chiselMain(testArgs, () => Module(new QueueComp()))
    assertFile("StdlibSuite_QueueComp_1.v")
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

    chiselMain(testArgs, () => Module(new FillComp()))
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

    chiselMain(testArgs, () => Module(new Log2Comp()))
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

    chiselMain(testArgs, () => Module(new MuxLookupComp()))
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

    chiselMain(testArgs, () => Module(new MuxCaseComp()))
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

    chiselMain(testArgs, () => Module(new MuxComp()))
  }
}
