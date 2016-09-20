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

import scala.collection.mutable.ArrayBuffer
import org.junit.Assert._
import org.junit.Test

import Chisel._

/** This testsuite checks the primitives of the standard library
  that will generate basic common graphs of *Node*.
*/
// Since we use magic numbers to test edge cases,
// inhibit the scalastyle warnings.
// scalastyle:off magic.number
// scalastyle:off number.of.methods
// scalastyle:off number.of.types
// scalastyle:off regex
// scalastyle:off method.length

class StdlibSuite extends TestSuite {
  val testArgs = Array("--backend", "v",
      "--targetDir", dir.getPath.toString()
      )


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

      // =/= (b: UInt): Bool
      val w = io.x =/= io.y

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

    chiselMain(testArgs,
      () => Module(new OperatorComp()))
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
    chiselMain(testArgs,
      () => Module(new MulUS()))
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
    chiselMain(testArgs,
      () => Module(new DivUS()))
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
    chiselMain(testArgs,
      () => Module(new RemUS()))
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
    chiselMain(testArgs,
      () => Module(new MulSU()))
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
    chiselMain(testArgs,
      () => Module(new DivSU()))
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
    chiselMain(testArgs,
      () => Module(new RemSU()))
    assertFile("StdlibSuite_RemSU_1.v")
  }

  /** Assign a Bundle */
  @Test def testAssignBundle() {
    println("\ntestAssignBundle ...")
    class AssignBundle extends Bundle {
        val v = Vec(2, UInt(INPUT, 2))
    }
    class AssignBundleComp extends Module {
      val io = new Bundle {
        val in = new AssignBundle()
        val out = new AssignBundle().flip
      }

      io.out := io.in
    }
    chiselMain(testArgs,
      () => Module(new AssignBundleComp()))
    assertFile("StdlibSuite_AssignBundleComp_1.v")
  }

  /** Concatenate two nodes X and Y in a node Z such that
    Z[0..wx + wy] = X[0..wx] :: Y[0..wy]. */
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

    chiselMain(testArgs,
      () => Module(new CatComp()))
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

    chiselMain(testArgs,
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

    chiselMain(testArgs,
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

    chiselMain(testArgs,
      () => Module(new ReverseComp()))
  }

  /** Generate various ShiftRegisters
    */
  @Test def testShiftRegister() {
    println("\ntestShiftRegister ...")
    case class ShiftRegisterComp(nSections: Int) extends Module {
      class ShiftRegisterSection(delays: Int) extends Module {
        val io = new Bundle {
          val in = UInt(INPUT, 8)
          val out = UInt(OUTPUT)
        }
        io.out := ShiftRegister(io.in, delays)
      }

      val io = new Bundle {
        val in = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
        val stages = new Array[UInt](nSections)
      }

      var lastin = io.in
      for (s <- 1 to nSections) {
        val f = Module(new ShiftRegisterSection(s - 1))
        f.name = "f" + s.toString
        f.io.in <> lastin
        lastin = f.io.out
        // Save this stage's output somewhere we can read it.
        io.stages(s - 1) = f.io.out
      }
      io.out <> lastin
    }

    class ShiftRegisterTester(m: ShiftRegisterComp) extends Tester(m) {
      // Calculate the total delay.
      // Each section introduces (sectionIndex) delays
      // where "sectionIndex" is the 0-origin section number.
      val totalDelays = (0 until m.nSections).sum
      for (d <- 0 to totalDelays) {
        val pokeVal = totalDelays - d
        poke(m.io.in, pokeVal)
        // Check each section's expected output.
        var accumulatedDelay = 0
        for (s <- 0 until m.nSections) {
          accumulatedDelay += s
          // The expected value is either the delayed poked value,
          // initial values are not necessarily 0
          if (d >= accumulatedDelay) {
            expect(m.io.stages(s), pokeVal + accumulatedDelay)
          }
        }
        step(1)
      }
    }

    val nSections = 3
    chiselMainTest(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
      () => Module(new ShiftRegisterComp(nSections))) {m => new ShiftRegisterTester(m)}
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

    chiselMain(testArgs,
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

    chiselMain(testArgs,
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

      chiselMain(testArgs,
        () => Module(new ArbiterTest()))
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

    chiselMain(testArgs,
      () => Module(new ArbiterNeedsLock()))
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

    chiselMain(testArgs,
      () => Module(new RRArbiterTest()))
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

    chiselMain(testArgs,
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
    chiselMain(testArgs,
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

    chiselMain(testArgs,
      () => Module(new OHToUIntComp()))
    assertFile("StdlibSuite_OHToUIntComp_1.v")
  }

  /* Delays an element by a fixed latency. */
  @Test def testPipe() {
    println("\ntestPipe ...")
    class PipeComp extends Pipe(UInt(width=8), 2) {
    }

    chiselMain(testArgs,
      () => Module(new PipeComp()))
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

    chiselMain(testArgs,
      () => Module(new PriorityMuxComp()))
  }

  /** PriorityMux can be used with Bundles.
    */
  @Test def testPriorityMuxWithBundles() {
    println("\ntestPriorityMuxWithBundles ...")
    class BundleStub extends Bundle {
      val x = Bool(INPUT)

      override def cloneType: this.type = new BundleStub().asInstanceOf[this.type]
    }
    class PriorityMuxComp extends Module {
      val io = new Bundle {
        val sel = UInt(INPUT, width = 2)
        val in0 = new BundleStub()
        val in1 = new BundleStub()
        val out = new BundleStub().flip()
      }
      io.out := PriorityMux(io.sel, io.in0 :: io.in1 :: Nil)
    }

    class PriorityMuxCompTester(m: PriorityMuxComp) extends Tester(m) {
      // Put the value 0 in the first bundle, and 1 in the second
      // bundle and verify that we get the expected output for sel =
      // 0b01, 0b10, and 0b11.

      poke(m.io.in0.x, 0)
      poke(m.io.in1.x, 1)

      poke(m.io.sel, 1)
      expect(m.io.out.x, 0)

      poke(m.io.sel, 2)
      expect(m.io.out.x, 1)

      poke(m.io.sel, 3)
      expect(m.io.out.x, 0)
    }

    chiselMainTest(
      Array[String](
        "--backend", "c",
        "--targetDir", dir.getPath.toString(),
        "--genHarness",
        "--compile",
        "--test"
      ),
      () => Module(new PriorityMuxComp())
    ) {m => new PriorityMuxCompTester(m)}
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

    chiselMain(testArgs,
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

    chiselMain(testArgs,
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

    chiselMain(testArgs,
      () => Module(new QueueComp()))
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

    chiselMain(testArgs,
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

    chiselMain(testArgs,
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

    chiselMain(testArgs,
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

    chiselMain(testArgs,
      () => Module(new MuxCaseComp()))
  }

  /** MuxCase can be used with Bundles.
    */
  @Test def testMuxCaseWithBundles() {
    println("\ntestMuxCaseWithBundles ...")
    class BundleStub extends Bundle {
      val x = Bool(INPUT)

      override def cloneType: this.type = new BundleStub().asInstanceOf[this.type]
    }

    class MuxCaseComp extends Module {
      val io = new Bundle {
        val sel0 = Bool(INPUT)
        val sel1 = Bool(INPUT)
        val in0 = new BundleStub()
        val in1 = new BundleStub()
        val out = new BundleStub().flip()
      }
      val default = new BundleStub()
      default.x := Bool(true)
      io.out := MuxCase(
        default,
        Array(
          io.sel0 -> io.in0,
          io.sel1 -> io.in1
        )
      )
    }

    class MuxCaseCompTester(m: MuxCaseComp) extends Tester(m) {
      // set the contents of the in0 and in1 bundles to 0 and 1
      // respectively and then verify the expected behaviour for all 4
      // combinations of sel0 and sel0.

      poke(m.io.in0.x, 0)
      poke(m.io.in1.x, 1)

      poke(m.io.sel0, 0)
      poke(m.io.sel1, 0)
      expect(m.io.out.x, 1)

      poke(m.io.sel0, 1)
      poke(m.io.sel1, 0)
      expect(m.io.out.x, 0)

      poke(m.io.sel0, 0)
      poke(m.io.sel1, 1)
      expect(m.io.out.x, 1)

      poke(m.io.sel0, 1)
      poke(m.io.sel1, 1)
      expect(m.io.out.x, 0)
    }

    chiselMainTest(
      Array[String](
        "--backend", "c",
        "--targetDir", dir.getPath.toString(),
        "--genHarness",
        "--compile",
        "--test"
      ),
      () => Module(new MuxCaseComp())
    ) {m => new MuxCaseCompTester(m)}
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

    chiselMain(testArgs,
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

    chiselMain(testArgs,
      () => Module(new MuxComp()))
  }

  /** Test Log2Ceil, Log2Floor
    */
  @Test def testLog2CeilFloor() {
    println("\ntestLog2CeilFloor ...")
    assertResult(1) { log2Ceil(2) }
    assertResult(0) { log2Ceil(1) }
    assertResult(1) { log2Floor(2) }
    assertResult(0) { log2Floor(1) }
  }

  /** Test width adjustment for Operations on literals.
   *
   */
  @Test def testLitAddSubDoesntWiden () {
    println("\ntestLitAddSubDoesntWiden ...")

    class LitAddSub extends Module {
      val io = new Bundle {
        val out1 = UInt(OUTPUT)
      }
      // The following should generate a warning since we'll assign the
      //  width of the result as the maximum of the input widths
      //  which will be too small for the computed result.
      val res1 = Reg(init = (UInt(7) + UInt(2)))
      debug(res1)
      io.out1 := res1
    }

    try {
      chiselMain(Array("--backend", "c",
        "--targetDir", dir.getPath.toString()),
        () => Module(new LitAddSub()))
    } catch {
      case e : java.lang.IllegalStateException => {}
    }
    assertTrue(ChiselError.hasErrors);
  }

  @Test def testLitAddSub () {
    println("\ntestLitAddSub ...")

    // If we had a "Tester" backend that allowed us to examine
    // the constructed graph, we wouldn't need this.
    class LitAddSub extends Module {
      val io = new Bundle {
        val out2 = SInt(OUTPUT)
        val out3 = UInt(OUTPUT, width=8)
      }
      val res2 = Reg(init = (SInt(2) - SInt(4)))
      val res3 = Reg(init = (UInt(2) - UInt(4)))
      // We'd like to just access the 'debug' nodes,
      // but we get warnings about connections and there
      // have been cases where unconnected debug node chains
      // don't have their type nodes removed or their widths
      // correctly inferred. These shouldn't happen, but ...
      debug(res2)
      debug(res3)
      io.out2 := res2
      io.out3 := res3
    }
    class LitAddSubTester(m: LitAddSub) extends Tester(m) {
      // (until "expect" learns to deal with 0x and negative numbers...)
      // Half of these are redundant.
      val res2 = peek(m.res2)
      val res3 = peek(m.res3)
      assertResult(-2) { res2 }
      assertResult(6) { res3 }
      val out2 = peek(m.io.out2)
      val out3 = peek(m.io.out3)
      assertResult(-2) { out2 }
      assertResult(6) { out3 }
      assertResult(4) {m.res2.getWidth}
      assertResult(3) {m.res3.getWidth}
      assertResult(4) {m.io.out2.getWidth}
      assertResult(8) {m.io.out3.getWidth}
    }

    chiselMainTest(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
      () => Module(new LitAddSub())) {m => new LitAddSubTester(m)}
  }

  /** Test for issue #168 - lit as port breaks chisel
   *
   */
  @Test def testLitAsPort () {
    println("\ntestLitAsPort ...")
    try {
      class LitAsPort extends Module {
        val io = new Bundle {
          val s2 = Bits(12)
        }
      }
      chiselMain(Array("--backend", "c",
        "--targetDir", dir.getPath.toString()),
        () => Module(new LitAsPort()))
    } catch {
      case e : java.lang.IllegalStateException => {}
    }
    assertTrue(ChiselError.hasErrors);
  }

  /** Test for issue #384 - Data width lost in Vec
   *
   */
  @Test def testVecSIntWidth () {
    class VecSIntWidth extends Module {
      val io = new Bundle {
        val addr = UInt(INPUT,  8)
        val out  = UInt(OUTPUT, 8)
      }
      val r = Vec((0 until 4).map(i => SInt(i, width = 32)))
      io.out := r(io.addr)
    }

    class VecSIntWidthTester(m: VecSIntWidth) extends Tester(m) {
      for (t <- 0 until 16) {
        val addr = rnd.nextInt(4)
        poke(m.io.addr, addr)
        step(1)
        expect(m.io.out, addr)
      }
    }

    chiselMainTest(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
      () => Module(new VecSIntWidth())) {m => new VecSIntWidthTester(m)}
  }

  /** Test for issue #407 - Don't care in literal ("?") exhibits bizarre behavior
   */
  @Test def testLitDontCare () {
    println("\ntestLitDontCare ...")
    try {
      class LitDontCare extends Module {
        val io = new Bundle {
          val in = UInt(INPUT,  8)
          val out  = UInt(OUTPUT, 8)
        }
        io.out := Mux(io.in === (Bits("b?110") | Bits("b1???")), io.in, UInt(0))
      }

      chiselMain(testArgs,
        () => Module(new LitDontCare()))
    } catch {
      case e : java.lang.IllegalStateException => {}
    }
    assertTrue(ChiselError.hasErrors);
  }
}
