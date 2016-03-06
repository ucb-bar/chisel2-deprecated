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

import org.scalatest._
import org.junit.Assert._
import org.junit.Before
import org.junit.BeforeClass
import org.junit.Test
import org.junit.Ignore
import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

import Chisel._

object ZeroWidthTest{
  val W0WparameterName = "W0W"
  val enableW0W = true

  /* Should we enable these tests?
   * Either look for the JVM setting "W0W", or the value of enableW0W.
   */
  @BeforeClass def checkSupport() {
    val W0Wparameter = System.getProperty(W0WparameterName)
    val W0Wenable = if (W0Wparameter == null) enableW0W else true
    org.junit.Assume.assumeTrue(W0Wenable)
  }

}
/** This testsuite checks operations using zero-width wires.
*/
class ZeroWidthTest extends TestSuite {
  val backend = "dot"
  val testArgs = Array("--W0W",
      "--backend", backend,
      "--targetDir", dir.getPath.toString()
      )

  def filterArgs(args: Array[String], amap: HashMap[String, String]): Array[String] = {
    val newArgs = ArrayBuffer[String]()
    for (arg <- args) {
      if (amap.contains(arg)) {
        newArgs += amap(arg)
      } else {
        newArgs += arg
      }
    }
    newArgs.toArray
  }

  @Before override def initialize() {
    super.initialize()
    // Enable W0W support
    Driver.isSupportW0W = true
  }

  /** Generate a zero-width wire (explicitly) */
  @Test def testImplicitZeroWidth() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = UInt(0,0)
      assertTrue( res.getWidth == 0 )
      assertTrue( res.litValue() == 0 )
    }
    val dummyInst = Module(new Dummy)
  }

  /** Generate a zero-width wire from a '>>' */
  @Test def testRSHZeroWidth() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = UInt(3,2) >> UInt(2)
      assertTrue( res.getWidth == 0 )
      assertTrue( res.litValue() == 0 )
    }
    val dummyInst = Module(new Dummy)
  }

  /** Generate a zero-width wire from an extraction. */
  @Test def testExtractZeroWidth() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = UInt(3)((0, 1))
      assertTrue( res.getWidth == 0 )
      assertTrue( res.litValue() == 0 )
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testOperators() {
  try {
    class OperatorComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 0)  /* explicitly set x to zero-width */
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

    chiselMain(testArgs, () => Module(new OperatorComp()))
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }

  /** Multiply an unsigned number by an unsigned zero-width number */
  @Test def testMulUUZ() {
    println("\ntestMulUUZ ...")
    class MulUUZ extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 32)
        val y = UInt(INPUT, 0)
        val z = UInt(OUTPUT)
      }
      io.z := io.x * io.y
    }
    chiselMain(testArgs, () => Module(new MulUUZ()))
    assertFile("ZeroWidthTest_MulUUZ_1." + backend)
  }

  /** Multiply an unsigned number by signed zero-width number */
  @Test def testMulUZ() {
    println("\ntestMulUZ ...")
    class MulUZ extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 32)
        val y = SInt(INPUT, 0)
        val z = SInt(OUTPUT)
      }
      io.z := io.x * io.y
    }
    chiselMain(testArgs, () => Module(new MulUZ()))
    assertFile("ZeroWidthTest_MulUZ_1." + backend)
  }

  /** Divide an unsigned number by a zero-width unsigned number */
  @Test def testDivUUZ() {
    println("\ntestDivUUZ ...")
    class DivUUZ extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 32)
        val y = UInt(INPUT, 0)
        val z = SInt(OUTPUT)
      }
      io.z := io.x / io.y
    }
    chiselMain(testArgs, () => Module(new DivUUZ()))
    assertFile("ZeroWidthTest_DivUUZ_1." + backend)
  }

  /** Divide an unsigned zero-width number by an unsigned number - expect an error. */
  @Test def testDivUZ() {
    println("\ntestDivUZ ...")
    class DivUZ extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 0)
        val y = UInt(INPUT, 32)
        val z = UInt(OUTPUT)
      }
      io.z := io.x / io.y
    }
    chiselMain(testArgs, () => Module(new DivUZ()))
    assertFile("ZeroWidthTest_DivUZ_1." + backend)
  }

  /** Remainer of an unsigned number by signed zero-width number */
  @Test def testRemUZ() {
    println("\ntestRemUZ ...")
    class RemUZ extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 32)
        val y = SInt(INPUT, 0)
        val z = SInt(OUTPUT)
      }
      io.z := io.x % io.y
    }
    chiselMain(testArgs, () => Module(new RemUZ()))
    assertFile("ZeroWidthTest_RemUZ_1." + backend)
  }

  /** Multiply a signed zero-width number by an unsigned number */
  @Test def testMulZU() {
    println("\ntestMulZU ...")
    class MulZU extends Module {
      val io = new Bundle {
        val x = SInt(INPUT, 0)
        val y = UInt(INPUT, 32)
        val z = SInt(OUTPUT)
      }
      io.z := io.x * io.y
    }
    chiselMain(testArgs, () => Module(new MulZU()))
    assertFile("ZeroWidthTest_MulZU_1." + backend)
  }

  /** Multiply an unsigned zero-width number by a signed number. */
  @Test def testMulZS() {
    println("\ntestMulZS ...")
    class MulZS extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 0)
        val y = SInt(INPUT, 32)
        val z = SInt(OUTPUT)
      }
      io.z := io.x * io.y
    }
    class MulZSTester(m: MulZS) extends Tester(m) {
      poke(m.io.y, 42)
      step(1)
      expect(m.io.z, 42)
    }
    // Replace whatever backend we're using with verilog.
    if (true) {
      chiselMain(testArgs, () => Module(new MulZS()))
      assertFile("ZeroWidthTest_MulZS_1." + backend)
    } else {
      val replaceArgs = HashMap[String, String]((backend, "c"))
      val testerArgs = Array("--genHarness", "--compile", "--test")
      chiselMainTest(filterArgs(testArgs, replaceArgs) ++ testerArgs, () => Module(new MulZS())) {m => new MulZSTester(m)}
    }
  }

  /** Divide a signed zero-width number by an unsigned number */
  @Test def testDivZU() {
    println("\ntestDivZU ...")
    class DivZU extends Module {
      val io = new Bundle {
        val x = SInt(INPUT, 0)
        val y = UInt(INPUT, 32)
        val z = SInt(OUTPUT)
      }
      io.z := io.x / io.y
    }
    chiselMain(testArgs, () => Module(new DivZU()))
    assertFile("ZeroWidthTest_DivZU_1." + backend)
  }

  /** Remainder of a signed zero-width number by an unsigned number */
  @Test def testRemZU() {
    println("\ntestRemZU ...")
    class RemZU extends Module {
      val io = new Bundle {
        val x = SInt(INPUT, 0)
        val y = UInt(INPUT, 32)
        val z = SInt(OUTPUT)
      }
      io.z := io.x % io.y
    }
    chiselMain(testArgs, () => Module(new RemZU()))
    assertFile("ZeroWidthTest_RemZU_1." + backend)
  }

  /** Concatenate two nodes X and Y (zero-width) in a node Z such that
    Z[0..wx + wy] = X[0..wx] :: Y[0..wy]. */
  @Test def testCatCompW0W() {
    println("\ntestCat ...")
    class CatCompW0W extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 8)
        val y = UInt(INPUT, 0)
        val z = UInt(OUTPUT)
      }
      io.z := io.x ## io.y
    }

    chiselMain(testArgs, () => Module(new CatCompW0W()))
    assertFile("ZeroWidthTest_CatCompW0W_1." + backend)
  }

  /** Generate a lookup into an array.
    XXX Lookup.scala, use different code based on instance of CppBackend. */
  @Test def testLookup() {
    println("\ntestLookup ...")
    class LookupComp extends Module {
      val io = new Bundle {
        val addr = UInt(INPUT, 0)
        val data = UInt(OUTPUT)
      }
      io.data := Lookup(io.addr, UInt(0), Array(
        (UInt(0), UInt(10)),
        (UInt(1), UInt(11))))
    }

    chiselMain(testArgs, () => Module(new LookupComp()))
  }

  /** Generate a foldR
    */
  @Test def testfoldR() {
    println("\ntestfoldR ...")
    class foldRComp extends Module {
      val io = new Bundle {
        val in0 = UInt(INPUT, 8)
        val in1 = UInt(INPUT, 0)
        val in2 = UInt(INPUT, 8)
        val out = UInt(OUTPUT)
      }
      io.out := foldR(io.in0 :: io.in1 :: io.in2 :: Nil){ _ + _ }
    }

    chiselMain(testArgs, () => Module(new foldRComp()))
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

  /** Reverse produced many width warning messages.
    */
  @Test def testReverseNoisyWidth() {
    println("\ntesttestReverseNoisyWidth ...")
    class ReverseNoisyWidth extends Module {
      val io = new Bundle {
        val i = UInt(INPUT, width=64)
        val o = UInt(OUTPUT, width=64)
      }
      io.o := Reverse(io.i)
    }

    chiselMain(testArgs, () => Module(new ReverseNoisyWidth()))
    assertTrue(ChiselError.isEmpty);
  }

  /** Issue #335
   *  When you have an output that is driven inside of a when statement,
   *   but is left floating in other cases, a java.lang.NullPointerException
   *    is thrown during "checking widths".
   */
  @Test def testUnRefOutNullPointer() {
    println("\ntestUnRefOutNullPointer ...")
    class FloatOutModule extends Module {

      val io = new Bundle {
        val i_value     = UInt(INPUT, width = 64)
        val i_valid     = Bool(INPUT)
        val o_value     = UInt(OUTPUT, width = 64)
      }

      when ( io.i_valid ) {
        io.o_value := io.i_value
      }
    }
  }

  /** Issue 439
    *  Chisel somehow finds a way to build negative (actually zero) width wires.
    */
  @Test def testZeroWidthForceMatching {
    println("\ntestZeroWidthForceMatching ...")
    class ZeroWidthForceMatching extends Module {
      val io = UInt(OUTPUT, 1)
      io := UInt(1, 1)
      io := UInt(width = 0)
    }

    class ZeroWidthForceMatchingTester(c: ZeroWidthForceMatching) extends Tester(c) {
      assertTrue(c.io.getWidth == 1)
      expect(c.io, 0)
    }
    if (true) {
      // Replace whatever backend we're using with verilog.
      val replaceArgs = HashMap[String, String]((backend, "v"))
      chiselMain(filterArgs(testArgs, replaceArgs) , () => Module(new ZeroWidthForceMatching()))
      assertFile("ZeroWidthTest_ZeroWidthForceMatching_1." + "v")
    } else {
      val replaceArgs = HashMap[String, String]((backend, "c"))
      val testerArgs = Array("--genHarness", "--compile", "--test")
      chiselMainTest(filterArgs(testArgs, replaceArgs) ++ testerArgs, () => Module(new ZeroWidthForceMatching())) {m => new ZeroWidthForceMatchingTester(m)}
    }
  }
}
