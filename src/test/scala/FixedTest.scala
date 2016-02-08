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
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore


import Chisel._


/** This testsuite checks all methods in the Bits class.
*/
class FixedSuite extends TestSuite {

  def toFixedT(x : Double, fracWidth : Int) : BigInt = BigInt((x*scala.math.pow(2, fracWidth)).toInt)
  def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
  def toDouble(x : BigInt, fracWidth : Int) : Double = x.toDouble/scala.math.pow(2, fracWidth)

  @Test def testConversion() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val r = scala.util.Random
      val in = BigInt(r.nextInt(1 << 30))
      assertTrue(toFixed(toDouble(in, 16), 16) == in)
    }
    val dummyInst = Module(new Dummy)
  }

  /** Extract a bit from a constant at a fixed position */
  @Test def testFixedConstructor() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = Fixed(0, 16, 8)
      assertTrue( res.getWidth == 16 )
      assertTrue( res.getFractionalWidth == 8 )
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testFixedColonEqual() {
    class FixedColonEqual extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(OUTPUT, 16, 8)
      }
      io.b := io.a
      assertTrue( io.a.getWidth() == 32)
      assertTrue( io.a.getFractionalWidth() == 16)
      assertTrue( io.b.getWidth() == 16)
      assertTrue( io.b.getFractionalWidth() == 8)
    }

    trait FixedColonEqualTests extends Tests {
      val trials = 10
      def tests(c: FixedColonEqual) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 30))
          poke(c.io.a, inA)
          expect(c.io.b, toFixedT(toDouble(inA, 16), 8))
        }
      }
    }

    class FixedColonEqualTester(c : FixedColonEqual) extends Tester(c) with FixedColonEqualTests {
      tests(c)
    }

    launchCppTester((c: FixedColonEqual) => new FixedColonEqualTester(c))
  }

  @Test def testFixedEqual() {
    class FixedEqual extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Bool(OUTPUT)
      }
      io.c := io.a === io.b
    }

    trait FixedEqualTests extends Tests {
      val trials = 10
      def tests(c: FixedEqual) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 30))
          val inB = if (i%2 == 0) inA else BigInt(rnd.nextInt(1 << 30))
          poke(c.io.a, inA)
          poke(c.io.b, inB)
          expect(c.io.c, Bool(inA == inB).litValue())
        }
      }
    }

    class FixedEqualTester(c : FixedEqual) extends Tester(c) with FixedEqualTests {
      tests(c)
    }

    launchCppTester((c: FixedEqual) => new FixedEqualTester(c))
  }

  @Test def testFixedAdd() {
    class FixedAdd extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := io.a + io.b
    }

    trait FixedAddTests extends Tests {
      val trials = 10
      def tests(c: FixedAdd) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          val inB = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          poke(c.io.a, inA)
          poke(c.io.b, inB)
          expect(c.io.c, toFixed(toDouble(inA, 16) + toDouble(inB, 16), 16))
        }
      }
    }

    class FixedAddTester(c: FixedAdd) extends Tester(c) with FixedAddTests {
      tests(c)
    }

    launchCppTester((c: FixedAdd) => new FixedAddTester(c))
  }

  @Test def testFixedSub() {
    class FixedSub extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := io.a - io.b
    }

    trait FixedSubTests extends Tests {
      val trials = 10
      def tests(c: FixedSub) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          val inB = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          poke(c.io.a, inA)
          poke(c.io.b, inB)
          expect(c.io.c, toFixed(toDouble(inA, 16) - toDouble(inB, 16), 16))
        }
      }
    }

    class FixedSubTester(c : FixedSub) extends Tester(c) with FixedSubTests {
      tests(c)
    }

    launchCppTester((c: FixedSub) => new FixedSubTester(c))
  }

  @Test def testFixedReg() {
    class FixedReg extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := Reg(init=Fixed(0, 32, 16), next=io.a)
    }

    trait FixedRegTests extends Tests {
      val trials = 10
      def tests(c: FixedReg) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          poke(c.io.a, inA)
          step(1)
          expect(c.io.c, inA)
        }
      }
    }

    class FixedRegTester(c : FixedReg) extends Tester(c) with FixedRegTests {
      tests(c)
    }

    launchCppTester((c: FixedReg) => new FixedRegTester(c))
  }

  @Test def testFixedUnary() {
    class FixedUnary extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := -io.a
    }

    trait FixedUnaryTests extends Tests {
      val trials = 10
      def tests(c: FixedUnary) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          poke(c.io.a, inA)
          expect(c.io.c, -inA)
        }
      }
    }

    class FixedUnaryTester(c : FixedUnary) extends Tester(c) with FixedUnaryTests {
       tests(c)
    }

    launchCppTester((c: FixedUnary) => new FixedUnaryTester(c))
  }

  @Test def testFixedCompare() {
    class FixedCompare extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val gt = Bool(OUTPUT)
        val lt = Bool(OUTPUT)
        val gte = Bool(OUTPUT)
        val lte = Bool(OUTPUT)
      }
      io.gt := io.a > io.b
      io.lt := io.a < io.b
      io.gte := io.a >= io.b
      io.lte := io.a <= io.b
    }

    trait FixedCompareTests extends Tests {
      val trials = 10
      def tests(c: FixedCompare) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          val inB = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          poke(c.io.a, inA)
          poke(c.io.b, inB)
          expect(c.io.gt, Bool(inA > inB).litValue())
          expect(c.io.lt, Bool(inA < inB).litValue())
          expect(c.io.gte, Bool(inA >= inB).litValue())
          expect(c.io.lte, Bool(inA <= inB).litValue())
        }
      }
    }

    class FixedCompareTester(c : FixedCompare) extends Tester(c) with FixedCompareTests {
      tests(c)
    }

    launchCppTester((c: FixedCompare) => new FixedCompareTester(c))
  }

  @Test def testFixedDiv() {
    class FixedDiv extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := io.a / io.b
    }

    trait FixedDivTests extends Tests {
      val trials = 10
      def tests(c: FixedDiv) {
        for (i <- 0 until trials) {

          // For the testing find two numbers that we also give a number that is representable in fixed point
          var inA = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          var inB = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          var doubleA = toDouble(inA, 16)
          var doubleB = toDouble(inB, 16)
          while(  scala.math.abs(toDouble(toFixedT(doubleA / doubleB, 16), 16) - doubleA/doubleB) > scala.math.pow(2, -17)) {
            inA = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
            inB = BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
            doubleA = toDouble(inA, 16)
            doubleB = toDouble(inB, 16)
          }
          poke(c.io.a, inA)
          poke(c.io.b, inB)
          expect(c.io.c, toFixedT(toDouble(inA, 16) / toDouble(inB, 16), 16))
        }
      }
    }

    class FixedDivTester(c : FixedDiv) extends Tester(c) with FixedDivTests {
      tests(c)
    }

    launchCppTester((c: FixedDiv) => new FixedDivTester(c))
  }

  @Test def testFixedMul() {
    class FixedMul extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 64, 32)
      }
      io.c := io.a * io.b
    }

    trait FixedMulTests extends Tests {
      val trials = 10
      def tests(c: FixedMul) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          val inB = BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          poke(c.io.a, inA)
          poke(c.io.b, inB)
          expect(c.io.c, toFixedT(toDouble(inA, 16) * toDouble(inB, 16), 32))
        }
      }
    }

    class FixedMulTester(c : FixedMul) extends Tester(c) with FixedMulTests {
      tests(c)
    }

    launchCppTester((c: FixedMul) => new FixedMulTester(c))
  }

  @Test def testFixedMulT() {
    class FixedMulT extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := io.a *% io.b
    }

    trait FixedMulTTests extends Tests {
      val trials = 10
      def tests(c: FixedMulT) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          val inB = BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          poke(c.io.a, inA)
          poke(c.io.b, inB)
          val expected = toFixedT(toDouble(inA, 16) * toDouble(inB, 16), 16)
          val res = peek(c.io.c)
          val errorVal = scala.math.abs(scala.math.abs(expected.toInt) - scala.math.abs(res.toInt))
          val error = errorVal > 1
          val errorMessage = if(error) "Error outside acceptiable Range: " + errorVal.toString else "Error within acceptiable range: " + errorVal.toString
          expect(!error, errorMessage)
        }
      }
    }

    class FixedMulTTester(c : FixedMulT) extends Tester(c) with FixedMulTTests {
      tests(c)
    }

    launchCppTester((c: FixedMulT) => new FixedMulTTester(c))
  }

  @Test def testFixedMulR() {
    class FixedMulR extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := io.a *& io.b
    }

    trait FixedMulRTests extends Tests {
      val trials = 10
      def tests(c: FixedMulR) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          val inB = BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          poke(c.io.a, inA)
          poke(c.io.b, inB)
          expect(c.io.c, toFixed(toDouble(inA, 16) * toDouble(inB, 16), 16))
        }
      }
    }

    class FixedMulRTester(c : FixedMulR) extends Tester(c) with FixedMulRTests {
      tests(c)
    }

    launchCppTester((c: FixedMulR) => new FixedMulRTester(c))
  }

  @Test def testFixedLine() {
    class FixedLine extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      val temp = io.a + io.b
      val temp2 = temp *& io.a
      val temp3 = temp2 - io.b
      io.c := temp3
    }

    trait FixedLineTests extends Tests {
      val trials = 10
      def tests(c: FixedLine) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          val inB = BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          poke(c.io.a, inA)
          poke(c.io.b, inB)
          expect(c.io.c, toFixed((toDouble(inA, 16) + toDouble(inB, 16)) * toDouble(inA, 16) - toDouble(inB, 16), 16))
        }
      }
    }

    class FixedLineTester(c : FixedLine) extends Tester(c) with FixedLineTests {
      tests(c)
    }

    launchCppTester((c: FixedLine) => new FixedLineTester(c))
  }

  @Test def testFixedMux() {
    class FixedMux extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Fixed(INPUT, 32, 16)
        val sel = Bool(INPUT)
        val res = Fixed(OUTPUT, 32, 16)
      }
      val muxSel = Mux(io.sel, io.a + io.b, io.a + io.c)
      io.res := muxSel
    }

    trait FixedMuxTests extends Tests {
      val trials = 10
      def tests(c: FixedMux) {
        for (i <- 0 until trials) {
          val inA = BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          val inB = BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          val inC = BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt
          val inSel = rnd.nextInt(2)
          poke(c.io.a, inA)
          poke(c.io.b, inB)
          poke(c.io.c, inC)
          poke(c.io.sel, inSel)
          val res = if(inSel == 1) toDouble(inA, 16) + toDouble(inB, 16) else toDouble(inA, 16) + toDouble(inC, 16)
          expect(c.io.res, toFixed(res, 16))
        }
      }
    }

    class FixedMuxTester(c : FixedMux) extends Tester(c) with FixedMuxTests {
      tests(c)
    }

    launchCppTester((c: FixedMux) => new FixedMuxTester(c))
  }

  @Test def testFixedVec() {
    class FixedVec extends Module {
      val io = new Bundle {
        val a = Vec.fill(8){Fixed(INPUT, 32, 16)}
        val b = Vec.fill(8){Fixed(INPUT, 32, 16)}
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := (io.a, io.b).zipped.map(_ + _).reduce(_ + _)
    }

    trait FixedVecTests extends Tests {
      val trials = 10
      def tests(c: FixedVec) {
        for (i <- 0 until trials) {
          val inA = Array.fill(8){BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt}
          val inB = Array.fill(8){BigInt(rnd.nextInt(1 << 30)) * scala.math.pow(-1, rnd.nextInt(2)).toInt}
          (c.io.a, inA).zipped.map((w, v) => poke(w, v))
          (c.io.b, inB).zipped.map((w, v) => poke(w, v))
          val res = (inA.map(v => toDouble(v, 16)), inB.map(v => toDouble(v, 16))).zipped.map(_ + _).reduce(_ + _)
          expect(c.io.c, toFixed(res, 16))
        }
      }
    }

    class FixedVecTester(c : FixedVec) extends Tester(c) with FixedVecTests {
      tests(c)
    }

    launchCppTester((c: FixedVec) => new FixedVecTester(c))
  }

  @Test def testFixedVec2() {
    class FixedVec2 extends Module {
      val io = new Bundle {
        val a = Vec.fill(8){Fixed(INPUT, 32, 16)}
        val b = Vec.fill(8){Fixed(INPUT, 32, 16)}
        val c = Vec.fill(8){Fixed(OUTPUT, 32, 16)}
      }
      io.c := (io.a, io.b).zipped.map(_*&_)
    }

    trait FixedVec2Tests extends Tests {
      val trials = 10
      def tests(c: FixedVec2) {
        for (i <- 0 until trials) {
          val inA = Array.fill(8){BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt}
          val inB = Array.fill(8){BigInt(rnd.nextInt(1 << 15)) * scala.math.pow(-1, rnd.nextInt(2)).toInt}
          (c.io.a, inA).zipped.map((w, v) => poke(w, v))
          (c.io.b, inB).zipped.map((w, v) => poke(w, v))
          val res = (inA.map(v => toDouble(v, 16)), inB.map(v => toDouble(v, 16))).zipped.map(_*_)
          (c.io.c, res).zipped.map((w, v) => expect(w, toFixed(v, 16)))
        }
      }
    }

    class FixedVec2Tester(c : FixedVec2) extends Tester(c) with FixedVec2Tests {
      tests(c)
    }

    launchCppTester((c: FixedVec2) => new FixedVec2Tester(c))
  }

  @Test def testFixedMod() {
    class FixedMod extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := io.a % io.b
    }

    trait FixedModTests extends Tests {
      val trials = 10
      def tests(c: FixedMod) {
        for (i <- 0 until trials) {

          // For the testing find two numbers that we also give a number that is representable in fixed point
          var inA = BigInt(rnd.nextInt(1 << 30))
          var inB = BigInt(rnd.nextInt(1 << 30))
          poke(c.io.a, inA)
          poke(c.io.b, inB)
          val mod = inA % inB
          expect(c.io.c, mod)
        }
      }
    }

    class FixedModTester(c : FixedMod) extends Tester(c) with FixedModTests {
      tests(c)
    }

    launchCppTester((c: FixedMod) => new FixedModTester(c))
  }
}
