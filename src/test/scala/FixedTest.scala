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

  // Newton-Rhapson to find 1/x - x_(t+1) = x_t(2 - a*x_t)
  def performNR(in : Double, xt : Double) : Double = xt*(2.0 - in*xt)

  def tailNR(in : Double, xt : Double, it : Int) : Double = {
    val nxt = performNR(in, xt)
    if (it == 0) nxt else tailNR(in, nxt, it - 1)
  }

  @Test def testConversion() {
    val r = scala.util.Random
    val in = BigInt(r.nextInt(1 << 30))
    assertTrue(toFixed(toDouble(in, 16), 16) == in)
  }

  /** Extract a bit from a constant at a fixed position */
  @Test def testFixedConstructor() {
    val res = Fixed(0, 16, 8)
    assertTrue( res.getWidth == 16 )
    assertTrue( res.getFractionalWidth == 8 )
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

    class FixedEqualTests(c : FixedEqual) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << 30))
        val inB = if (i%2 == 0) inA else BigInt(r.nextInt(1 << 30))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        expect(c.io.c, Bool(inA == inB).litValue())
      }
    }

    launchCppTester((c: FixedEqual) => new FixedEqualTests(c))
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

    class FixedAddTests(c : FixedAdd) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << 30))
        val inB = BigInt(r.nextInt(1 << 30))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        expect(c.io.c, toFixed(toDouble(inA, 16) + toDouble(inB, 16), 16))
      }
    }

    launchCppTester((c: FixedAdd) => new FixedAddTests(c))
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

    class FixedSubTests(c : FixedSub) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << 30))
        val inB = BigInt(r.nextInt(1 << 30))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        expect(c.io.c, toFixed(toDouble(inA, 16) - toDouble(inB, 16), 16))
      }
    }

    launchCppTester((c: FixedSub) => new FixedSubTests(c))
  }
  
  @Test def testFixedReg() {
    class FixedReg extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := Reg(init=Fixed(0, 32, 16), next=io.a)
    }

    class FixedRegTests(c : FixedReg) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << 30))
        poke(c.io.a, inA)
        step(1)
        expect(c.io.c, inA)
      }
    }

    launchCppTester((c: FixedReg) => new FixedRegTests(c))
  }

  @Test def testFixedUnary() {
    class FixedUnary extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := -io.a
    }

    class FixedUnaryTests(c : FixedUnary) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << 30))
        poke(c.io.a, inA)
        expect(c.io.c, -inA)
      }
    }

    launchCppTester((c: FixedUnary) => new FixedUnaryTests(c))
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

    class FixedCompareTests(c : FixedCompare) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << 30))
        val inB = BigInt(r.nextInt(1 << 30))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        expect(c.io.gt, Bool(inA > inB).litValue())
        expect(c.io.lt, Bool(inA < inB).litValue())
        expect(c.io.gte, Bool(inA >= inB).litValue())
        expect(c.io.lte, Bool(inA <= inB).litValue()) 
      }
    }

    launchCppTester((c: FixedCompare) => new FixedCompareTests(c))
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

    class FixedDivTests(c : FixedDiv) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {

        // For the testing find two numbers that we also give a number that is representable in fixed point
        var inA = BigInt(r.nextInt(1 << 30))
        var inB = BigInt(r.nextInt(1 << 30))
        var doubleA = toDouble(inA, 16)
        var doubleB = toDouble(inB, 16)
        while(  scala.math.abs(toDouble(toFixedT(doubleA / doubleB, 16), 16) - doubleA/doubleB) > scala.math.pow(2, -17)) {
          inA = BigInt(r.nextInt(1 << 30))
          inB = BigInt(r.nextInt(1 << 30))
          doubleA = toDouble(inA, 16)
          doubleB = toDouble(inB, 16)
        }
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        expect(c.io.c, toFixedT(toDouble(inA, 16) / toDouble(inB, 16), 16))
      }
    }

    launchCppTester((c: FixedDiv) => new FixedDivTests(c))
  }
  
  @Test def testFixedMulT() {
    class FixedMulT extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := io.a * io.b
    }

    class FixedMulTTests(c : FixedMulT) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << 15))
        val inB = BigInt(r.nextInt(1 << 15))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        expect(c.io.c, toFixedT(toDouble(inA, 16) * toDouble(inB, 16), 16))
      }
    }

    launchCppTester((c: FixedMulT) => new FixedMulTTests(c))
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

    class FixedMulRTests(c : FixedMulR) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << 15))
        val inB = BigInt(r.nextInt(1 << 15))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        expect(c.io.c, toFixed(toDouble(inA, 16) * toDouble(inB, 16), 16))
      }
    }

    launchCppTester((c: FixedMulR) => new FixedMulRTests(c))
  }

  @Test def testFixedLine() {
    class FixedLine extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 32, 16)
        val b = Fixed(INPUT, 32, 16)
        val c = Fixed(OUTPUT, 32, 16)
      }
      val temp = io.a + io.b
      val temp2 = temp - io.a
      val temp3 = temp2 - io.b
      io.c := temp3
    }

    class FixedLineTests(c : FixedLine) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << 15))
        val inB = BigInt(r.nextInt(1 << 15))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        expect(c.io.c, toFixed(toDouble(inA, 16) + toDouble(inB, 16) - toDouble(inA, 16) - toDouble(inB, 16), 16))
      }
    }

    launchCppTester((c: FixedLine) => new FixedLineTests(c))
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

    class FixedMuxTests(c : FixedMux) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = BigInt(r.nextInt(1 << 15))
        val inB = BigInt(r.nextInt(1 << 15))
        val inC = BigInt(r.nextInt(1 << 15))
        val inSel = r.nextInt(2)
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        poke(c.io.c, inC)
        poke(c.io.sel, inSel)
        val res = if(inSel == 1) toDouble(inA, 16) + toDouble(inB, 16) else toDouble(inA, 16) + toDouble(inC, 16)
        expect(c.io.res, toFixed(res, 16))
      }
    }

    launchCppTester((c: FixedMux) => new FixedMuxTests(c))
  }
  
  @Test def testFixedVec() {
    class FixedVec extends Module {
      val io = new Bundle {
        val a = Vec.fill(8){Fixed(INPUT, 32, 16)}
        val b = Vec.fill(8){Fixed(INPUT, 32, 16)}
        val c = Fixed(OUTPUT, 32, 16)
      }
      io.c := (io.a, io.b).zipped.map(_+_).reduce(_+_)
    }

    class FixedVecTests(c : FixedVec) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = Array.fill(8){BigInt(r.nextInt(1 << 30))}
        val inB = Array.fill(8){BigInt(r.nextInt(1 << 30))}
        (c.io.a, inA).zipped.map((w, v) => poke(w, v))
        (c.io.b, inB).zipped.map((w, v) => poke(w, v))
        val res = (inA.map(v => toDouble(v, 16)), inB.map(v => toDouble(v, 16))).zipped.map(_+_).reduce(_+_)
        expect(c.io.c, toFixed(res, 16))
      }
    }

    launchCppTester((c: FixedVec) => new FixedVecTests(c))
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

    class FixedVec2Tests(c : FixedVec2) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        val inA = Array.fill(8){BigInt(r.nextInt(1 << 15))}
        val inB = Array.fill(8){BigInt(r.nextInt(1 << 15))}
        (c.io.a, inA).zipped.map((w, v) => poke(w, v))
        (c.io.b, inB).zipped.map((w, v) => poke(w, v))
        val res = (inA.map(v => toDouble(v, 16)), inB.map(v => toDouble(v, 16))).zipped.map(_*_)
        (c.io.c, res).zipped.map((w, v) => expect(w, toFixed(v, 16)))
      }
    }

    launchCppTester((c: FixedVec2) => new FixedVec2Tests(c))
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

    class FixedModTests(c : FixedMod) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {

        // For the testing find two numbers that we also give a number that is representable in fixed point
        var inA = BigInt(r.nextInt(1 << 30))
        var inB = BigInt(r.nextInt(1 << 30))
        var doubleA = toDouble(inA, 16)
        var doubleB = toDouble(inB, 16)
        while(  scala.math.abs(toDouble(toFixedT(doubleA / doubleB, 16), 16) - doubleA/doubleB) > scala.math.pow(2, -17)) {
          inA = BigInt(r.nextInt(1 << 30))
          inB = BigInt(r.nextInt(1 << 30))
          doubleA = toDouble(inA, 16)
          doubleB = toDouble(inB, 16)
        }
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        val div = toDouble(inA, 16) / toDouble(inB, 16)
        val mod = div - div.toInt.toDouble
        expect(c.io.c, toFixedT(mod, 16))
      }
    }

    launchCppTester((c: FixedMod) => new FixedModTests(c))
  }

  @Test def testFixedNDiv() {
    class FixedNDiv extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 24, 16)
        val b = Fixed(INPUT, 24, 16)
        val c = Fixed(OUTPUT, 24, 16)
      }
      io.c := io.a /& io.b
    }

    class FixedNDivTests(c : FixedNDiv) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        var inA = BigInt(r.nextInt(1 << 22))
        var inB = BigInt(r.nextInt(1 << 22))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        val div = toDouble(inA, 16) / toDouble(inB, 16)
        val nrDiv = peek(c.io.c)
        val err = scala.math.abs(toDouble(nrDiv, 16) - div)
        val res = if (err < toDouble(BigInt(scala.math.pow(2, 8).toInt), 16)) true else false
        expect(res, "Error: " + err.toString)
      }
    }

    launchCppTester((c: FixedNDiv) => new FixedNDivTests(c))
  }
  
  // This is the same as the NDiv test, however the optional pipelining is enabled.
  @Test def testFixedNDivPipe() {
    class FixedNDivPipe extends Module {
      val io = new Bundle {
        val a = Fixed(INPUT, 24, 16)
        val b = Fixed(INPUT, 24, 16)
        val c = Fixed(OUTPUT, 24, 16)
      }
      io.c := io.a /& (io.b, true)
    }

    class FixedNDivPipeTests(c : FixedNDivPipe) extends Tester(c) {
      val trials = 10
      val r = scala.util.Random

      for (i <- 0 until trials) {
        var inA = BigInt(r.nextInt(1 << 22))
        var inB = BigInt(r.nextInt(1 << 22))
        poke(c.io.a, inA)
        poke(c.io.b, inB)
        step(5)
        val div = toDouble(inA, 16) / toDouble(inB, 16)
        val nrDiv = peek(c.io.c)
        val err = scala.math.abs(toDouble(nrDiv, 16) - div)
        val res = if (err < toDouble(BigInt(scala.math.pow(2, 8).toInt), 16)) true else false
        expect(res, "Error: " + err.toString)
      }
    }

    launchCppTester((c: FixedNDivPipe) => new FixedNDivPipeTests(c))
  }
}
