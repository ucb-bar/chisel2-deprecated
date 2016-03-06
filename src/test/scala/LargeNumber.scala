/*
 Copyright (c) 2011 - 2016 The Regents of the University of
 Sydney. All Rights Reserved.  Redistribution and use in
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
import scala.util.Random
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._

/** This test suite tests the emulator with large integers ( > 64 bits )
  */
class LargeNumberSuite extends TestSuite {

  val bitWidth = 130

  def getBigRandom(rnd: Random, bitWidth : Int) : BigInt = {
    val noWords = (bitWidth / 64).toInt + 1
    var myRand = BigInt( rnd.nextLong )
    for ( i <- 0 until noWords )
      myRand = ( myRand << 64 ) | BigInt( rnd.nextLong )
    val res = myRand & ( ( BigInt(1) << bitWidth ) - 1 )
    res
  }

  def toSigned(myNum : BigInt, bitWidth : Int) : BigInt = {
    if ( myNum > ( BigInt(1) << ( bitWidth - 1 ) ))
      myNum - (BigInt(1) << bitWidth)
    else
      myNum
  }

  class SandUIntIO(bitWidth : Int) extends Bundle {
    val x_s = SInt(INPUT, width=bitWidth)
    val y_s = SInt(INPUT, width=bitWidth)
    val x_u = UInt(INPUT, width=bitWidth)
    val y_u = UInt(INPUT, width=bitWidth)
    val z_s = SInt(OUTPUT, width=bitWidth)
    val z_u = UInt(OUTPUT, width=bitWidth)
  }

  @Test def testNot() {
    class Not extends Module {
      val io = new SandUIntIO(bitWidth)
      io.z_s := ~ io.x_s
      io.z_u := ~ io.x_u
    }

    class NotTests(c : Not) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val z = ~ x
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      expect(c.io.z_s, z)
      expect(c.io.z_u, z)
    }
    launchCppTester((c: Not) => new NotTests(c))
  }

  @Test def testAdd() {
    class Add extends Module {
      val io = new SandUIntIO(bitWidth)
      io.z_s := io.x_s + io.y_s
      io.z_u := io.x_u + io.y_u
    }

    class AddTests(c : Add) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = getBigRandom(rnd, bitWidth)
      val z_u = x + y
      val z_s = toSigned(x, bitWidth) + toSigned(y, bitWidth)
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, y)
      expect(c.io.z_s, z_s)
      expect(c.io.z_u, z_u)
    }
    launchCppTester((c: Add) => new AddTests(c))
  }

  @Test def testSub() {
    class Sub extends Module {
      val io = new SandUIntIO(bitWidth)
      io.z_s := io.x_s - io.y_s
      io.z_u := io.x_u - io.y_u
    }

    class SubTests(c : Sub) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = getBigRandom(rnd, bitWidth)
      println("x = " + x)
      println("y = " + y)
      val z_u = x - y
      val z_s = toSigned(x, bitWidth) - toSigned(y, bitWidth)
      println("x = " + x)
      println("y = " + y)
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, y)
      expect(c.io.z_s, z_s)
      expect(c.io.z_u, z_u)
    }
    launchCppTester((c: Sub) => new SubTests(c))
  }

  @Test def testMult() {
    class Mult extends Module {
      val io = new SandUIntIO(bitWidth)
      io.z_s := io.x_s * io.y_s
      io.z_u := io.x_u * io.y_u
    }

    class MultTests(c : Mult) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = getBigRandom(rnd, bitWidth)
      val z_u = x * y
      val z_s = toSigned(x, bitWidth) * toSigned(y, bitWidth)
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, y)
      expect(c.io.z_s, z_s)
      expect(c.io.z_u, z_u)
    }
    launchCppTester((c: Mult) => new MultTests(c))
  }

  @Test def testDiv() {
    class Div extends Module {
      val io = new SandUIntIO(bitWidth)
      io.z_s := io.x_s / io.y_s
      io.z_u := io.x_u / io.y_u
    }

    class DivTests(c : Div) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = getBigRandom(rnd, bitWidth/4)
      val z_u = x / y
      val z_s = toSigned(x, bitWidth) / toSigned(y, bitWidth)
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, y)
      expect(c.io.z_s, z_s)
      expect(c.io.z_u, z_u)
    }
    launchCppTester((c: Div) => new DivTests(c))
  }

  @Test def testLsh() {
    class Lsh extends Module {
      val io = new SandUIntIO(bitWidth)
      io.z_s := io.x_s << io.y_u
      io.z_u := io.x_u << io.y_u
    }

    class LshTests(c : Lsh) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = rnd.nextInt(bitWidth)
      val z_u = x << y
      val z_s = toSigned(x, bitWidth) << y
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, BigInt(y))
      poke(c.io.y_u, BigInt(y))
      expect(c.io.z_s, z_s)
      expect(c.io.z_u, z_u)
    }
    launchCppTester((c: Lsh) => new LshTests(c))
  }

  @Test def testRsh() {
    class Rsh extends Module {
      val io = new SandUIntIO(bitWidth)
      io.z_s := io.x_s >> io.y_u
      io.z_u := io.x_u >> io.y_u
    }

    class RshTests(c : Rsh) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = rnd.nextInt(bitWidth)
      val z_u = x >> y
      val z_s = toSigned(x, bitWidth) >> y
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, BigInt(y))
      poke(c.io.y_u, BigInt(y))
      expect(c.io.z_s, z_s)
      expect(c.io.z_u, z_u)
    }
    launchCppTester((c: Rsh) => new RshTests(c))
  }

  // regression test for github #576
  @Test def testRshA576() {
    class RightShift extends Module {
      val W = 64

      val io = new Bundle {
        val a = SInt(INPUT, W)
        val b = UInt(INPUT, log2Up(W))
        val signed = Bool(INPUT)
        val out = SInt(OUTPUT, W)
      }

      val toshift = Cat(io.a(W - 1) & io.signed, io.a).toSInt
      io.out := toshift >> io.b
    }

    class RightShiftTester(c: RightShift) extends Tester(c) {
      val test = BigInt("-8000000000000000", 16)

      poke(c.io.a, test)
      poke(c.io.signed, 1)

      for (i <- 0 until 8) {
        poke(c.io.b, i)
        step(1)
        expect(c.io.out, test >> i)
      }
    }
    launchCppTester((c:RightShift) => new RightShiftTester(c))
  }

  @Test def testRshExt() {
    class Rsh extends Module {
      val io = new Bundle {
        val x = SInt(INPUT, width=66)
        val z = SInt(OUTPUT, width=33)
      }
      io.z := io.x >> UInt(24, width=33)
    }

    class RshTests(c : Rsh) extends Tester(c) {
      val x = BigInt(4003500)*BigInt(825802)
      val z = x >> 24
      poke(c.io.x, x)
      expect(c.io.z, z)
    }
    launchCppTester((c: Rsh) => new RshTests(c))
  }

  @Test def testCat() {
    class Cat extends Module {
      val io = new Bundle {
        val x_s = SInt(INPUT, width=bitWidth/2)
        val y_s = SInt(INPUT, width=bitWidth/2)
        val x_u = UInt(INPUT, width=bitWidth/2)
        val y_u = UInt(INPUT, width=bitWidth/2)
        val z_s = SInt(OUTPUT, width=bitWidth)
        val z_u = UInt(OUTPUT, width=bitWidth)
      }
      io.z_s := io.x_s ## io.y_s
      io.z_u := io.x_u ## io.y_u
    }

    class CatTests(c : Cat) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth/2)
      val y = getBigRandom(rnd, bitWidth/2)
      val z = (x << (bitWidth/2)) | y
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, y)
      expect(c.io.z_s, z)
      expect(c.io.z_u, z)
    }
    launchCppTester((c: Cat) => new CatTests(c))
  }

  @Test def testXor() {
    class Xor extends Module {
      val io = new SandUIntIO(bitWidth)
      io.z_s := io.x_s ^ io.y_s
      io.z_u := io.x_u ^ io.y_u
    }

    class XorTests(c : Xor) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = getBigRandom(rnd, bitWidth)
      val z = x ^ y
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, y)
      expect(c.io.z_s, z)
      expect(c.io.z_u, z)
    }
    launchCppTester((c: Xor) => new XorTests(c))
  }

  @Test def testAnd() {
    class And extends Module {
      val io = new SandUIntIO(bitWidth)
      io.z_s := io.x_s & io.y_s
      io.z_u := io.x_u & io.y_u
    }

    class AndTests(c : And) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = getBigRandom(rnd, bitWidth)
      val z = x & y
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, y)
      expect(c.io.z_s, z)
      expect(c.io.z_u, z)
    }
    launchCppTester((c: And) => new AndTests(c))
  }

  @Test def testOr() {
    class Or extends Module {
      val io = new SandUIntIO(bitWidth)
      io.z_s := io.x_s | io.y_s
      io.z_u := io.x_u | io.y_u
    }

    class OrTests(c : Or) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = getBigRandom(rnd, bitWidth)
      val z = x | y
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, y)
      expect(c.io.z_s, z)
      expect(c.io.z_u, z)
    }
    launchCppTester((c: Or) => new OrTests(c))
  }

  @Test def testLt() {
    class Lt extends Module {
      val io = new Bundle {
        val x_s = SInt(INPUT, width=bitWidth)
        val y_s = SInt(INPUT, width=bitWidth)
        val x_u = UInt(INPUT, width=bitWidth)
        val y_u = UInt(INPUT, width=bitWidth)
        val z_s = Bool(OUTPUT)
        val z_u = Bool(OUTPUT)
      }
      io.z_s := io.x_s < io.y_s
      io.z_u := io.x_u < io.y_u
    }

    class LtTests(c : Lt) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = getBigRandom(rnd, bitWidth)
      val z_u = x < y
      val z_s = toSigned(x, bitWidth) < toSigned(y, bitWidth)
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, y)
      expect(c.io.z_s, Bool(z_s).litValue())
      expect(c.io.z_u, Bool(z_u).litValue())
    }
    launchCppTester((c: Lt) => new LtTests(c))
  }

  @Test def testLtEq() {
    class LtEq extends Module {
      val io = new Bundle {
        val x_s = SInt(INPUT, width=bitWidth)
        val y_s = SInt(INPUT, width=bitWidth)
        val x_u = UInt(INPUT, width=bitWidth)
        val y_u = UInt(INPUT, width=bitWidth)
        val z_s = Bool(OUTPUT)
        val z_u = Bool(OUTPUT)
      }
      io.z_s := io.x_s <= io.y_s
      io.z_u := io.x_u <= io.y_u
    }

    class LtEqTests(c : LtEq) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = getBigRandom(rnd, bitWidth)
      val z_u = x <= y
      val z_s = toSigned(x, bitWidth) <= toSigned(y, bitWidth)
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, y)
      expect(c.io.z_s, Bool(z_s).litValue())
      expect(c.io.z_u, Bool(z_u).litValue())
    }
    launchCppTester((c: LtEq) => new LtEqTests(c))
  }

  @Test def testEq() {
    class Eq extends Module {
      val io = new Bundle {
        val x_s = SInt(INPUT, width=bitWidth)
        val y_s = SInt(INPUT, width=bitWidth)
        val x_u = UInt(INPUT, width=bitWidth)
        val y_u = UInt(INPUT, width=bitWidth)
        val z_s = Bool(OUTPUT)
        val z_u = Bool(OUTPUT)
      }
      io.z_s := io.x_s === io.y_s
      io.z_u := io.x_u === io.y_u
    }

    class EqTests(c : Eq) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = getBigRandom(rnd, bitWidth)
      val z_u = x == x
      val z_s = toSigned(x, bitWidth) == toSigned(y, bitWidth)
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, x)
      expect(c.io.z_s, Bool(z_s).litValue())
      expect(c.io.z_u, Bool(z_u).litValue())
    }
    launchCppTester((c: Eq) => new EqTests(c))
  }

  @Test def testNeq() {
    class Neq extends Module {
      val io = new Bundle {
        val x_s = SInt(INPUT, width=bitWidth)
        val y_s = SInt(INPUT, width=bitWidth)
        val x_u = UInt(INPUT, width=bitWidth)
        val y_u = UInt(INPUT, width=bitWidth)
        val z_s = Bool(OUTPUT)
        val z_u = Bool(OUTPUT)
      }
      io.z_s := ( io.x_s =/= io.y_s )
      io.z_u := ( io.x_u =/= io.y_u )
    }

    class NeqTests(c : Neq) extends Tester(c) {
      val x = getBigRandom(rnd, bitWidth)
      val y = getBigRandom(rnd, bitWidth)
      val z_u = x != x
      val z_s = toSigned(x, bitWidth) != toSigned(y, bitWidth)
      poke(c.io.x_s, x)
      poke(c.io.x_u, x)
      poke(c.io.y_s, y)
      poke(c.io.y_u, x)
      expect(c.io.z_s, Bool(z_s).litValue())
      expect(c.io.z_u, Bool(z_u).litValue())
    }
    launchCppTester((c: Neq) => new NeqTests(c))
  }
}
