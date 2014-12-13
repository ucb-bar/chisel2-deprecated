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

import org.junit.Assert._
import org.junit.Test

import Chisel._
import Chisel.Implicits._
import Chisel.AdvTester._

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

class TesterTest extends TestSuite {
  val testArgs = Array("--backend", "v",
      "--targetDir", dir.getPath.toString()
      )

  /** Test poking various numbers.
   *  This is primarily a test of the Tester and its peek/poke/expect interface.
   *
   */
  @Test def testVariousPokes () {
    println("\ntestVariousPokes ...")
    
    class IOSelector extends Module {
      val io = new Bundle {
        val selectIn = UInt(INPUT, width=4)
        val in8 = UInt(INPUT, width=8)
        val in16 = UInt(INPUT, width=16)
        val in24 = UInt(INPUT, width=24)
        val in32 = UInt(INPUT, width=32)
        val in40 = UInt(INPUT, width=40)
        val in48 = UInt(INPUT, width=48)
        val in56 = UInt(INPUT, width=56)
        val in64 = UInt(INPUT, width=64)
        val in72 = UInt(INPUT, width=72)
        val infm1 = UInt(INPUT, width=32)
        val out = UInt(OUTPUT, width=128)
      }
      io.out := UInt(0)
      switch(io.selectIn) {
        is(UInt(0)) {io.out := io.in8}
        is(UInt(1)) {io.out := io.in16}
        is(UInt(2)) {io.out := io.in24}
        is(UInt(3)) {io.out := io.in32}
        is(UInt(4)) {io.out := io.in40}
        is(UInt(5)) {io.out := io.in48}
        is(UInt(6)) {io.out := io.in56}
        is(UInt(7)) {io.out := io.in64}
        is(UInt(8)) {io.out := io.in72}
        is(UInt(9)) {io.out := io.infm1}
      }
    }

    class VariousPokeTester(m: IOSelector) extends Tester(m) {
      case class TestVector(inSelect: Int, inValue: BigInt, expectedValue: BigInt) {}
      val testVectors = Array[TestVector](
          TestVector(0, -1, 0x0000000000000000ffL),
          TestVector(1, -1, 0x00000000000000ffffL),
          TestVector(2, -1, 0x000000000000ffffffL),
          TestVector(3, -1, 0x0000000000ffffffffL),
          TestVector(4, -1, 0x00000000ffffffffffL),
          TestVector(5, -1, 0x000000ffffffffffffL),
          TestVector(6, -1, 0x0000ffffffffffffffL),
          // The following contortion is required to prevent Scala/Java from interpreting
          // this is as "-1" and failing our expected result.
          TestVector(7, -1, (BigInt(0x0000ffffffffffffffL) << 8) |0x0000ffL),
          TestVector(8, -1, (BigInt(0x0000ffffffffffffffL) << 16)|0x00ffffL),
          TestVector(9, (java.lang.Float.floatToIntBits(-1.0f).toLong & 0x00000000ffffffffL), 0x00bf800000L)
          )
      for (tv <- testVectors) {
        tv.inSelect match {
          case 0 => poke(m.io.in8, tv.inValue)
          case 1 => poke(m.io.in16, tv.inValue)
          case 2 => poke(m.io.in24, tv.inValue)
          case 3 => poke(m.io.in32, tv.inValue)
          case 4 => poke(m.io.in40, tv.inValue)
          case 5 => poke(m.io.in48, tv.inValue)
          case 6 => poke(m.io.in56, tv.inValue)
          case 7 => poke(m.io.in64, tv.inValue)
          case 8 => poke(m.io.in72, tv.inValue)
          case 9 => poke(m.io.infm1, tv.inValue)
        }
        poke(m.io.selectIn, tv.inSelect)
        expect(m.io.out, tv.expectedValue)
      }
    }

    chiselMainTest(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
      () => Module(new IOSelector())) {m => new VariousPokeTester(m)}
  }

  /** Test poking negative numbers.
   *  This is primarily a test of the Tester and its peek/poke/expect interface.
   *
   */
  @Test def testPokeNegTests () {
    println("\ntestPokeNegTests ...")
    
    class PokeNegModule extends Module {
    
      val io = new Bundle {  
        val i_value     = UInt(INPUT, width = 64)
        val o_value     = UInt(OUTPUT, width = 64)
      }
    
      io.o_value := io.i_value
    } 
    
    class PokeNegTests(c:PokeNegModule) extends AdvTester(c){
      isTrace = true
    
      wire_poke(c.io.i_value, 0x7100a000a000a000L)
      expect(c.io.o_value, 0x7100a000a000a000L)
    
      wire_poke(c.io.i_value, 0x8100a000a000a000L)
      expect(c.io.o_value, 0x8100a000a000a000L) 
    
      wire_poke(c.io.i_value, -1L )
      expect(c.io.o_value, -1L )
    }   
    
    chiselMainTest(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
      () => Module(new PokeNegModule())) {m => new PokeNegTests(m)}
  }

  /** Test sign bits issues.
   *  Mishandling of UInts with MSB == 1 #347 
   *
   */
  @Test def testUIntMSB1 () {
    class HWAssert extends Module {
      val io = new Bundle(){
        val in  = UInt(INPUT, width = 32)
        val out = UInt(OUTPUT, width = 32)
      }
    
      val reg = Reg(next = io.in, init = UInt(0xffffffff))
    
      io.out := reg
    
      assert(reg != UInt(0xff000000), "Assertion Test")
    }
    
    class UIntMSB1Tests(c:HWAssert) extends Tester(c){
      peek(c.io.out)
    
      poke(c.io.in, 100)
      step(1)
      peek(c.io.out)
    
      poke(c.io.in, 0x0f000000)
      step(1)
      peek(c.io.out)
    
      poke(c.io.in, 0xf0000000)
      step(1)
      peek(c.io.out)
    }
    
    chiselMainTest(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
      () => Module(new HWAssert())) {m => new UIntMSB1Tests(m)}

    assertFile("TesterTest_HWAssert_1.cpp")

  }
}
