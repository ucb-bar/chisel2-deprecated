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

import Chisel._
import Chisel.testers.TesterDriver
import hwiotesters.SteppedHWIOTester
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import TestHelpers._

// Test the Chisel3 UnitTester interface.

class GCDUnitTest extends TestSuite {

 @Test def testGCDUnitTester() {
   println("\ntestGCDUnitTester ...")

  class GCD extends Module {
    val io = new Bundle {
      val a  = UInt(INPUT, 32)
      val b  = UInt(INPUT, 32)
      val e  = Bool(INPUT)
      val z  = UInt(OUTPUT, 32)
      val v  = Bool(OUTPUT)
    }
    val x = Reg(UInt(width = 32))
    val y = Reg(UInt(width = 32))
    when (x > y)   { x := x -% y }
    .otherwise     { y := y -% x }
    when (io.e) { x := io.a; y := io.b }
    io.z := x
    io.v := y === UInt(0)
  }

  // We need to not only compute the GCD for test verification,
  //  we need to calculate the number of cycles it will take so we can
  //  structure out step()'s and expect()'s appropriately.
  class GCDUnitTester extends SteppedHWIOTester {
    def compute_gcd(a: Int, b: Int): Tuple2[Int, Int] = {
      var x = a
      var y = b
      var depth = 1
      while(y > 0 ) {
        if (x > y) {
          x -= y
        }
        else {
          y -= x
        }
        depth += 1
      }
      return (x, depth)
    }

    // Instantiate the GCD circuit.
    val device_under_test = Module(new GCD)
    val gcd = device_under_test

    for {
      value_1 <- 4 to 8
      value_2 <- 2 to 4
    } {
      poke(gcd.io.a, value_1)
      poke(gcd.io.b, value_2)
      poke(gcd.io.e, 1)
      step(1)
      poke(gcd.io.e, 0)

      val (expected_gcd, steps) = compute_gcd(value_1, value_2)

      step(steps-1) // -1 is because we step(1) already to toggle the enable
      expect(gcd.io.z, expected_gcd)
      expect(gcd.io.v, 1 )
    }
  }

  implicit val args = Array[String]("--backend", "c", "--compile", "--genHarness", "--test", "--targetDir", TestHelpers.dir.getPath.toString())
  TesterDriver.execute { () => new GCDUnitTester }
 }
}
