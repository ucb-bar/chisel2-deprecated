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

import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import org.scalatest._

import Chisel._

class AssertSuite extends TestSuite with ShouldMatchers {
  @Test def testAssert() {
    println("\ntestAssert...")
    class AssertModule extends Module {
      class IO extends Bundle {
        val in0 = SInt(INPUT, width=32)
        val out0 = SInt(OUTPUT, width=32)
      }
      val io = new IO()
      io.in0.setIsTypeNode
    }

    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString())
    intercept[AssertionError] {
      // This should fail since io.out does not have a default value.
      chiselMain(testArgs, () => Module(new AssertModule))
    }
    assertTrue(ChiselError.hasErrors)
  }

  @Test def testRTAssert() {
    println("\ntestRTAssert...")
    val assertMessage = "io.dataIn == UInt(8)"
    class mkAssert extends Module{
      val io = new Bundle{
        val dataIn = UInt(INPUT,8)
        val dataOut = UInt(OUTPUT,width=3)
      }

      io.dataOut := OHToUInt(io.dataIn)
      assert(io.dataIn =/= UInt(8), assertMessage)
    }

    class TBAssert(c: mkAssert) extends Tester(c) {
      step(1)
      poke(c.io.dataIn, 32)
      peek(c.io.dataOut)
      step(1)
      poke(c.io.dataIn, 16)
      peek(c.io.dataOut)
      step(1)
      poke(c.io.dataIn,8)
      peek(c.io.dataOut)
      step(1)
      poke(c.io.dataIn, 0)
      peek(c.io.dataOut)
    }

    val swArgs = Array("--backend", "c", "--genHarness","--targetDir","cpp","--compile","--test","--debug"/*, "--assertWarn"*/)
    val hwArgs = Array("--backend", "v","--genHarness","--targetDir","rtl")

    "sw" match {
      case "sw" => {
        val ex = intercept[TestApplicationException] {
          chiselMainTest(swArgs, () => Module(new mkAssert)){ c => new TBAssert(c)}
        }
        // Verify the exception message contains our assertion message.
        ex.getMessage should endWith (assertMessage)
      }
      case "hw" => chiselMain(hwArgs, () => Module(new mkAssert))
    }
  }
}
