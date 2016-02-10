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

import scala.util.matching.Regex
import scala.reflect.runtime.universe._
import org.scalatest.Assertions._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Ignore

import Chisel._
import FlushPrintfOutput._

object FlushPrintfOutput {
    val whiteSpaceRE = """\s""".r
    def eliminateWhiteSpace(s: String): String = whiteSpaceRE.replaceAllIn(s, "")
}

class FlushPrintfOutput extends TestSuite {

  abstract class BasePrintfModule(theWidth: Int = 4) extends Module {
    val io = new Bundle {
      val in = Decoupled(UInt(width = theWidth)).flip
      val out = Decoupled(UInt(width = theWidth))
    }
    val counterString: String
    val isFloat: Boolean
  }

  class UIntPrintfModule extends BasePrintfModule {
    val isFloat = false
    val counterString: String = "counter = %d\n"
    val counter = Reg(UInt(width = 8), init = UInt(0))
    counter := counter + UInt(1)
    printf(counterString, counter);
  }

  class FloPrintfModule extends BasePrintfModule {
    val isFloat = true
    val counterString = "counter = %e\n"
    val counter = Reg(Flo(), init = Flo(0))
    counter := counter + Flo(1)
    printf(counterString, counter);
  }

  // TODO: better way to check logs? logging is lower than tests, so it sometimes fails...
  trait FlushPrintfOutputTests extends Tests {
    val expectedOutputs = collection.mutable.ArrayBuffer[String]()
    def tests(m: BasePrintfModule) {
      for (i <- 0 until 4) {
        step(1)
        if (m.isFloat) {
          expectedOutputs += m.counterString.format(i.toFloat)
        } else {
          expectedOutputs += m.counterString.format(i)
        }
      }
      // Wait for any delayed output to accumulate
      Thread.sleep(200)
      val outputs = printfs
      assertResult(true, "incorrect number of outputs - %s".format(outputs)) {
        outputs.length == expectedOutputs.length
      }
      (outputs zip expectedOutputs) foreach {case (output, expected) =>
        assertResult(true, "incorrect output - %s".format(output)) {
          eliminateWhiteSpace(output) == eliminateWhiteSpace(expected)
        }
      }
    }
  }

  @Test def testFlushUIntPrintfOutput() {
    println("\ntestFlushUIntPrintfOutput ...")

    class FlushPrintfOutputTester(m: UIntPrintfModule) extends Tester(m) with FlushPrintfOutputTests {
      tests(m)
    }
    launchCppTester((m: UIntPrintfModule) => new FlushPrintfOutputTester(m))
  }

  @Test def testFlushFlotPrintfOutput() {
    println("\ntestFlushFloPrintfOutput ...")

    class FlushPrintfOutputTester(m: FloPrintfModule) extends Tester(m) with FlushPrintfOutputTests {
      tests(m)
    }
    launchCppTester((m: FloPrintfModule) => new FlushPrintfOutputTester(m))
  }

}
