/*
 Copyright (c) 2011, 2012, 2013, 2014, 2015 The Regents of the University of
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
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._

class FlushPrintfOutput extends TestSuite {
  @Test def testFlushPrintfOutput() {
    println("\ntestFlushPrintfOutput ...")
    val whiteSpaceRE = """\s""".r
    def eliminateWhiteSpace(s: String): String = whiteSpaceRE.replaceAllIn(s, "")

    class PrintfModule extends Module {
      val io = new DecoupledUIntIO
      val counter = Reg(UInt(width = 8), init = UInt(0))
      val counterString = "counter = %d\n"
      counter := counter + UInt(1)
      printf(counterString, counter);
    }

    class FlushPrintfOutputTests(m: PrintfModule) extends Tester(m) {
      for (i <- 0 until 4) {
        step(1)
        // Fetch the output printed on stdout by the test.
        val printfOutput = testOutputString
        val expectedString = m.counterString.format(i)
        assertTrue("incorrect output - %s".format(printfOutput), eliminateWhiteSpace(printfOutput) == eliminateWhiteSpace(expectedString))
      }
    }
    launchCppTester((m: PrintfModule) => new FlushPrintfOutputTests(m))
  }
}
