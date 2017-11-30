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

//package ChiselTests
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._


class OuterSuite extends TestSuite {
  @Test def testOuterSuite() {
    println("\ntestOuterSuite...")

    class Inner extends Module {
      val io = new Bundle {
        val in  = Bits(INPUT, 8)
        val out = Bits(OUTPUT, 8)
      }
      io.out := io.in + Bits(1)
    }

    class Outer extends Module {
      val io = new Bundle {
        val in  = Bits(INPUT, 8)
        val out = Bits(OUTPUT, 8)
      }
      // val c = Module(new Inner)
      val c = Array(Module(new Inner))
      // val w = Wire(Bits(NO_DIR, 8))
      // w := io.in
      c(0).io.in := io.in
      io.out  := (c(0).io.out * Bits(2))(7,0)
    }

    class OuterTester(c: Outer) extends Tester(c) {
      for (t <- 0 until 16) {
        val test_in = rnd.nextInt(256)
        poke(c.io.in, test_in)
        step(1)
        expect(c.io.out, ((test_in + 1) * 2)&255)
      }
    }

    launchCppTester((c: Outer) => new OuterTester(c))
  }
}

