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

import Chisel._

class BitPatSuite extends TestSuite {
  // BitPat ? literals
  @Test def testBitPat() {
    println("\ntestBitPat...")
    class BitPatModule extends Module {
      val io = new Bundle {
        val in = UInt(INPUT,4)
        val out = Bool(OUTPUT)
      }
      io.out := Bool(false)
      switch(io.in) {
        is(UInt(0)) { io.out := Bool(true) }
        is(BitPat("b???1")) { io.out := Bool(true) }
      }
    }

    class BitPatModuleTests(m: BitPatModule) extends Tester(m) {
      (0 until 8).map { i =>
        poke(m.io.in, i)
        step(1)
        expect(m.io.out, if(i == 0) (1) else (i % 2))
      }
    }

    launchCppTester((m: BitPatModule) => new BitPatModuleTests(m))
  }

  @Test def testBitPatBool() {
    println("\ntestBitPatBool...")
    class BitPatBoolModule extends Module {
      val io = new Bundle {
        val in = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
      io.out := Bool(false)
      val testDC = Bool.DC
      val testTrue = Bool(true)
      switch(io.in) {
        is(testDC) { io.out := Bool(true) }
      }
    }

    class BitPatBoolModuleTests(m: BitPatBoolModule) extends Tester(m) {
      (0 until 8).map { i =>
        poke(m.io.in, i)
        step(1)
        expect(m.io.out, 1)
      }
    }

    launchCppTester((m: BitPatBoolModule) => new BitPatBoolModuleTests(m))
  }
}
