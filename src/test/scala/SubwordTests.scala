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
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

class SubwordSuite extends TestSuite {
  class SubwordModule extends Module {
    val w = 8
    val io = new Bundle {
      val in = UInt(INPUT, w)
      val out = UInt(OUTPUT, w)
    }
    io.out := UInt(0)
    for (i <- 0 until w/2) {
      io.out(i) := io.in(i)
    }
  }

  class SubwordTester(c: SubwordModule) extends Tester(c) {
    val in = rnd.nextInt(1 << c.w)
    poke(c.io.in, in)
    expect(c.io.out, in & ((1 << (c.w/2))-1))
  }

  @Test def testSubwordCpp() {
    println("\ntestSubwordCpp ...")
    launchCppTester((c: SubwordModule) => new SubwordTester(c))
  }

  @Test def testSubwordVerilog() {
    println("\ntestSubwordVerilog ...")
    if (!Driver.isVCSAvailable) {
      assert(true, "vcs unavailable - skipping testSubwordVerilog")
    } else {
      launchVerilogTester((c: SubwordModule) => new SubwordTester(c))
    }
  }


  class SubwordModule2 extends Module {
    val w = 4
    val io = new Bundle {
      val out_initial = UInt(INPUT, w)
      val in = Bool(INPUT)
      val out = UInt(OUTPUT, w)
    }

    io.out    := io.out_initial
    io.out(0) := io.in
  }

  class SubwordTester2(c: SubwordModule2) extends Tester(c) {
    for (
      in          <- Array(0, 1);
      out_initial <- 0 until 1 << c.w
    )
    {
      poke(c.io.in, in)
      poke(c.io.out_initial, out_initial)
      var expected = out_initial
      if (in == 1) {
        // Set the least significant bit
        expected |= 0x0001
      } else {
        // Clear the least significant bit
        expected &= 0xFFFE
      }
      expect(c.io.out, expected)
    }
  }

  @Test def testSubwordCpp2() {
    println("\ntestSubwordCpp2 ...")
    launchCppTester((c: SubwordModule2) => new SubwordTester2(c))
  }
}
