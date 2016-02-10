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
import Chisel.testers.{TesterDriver, SteppedHWIOTester, OrderedDecoupledHWIOTester}
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import TestHelpers._

object GCDCalculator {
  def computeGcdResultsAndCycles(a: Int, b: Int, depth: Int = 1): (Int, Int) = {
    if(b == 0) {
      (a, depth)
    }
    else {
      computeGcdResultsAndCycles(b, a%b, depth + 1 )
    }
  }
}

// Test the Chisel3 UnitTester interface.
class GCDDecoupledTest extends TestSuite {

 @Test def testGCDDecoupledTester() {
   println("\ntestGCDDecoupledTester ...")

  class RealGCDInput extends Bundle {
    val a = Bits(width = 16)
    val b = Bits(width = 16)
    override def cloneType = new RealGCDInput().asInstanceOf[this.type]
  }

  class RealGCD extends Module {
    val io  = new Bundle {
      val in  = Decoupled(new RealGCDInput()).flip()
      val out = Decoupled(UInt(width = 16))
    }

    val x = Reg(UInt())
    val y = Reg(UInt())
    val p = Reg(init=Bool(false))

    val ti = Reg(init=UInt(0, width = 16))
    ti := ti + UInt(1)

    io.in.ready := !p

    when (io.in.valid && !p) {
      x := io.in.bits.a
      y := io.in.bits.b
      p := Bool(true)
    }

    when (p) {
      when (x > y)  { x := y; y := x }
        .otherwise    { y := y - x }
    }

//    printf("RealGCD: ti %d  x %d y %d  in_ready %d  in_valid %d  out %d  out_ready %d  out_valid %d==============\n",
//        ti, x, y, io.in.ready, io.in.valid, io.out.bits, io.out.ready, io.out.valid)

    io.out.bits  := x
    io.out.valid := y === Bits(0) && p
    when (io.out.valid) {
      p := Bool(false)
    }
  }

  class RealGCDTests extends SteppedHWIOTester {
    val device_under_test = Module( new RealGCD )
    val c = device_under_test

    val inputs = List( (48, 32), (7, 3), (100, 10) )
    val outputs = List( 16, 1, 10)

    for( (input_1, input_2) <- inputs) {
      val (output, cycles) = GCDCalculator.computeGcdResultsAndCycles(input_1, input_2)

      poke(c.io.in.bits.a, input_1)
      poke(c.io.in.bits.b, input_2)
      poke(c.io.in.valid,  1)

      step(1)
      expect(c.io.in.ready, 1)
      poke(c.io.in.valid, 0)
      step(1)

      step(cycles-2)
      expect(c.io.out.bits, output)
    }
  }

  class DecoupledRealGCDTestHandCodedExample extends OrderedDecoupledHWIOTester {
    val device_under_test = Module(new RealGCD())
    val c = device_under_test

    val a_values = Vec(Array(UInt(12, width = 16), UInt(33, width = 16)))
    val b_values = Vec(Array(UInt(24, width = 16), UInt(24, width = 16)))

    val ti = Reg(init=UInt(0, width = 16))
    val pc = Reg(init=UInt(0, width = 16))
    val oc = Reg(init=UInt(0, width = 16))

    val in_done  = Reg(init=Bool(false))
    val out_done = Reg(init=Bool(false))

    ti := ti + UInt(1)
    when(ti >= UInt(40)) { stop() }
    when(in_done && out_done) { stop() }

    //printf("ti %d pc %d oc %d in_ready %d out_valid %d==============",
    //    ti, pc, oc, c.io.in.ready, c.io.out.valid)
    when(!in_done && c.io.in.ready) {
      //    printf(s"pc %d a %d b %d", pc, a_values(pc), b_values(pc))
      c.io.in.bits.a := a_values(pc)
      c.io.in.bits.b := b_values(pc)
      c.io.in.valid  := Bool(true)
      pc := pc + UInt(1)
      when(pc >= UInt(a_values.length)) {
        in_done := Bool(true)
      }
    }

    val c_values = Vec(Array(UInt(12, width = 16), UInt(3, width = 16)))
    c.io.out.ready := Bool(true)

    when(!out_done && c.io.out.valid) {
      printf("oc %d   got %d   expected %d", oc, c.io.out.bits, c_values(oc))
      assert(c.io.out.bits === c_values(oc), "got != expected")
      c.io.out.ready := Bool(true)
      oc := oc + UInt(1)
      when(oc >= UInt(c_values.length)) {
        out_done := Bool(true)
      }
    }
    //  finish()
    //  io_info.show_ports("".r)
  }

  class DecoupledRealGCDTests4 extends OrderedDecoupledHWIOTester {
    val device_under_test = Module(new RealGCD())
    val c = device_under_test

    for {
      i <- Array(12, 33)
      j <- Array(24, 24)
    } {
      val (gcd_value, cycles) = GCDCalculator.computeGcdResultsAndCycles(i, j)

      inputEvent(c.io.in.bits.a -> i, c.io.in.bits.b -> j)
      outputEvent(c.io.out.bits -> gcd_value)
    }
  }

  implicit val args = Array[String]("--backend", "c", "--compile", "--genHarness", "--test", "--targetDir", TestHelpers.dir.getPath.toString())
  TesterDriver.execute { () => new DecoupledRealGCDTests4 }
 }
}
