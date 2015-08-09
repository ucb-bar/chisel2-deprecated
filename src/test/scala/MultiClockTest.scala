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

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.rules.TemporaryFolder;

import Chisel._


/** This testsuite checks the generation of dot graphs.
*/
class MultiClockSuite extends TestSuite {

  /** Test Register on a different clock */
  @Test def testRegClock() {
    println("testRegClock:")
    class ClockedSubComp extends Module {
      val io = new Bundle {
        val ready = Bool(INPUT)
        val valid = Bool(OUTPUT)
      }
      val stored = Reg(next=io.ready, clock=new Clock())
      io.valid := stored
    }

    class Comp extends Module {
      val io = new Bundle {
        val data0 = Bool(INPUT)
        val data1 = Bool(INPUT)
        val result = Bool(OUTPUT)
      }
      val sub = Module(new ClockedSubComp())
      sub.io.ready := io.data0 & io.data1
      io.result := RegNext(sub.io.valid)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new Comp()))
    assertFile("MultiClockSuite_Comp_1.v")
  }
  
  @Test def testBundleCustomClock() {
    println("testBundleCustomClock:")
    class TestMultiClock2 extends Module {
      class BundleXY extends Bundle{
        override def cloneType: this.type = (new BundleXY).asInstanceOf[this.type]
        val onSignal = Bool()
      }
      
      class TestMultiClock2_subsub(clkB: Clock) extends Module {
        val io = new Bundle {
          val in = Bool(INPUT)
          val out = Bool(OUTPUT)
        }
        val r1 = Reg(outType = new BundleXY, clock = clkB)   
        r1.onSignal := io.in
      
        io.out := r1.onSignal
      }
      
      class TestMultiClock2_sub(clkA: Clock,clkB: Clock) extends Module(clkA) {
        val io = new Bundle {
          val in = Bool(INPUT)
          val out = Bool(OUTPUT)
        }
        val sub = Module(new TestMultiClock2_subsub(clkB))
        sub.io <> io
      }
    
      val io = new Bundle {
        val in = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
      val clkA = new Clock()
      val clkB = new Clock()
      val sub = Module(new TestMultiClock2_sub(clkA,clkB))
      sub.io <> io
    }

    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new TestMultiClock2()))
    assertFile("MultiClockSuite_TestMultiClock2_1.v")
  }

  @Test def testSingleReset() {
    class ClockedSubComp extends Module {
      val io = new Bundle {
        val ready = Bool(INPUT)
        val valid_A = Bool(OUTPUT)
        val valid_B = Bool(OUTPUT)
      }
      val stored_A = Reg(init=Bool(false), next=io.ready, clock=new Clock(this.reset))
      val stored_B = Reg(init=Bool(false), next=io.ready, clock=new Clock(this.reset))
      io.valid_A := stored_A
      io.valid_B := stored_B
    }

    class SingleReset extends Module {
      val io = new Bundle {
        val data     = Bool(INPUT)
        val result_A = Bool(OUTPUT)
        val result_B = Bool(OUTPUT)
      }
      val sub = Module(new ClockedSubComp())
      sub.io.ready := io.data
      io.result_A := RegNext(sub.io.valid_A)
      io.result_B := RegNext(sub.io.valid_B)
    }

    class SingleResetTests(c : SingleReset) extends Tester(c) {
      poke(c.io.data, BigInt(1))
      step(2)
      expect(c.io.result_A, BigInt(1))
      expect(c.io.result_B, BigInt(1))
      poke(c.io.data, BigInt(0))
      step(2)
      expect(c.io.result_A, BigInt(0))
      expect(c.io.result_B, BigInt(0))
    }

    launchCppTester((c: SingleReset) => new SingleResetTests(c))

    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new SingleReset()))
    assertFile("MultiClockSuite_SingleReset_1.v")
  }

}
