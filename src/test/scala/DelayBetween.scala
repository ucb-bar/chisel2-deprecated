/*
 Copyright (c) 2011 - 2016 The Regents of the University of
 Sydney. All Rights Reserved.  Redistribution and use in
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
import scala.util.Random
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._

/** This test suite tests measurement of Delays with DelayBetween
  */
class DelayBetweenSuite extends TestSuite {

  @Test def testZero() {
    class ZeroDelay extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 4)
        val out = UInt(OUTPUT, 4)
      }
      io.out := io.in
    }
    val myInst = Module(new ZeroDelay)

    val delayDepth = DelayBetween(myInst.io.in, myInst.io.out)
    assertTrue("Should only find exactly one path", delayDepth.length == 1)
    assertTrue("Delay should be 0", delayDepth(0) == 0)

    val delayShort = DelayBetween.findShortest(myInst.io.in, myInst.io.out)
    assertTrue("Shortest Delay should be 0", delayShort == 0)
  }

  @Test def testOne() {
    class OneDelay extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 4)
        val out = UInt(OUTPUT, 4)
      }
      io.out := RegNext(io.in)
    }
    val myInst = Module(new OneDelay)

    val delayDepth = DelayBetween(myInst.io.in, myInst.io.out)
    assertTrue("Should only find exactly one path", delayDepth.length == 1)
    assertTrue("Delay should be 1", delayDepth(0) == 1)

    val delayBreadth = DelayBetween(myInst.io.in, myInst.io.out, true)
    assertTrue("Should only find exactly one path", delayBreadth.length == 1)
    assertTrue("Delay should be 1", delayBreadth(0) == 1)

    val delayShort = DelayBetween.findShortest(myInst.io.in, myInst.io.out)
    assertTrue("Shortest Delay should be 1", delayShort == 1)
  }

  @Test def testInf() {
    class InfDelay extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 4)
        val out = UInt(OUTPUT, 4)
      }
      io.out := UInt(0, 4)
    }
    val myInst = Module(new InfDelay)

    val delayDepth = DelayBetween(myInst.io.in, myInst.io.out)
    assertTrue("Should only find no paths", delayDepth.length == 0)

    val delayBreadth = DelayBetween(myInst.io.in, myInst.io.out, true)
    assertTrue("Should only find no paths", delayBreadth.length == 0)

    val delayShort = DelayBetween.findShortest(myInst.io.in, myInst.io.out)
    assertTrue("No Path: Shortest Delay should be -1", delayShort == -1)
  }

  @Test def testMultiple() {
    class MultDelay extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 4)
        val en = Bool(INPUT)
        val out = UInt(OUTPUT, 4)
      }
      val reg1 = RegNext(io.in)
      io.out := Mux(io.en, reg1, io.in)
    }
    val myInst = Module(new MultDelay)

    val delayDepth = DelayBetween(myInst.io.in, myInst.io.out)
    assertTrue("Should only find exactly two paths", delayDepth.length == 2)
    assertTrue("Delay should contain 0 and 1", delayDepth.contains(0) && delayDepth.contains(1) )

    val delayBreadth = DelayBetween(myInst.io.in, myInst.io.out, true)
    assertTrue("Should only find exactly two paths", delayBreadth.length == 2)
    assertTrue("Delay should contain 0 and 1", delayBreadth.contains(0) && delayBreadth.contains(1))

    val delayShort = DelayBetween.findShortest(myInst.io.in, myInst.io.out)
    assertTrue("Shortest Delay should be 0", delayShort == 0)
  }

  @Test def testBundle() {
    class BundleDelay extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 4)
        val en = Bool(INPUT)
        val out = UInt(OUTPUT, 4)
      }
      val RegMod = Module(new RegModule()).io
      RegMod.in := Reg(next=io.in)
      io.out := RegMod.out
    }

    class RegModule extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 4)
        val out = UInt(OUTPUT, 4)
      }
      io.out := Reg(next=io.in)
    }

    val myInst = Module(new BundleDelay)

    val delayDepth = DelayBetween(myInst.io.in, myInst.io.out)
    assertTrue("Should only find exactly one path", delayDepth.length == 1)
    assertTrue("Delay should contain 2 ", delayDepth.contains(2))

    val delayBreadth = DelayBetween(myInst.io.in, myInst.io.out, true)
    assertTrue("Should only find exactly one paths", delayBreadth.length == 1)
    assertTrue("Delay should contain 2", delayBreadth.contains(2))

    val delayShort = DelayBetween.findShortest(myInst.io.in, myInst.io.out)
    assertTrue("Shortest Delay should be 2", delayShort == 2)
  }

  @Test def testRegLoop() {
    /** This test has a Delay loop in it
      * The Loop should find that it is back at the same node and terminate
      */
    class RegLoopDelay extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 4)
        val en = Bool(INPUT)
        val out = UInt(OUTPUT, 4)
      }
      val reg1 = RegNext(io.in)
      val reg2 = RegNext(reg1)
      val reg3 = RegNext(Mux(io.en, reg1, reg2))
      reg2 := reg3 // override for a logical loop
      io.out := reg3
    }
    val myInst = Module(new RegLoopDelay)

    val delayDepth = DelayBetween(myInst.io.in, myInst.io.out)
    assertTrue("Should only find exactly one path", delayDepth.length == 1)
    assertTrue("Delay should contain 2", delayDepth.contains(2) )

    val delayBreadth = DelayBetween(myInst.io.in, myInst.io.out, true)
    assertTrue("Should only find exactly one path", delayBreadth.length == 1)
    assertTrue("Delay should contain 2", delayBreadth.contains(2))

    val delayShort = DelayBetween.findShortest(myInst.io.in, myInst.io.out)
    assertTrue("Shortest Delay should be 2", delayShort == 2)
  }

}
