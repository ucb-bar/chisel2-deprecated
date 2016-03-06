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

import scala.collection.mutable.ArrayBuffer
import org.junit.Assert._
import org.junit.Test

import Chisel._

/** This testsuite checks features dealing with debugging Chisel code.
*/
class VerifSuite extends TestSuite {

  @Test def testAssertCpp() {
    println("testAssertCpp:")
    class CppAssertComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 8)
        val y = UInt(INPUT, 8)
        val z = UInt(OUTPUT)
      }
      assert(Bool(io.x == io.y), "failure")
      io.z := Cat(io.x, io.y)
    }

    chiselMain(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString()),
      () => Module(new CppAssertComp()))
    assertFile("VerifSuite_CppAssertComp_1.cpp")
  }

  @Test def testAssertVerilog() {
    println("testAssertVerilog:")
    class VerilogAssertComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 8)
        val y = UInt(INPUT, 8)
        val z = UInt(OUTPUT)
      }
      assert(Bool(io.x == io.y), "failure")
      io.z := Cat(io.x, io.y)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new VerilogAssertComp()))
    assertFile("VerifSuite_VerilogAssertComp_1.v")
  }

  @Test def testPrintfCpp() {
    println("testPrintfCpp:")
    class CppPrintfComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 8)
        val y = UInt(INPUT, 8)
        val z = UInt(OUTPUT)
      }
      printf("display %x %x", io.x, io.y)
      io.z := Cat(io.x, io.y)
    }

    chiselMain(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString()),
      () => Module(new CppPrintfComp()))
    assertFile("VerifSuite_CppPrintfComp_1.cpp")
    assertFile("VerifSuite_CppPrintfComp_1.h")
  }

  // test that printf of bundles are flagged
  @Test def testPrintfBundle() {
    println("testPrintfBundle:")
    try {
    class PrintfBundle extends Module {
      val io = new Bundle {
        val in  = UInt(INPUT,  4)
        val out = UInt(OUTPUT, 4)
      }
      printf("IO = %x", io)
      io.out := io.in + UInt(1)
    }
    chiselMain(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString()),
      () => Module(new PrintfBundle()))
    } catch {
      case _ : Throwable => ;
    }
    assertTrue(ChiselError.hasErrors);
  }

  @Test def testPrintfVerilog() {
    println("testPrintfVerilog:")
    class VerilogPrintfComp extends Module {
      val io = new Bundle {
        val x = UInt(INPUT, 8)
        val y = UInt(INPUT, 8)
        val z = UInt(OUTPUT)
      }

      val tsc_reg = Reg(init=UInt(0, width=32))
      tsc_reg := tsc_reg + UInt(1/*, width=32*/)

      printf("Cyc= %d io: %x %x", tsc_reg(31,0), io.x, io.y)
      io.z := Cat(io.x, io.y)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new VerilogPrintfComp()))
    assertFile("VerifSuite_VerilogPrintfComp_1.v")
  }

  @Test def testPrintfVerilogNUL() {
    println("testPrintfVerilogNUL:")
    class VerilogPrintfNULComp extends Module {
      val io = new Bundle {
        val in = Bool(INPUT)
      }

      printf("%b\n", io.in)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new VerilogPrintfNULComp()))
    assertFile("VerifSuite_VerilogPrintfNULComp_1.v")
  }

  class AssertTestModule extends Module {
    val io = new QueueIO(UInt(width=4), 2)
    io.deq <> Queue(io.enq, 2)
    assert(!(io.deq.ready && !io.deq.valid), "pop from an empty queue")
  }

  trait AssertTesterCommon extends Tests {
    val values = Vector.fill(2)(rnd.nextInt(1 << 4))
    def queue(c: AssertTestModule) {
      for (v <- values) {
        poke(c.io.enq.bits, v)
        poke(c.io.enq.valid, 1)
        step(1)
      }
      poke(c.io.enq.valid, 0)
      expect(c.io.enq.ready, 0)
      for (v <- values) {
        expect(c.io.deq.bits, v)
        expect(c.io.deq.valid, 1)
        poke(c.io.deq.ready, 1)
        step(1)
      }
      expect(c.io.deq.valid, 0)
    }
  }

  class AssertNoFireTester(c: AssertTestModule) extends Tester(c) with AssertTesterCommon {
    // Asserts shouldn't fire in this test
    queue(c)
  }

  class AssertFireTester(c: AssertTestModule) extends Tester(c) with AssertTesterCommon {
    queue(c)
    // Asserts should fire here
    step(1)
  }

  @Test def testAssertNoFireCpp {
    println("\ntestAssertNoFireCpp ...")
    launchCppTester((c: AssertTestModule) => new AssertNoFireTester(c))
  }

  @Test def testAssertNoFireVerilog {
    println("\ntestAssertNoFireVerilog ...")
    if (!Driver.isVCSAvailable) {
      assert(true, "vcs unavailable - skipping testPokeWide")
    } else {
      launchVerilogTester((c: AssertTestModule) => new AssertNoFireTester(c))
    }
  }

  @Test def testAssertFireCpp {
    println("\ntestAssertFireCpp ...")
    var hasException = false
    try {
      launchCppTester((c: AssertTestModule) => new AssertFireTester(c))
    } catch {
      case e: TestApplicationException => hasException = true
      case _: Throwable =>
    }
    assertTrue(hasException)
  }

  @Test def testAssertFireVerilog {
    println("\ntestAssertFireVerilog ...")
    if (!Driver.isVCSAvailable) {
      assert(true, "vcs unavailable - skipping testPokeWide")
    } else {
      var hasException = false
      try {
        launchVerilogTester((c: AssertTestModule) => new AssertFireTester(c))
      } catch {
        case e: TestApplicationException => hasException = true
        case _: Throwable =>
      }
      assertTrue(hasException)
    }
  }
}
