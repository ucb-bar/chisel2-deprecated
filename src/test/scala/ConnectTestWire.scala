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
import org.junit.Ignore

import Chisel._

/** This testsuite checks interaction of component class
  and runtime hierarchies.
*/
/** The `CWTRegStatus` class is defined at the top level so we can clone it:
  *  automatically construct it without requiring parameters.
  */
class CWTRegStatus extends Bundle {
  val im0 = UInt(width = 2)
  val im1 = UInt(width = 2)
}

// Generic wiring module used in following tests.
case class GenWiring[SB <: Bundle](val newStatusInstance: () => SB) extends Module {
  val io = new Bundle {
    val wen   = Bool(INPUT)
    val wdata = UInt(INPUT, 4)
    val status = newStatusInstance().asOutput
  }
  val wire_status = Reg(newStatusInstance())
  when (io.wen) {
    wire_status := newStatusInstance().fromBits(io.wdata)
  }
  io.status := wire_status

  // Since we're parameterized, we need a clone method.
  def cloneType = {
    new GenWiring[SB](newStatusInstance).asInstanceOf[this.type]
  }
}

class ConnectWireSuite extends TestSuite {
  // Test out wiring to a shim made via a scala def
  @Test def testWireShimConnections() {
    class UsesShim extends Module {
      val io = new DecoupledUIntIO
      def makeShim(in: DecoupledIO[UInt]): DecoupledIO[UInt] = {
        val out = Wire(Decoupled(UInt()))
        out.bits := in.bits + UInt(1)
        out.valid := in.valid
        in.ready := out.ready
        out
      }
      val s = makeShim(io.in)
      io.out.bits := s.bits
      io.out.valid := s.valid
      s.ready := io.out.ready
    }
    class UsesShimParent extends Module {
      val io = new DecoupledUIntIO
      val us = Module(new UsesShim)
      us.io.in <> io.in
      io.out <> us.io.out
    }
    class ShimConnectionsTests(m: UsesShimParent) extends Tester(m) {
      (0 until 4).map { i =>
        poke(m.io.in.bits,   i)
        poke(m.io.in.valid,  int(true))
        poke(m.io.out.ready, int(true))
        step(1)
        expect(m.io.out.valid, int(true))
        expect(m.io.in.ready,  int(true))
        expect(m.io.out.bits,  i + 1)
      }
    }
    launchCppTester((m: UsesShimParent) => new ShimConnectionsTests(m))
  }

  /** Test failure using a Wire with no default in a when(). */
  @Ignore @Test def testWireHookNoDefaultSpecifiedForWire() {
    try {
      class WireHookNoDefaultSpecifiedForWire extends Module {
        case class WireStatus() extends Bundle {
          val im0 = Wire(UInt(width = 2))
          val im1 = Wire(UInt(width = 2))
        }

        val io = new Bundle {
          val wen   = Bool(INPUT)
          val wdata = UInt(INPUT, 4)
          val status = new WireStatus().asOutput
        }
        val subModule = new GenWiring[WireStatus](WireStatus)
        io <> subModule.io
      }
      chiselMain(Array[String]("--backend", "c", "--compile",
        "--targetDir", dir.getPath.toString()),
        () => Module(new WireHookNoDefaultSpecifiedForWire()))
    } catch {
      case _ : Throwable => ;
    }
    assertTrue(ChiselError.hasErrors)
  }

  /** Test failure for reassignment to Wire in a when(). */
  @Ignore @Test def testWireHookReassignmentToWire() {
    try {
      class WireHookReassignmentToWire extends Module {
        case class WireStatus() extends Bundle {
          val im0 = UInt(0, 2)
          val im1 = UInt(1, 2)
        }

        val io = new Bundle {
          val wen   = Bool(INPUT)
          val wdata = UInt(INPUT, 4)
          val status = new WireStatus().asOutput
        }
        val subModule = new GenWiring[WireStatus](WireStatus)
        io <> subModule.io
      }
      chiselMain(Array[String]("--backend", "c", "--compile",
        "--targetDir", dir.getPath.toString()),
        () => Module(new WireHookReassignmentToWire()))
    } catch {
      case _ : Throwable => ;
    }
    assertTrue(ChiselError.hasErrors)
  }

  @Test def testRegisterHook() {
    try {
      class RegHook extends Module {
        val io = new Bundle {
          val wen   = Bool(INPUT)
          val wdata = UInt(INPUT, 4)
          val status = new CWTRegStatus().asOutput
        }
        val reg_status = Reg(new CWTRegStatus)
        when (io.wen) {
          reg_status := new CWTRegStatus().fromBits(io.wdata)
        }
        io.status := reg_status
      }

      class RegisterHookTests(m: RegHook) extends Tester(m) {
        List(1,     2,     4,     6,     8,     12,    15,   15).zip(
        List(false, true,  true,  false, true,  false, true, false)).zip(
        List((0,0), (0,2), (1,0), (1,0), (2,0), (2,0), (3,3), (3,3))).map {
          case ((in, en), (im0, im1)) =>
            poke(m.io.wdata, in)
            poke(m.io.wen,   int(en))
            step(1)
            expect(m.io.status.im0, im0)
            expect(m.io.status.im1, im1)
        }
      }
      launchCppTester((m: RegHook) => new RegisterHookTests(m))
    } catch {
      case e : Throwable => {
        val cause = e.getCause()
        println("exception: %s".format(cause))
      }
    }
  }

  /** Test hooking-up Wires. */
  @Ignore @Test def testWireHook() {
    try {

      class WireHook extends Module {
        val io = new Bundle {
          val wen   = Bool(INPUT)
          val wdata = UInt(INPUT, 4)
          val status = new CWTRegStatus().asOutput
        }
        val subModule = Module(new GenWiring[CWTRegStatus](() => new CWTRegStatus) )
        val subModuleIOWire = subModule.io
        if (false) {
          io <> subModule.io
        } else if (true) {
          io <> subModule.io
        } else {
          subModule.io.wen := io.wen.asOutput
          subModule.io.wdata := io.wdata.asOutput
          io.status := subModule.io.status.asInput
        }
      }


      class WireHookTests(m: WireHook) extends Tester(m) {
        List(1,     2,     4,     6,     8,     12,    15,   15).zip(
        List(false, true,  true,  false, true,  false, true, false)).zip(
        List((0,0), (0,2), (1,0), (1,0), (2,0), (2,0), (3,3), (3,3))).map {
          case ((in, en), (im0, im1)) =>
            poke(m.io.wdata, in)
            poke(m.io.wen,   int(en))
            step(1)
            expect(m.io.status.im0, im0)
            expect(m.io.status.im1, im1)
        }
      }
      launchCppTester((m: WireHook) => new WireHookTests(m))
    } catch {
      case e : Throwable => {
        val cause = e.getCause()
        fail("exception: %s".format(cause))
      }
    }
  }

  @Test def testWireVecInput() {
    class VecInput extends Module {
      val io = new Bundle {
        val in = Vec(2,  UInt(INPUT, 8) )
        val out0 = UInt(OUTPUT, 8)
        val out1 = UInt(OUTPUT, 8)
      }
      io.out0 := io.in(0)
      io.out1 := io.in(1)
    }

    class VecInputTests(c: VecInput) extends Tester(c) {
      poke(c.io.in, Array[BigInt](0, 1))
      step(1)
      expect(c.io.out0, 0)
      expect(c.io.out1, 1)
    }

    launchCppTester((m: VecInput) => new VecInputTests(m))
  }

  @Test def testVecWireOutput() {
    class VecOutput extends Module {
      val io = new Bundle {
        val in0 = UInt(INPUT, 8)
        val in1 = UInt(INPUT, 8)
        val out = Vec(2,  UInt(OUTPUT, 8) )
      }

      io.out(0) := io.in0
      io.out(1) := io.in1
    }

    class VecOutputTests(c: VecOutput) extends Tester(c) {
      poke(c.io.in0, 0)
      poke(c.io.in1, 1)
      step(1)
      expect(c.io.out, Array[BigInt](0, 1))
    }

    launchCppTester((m: VecOutput) => new VecOutputTests(m))
  }

  // More extensive test that submodule bindings are connected to appropriate logic
  @Test def testWireAddBindings() {
    class CrossingBlock extends Module {
      val io = new Bundle {
        val i1 = UInt(width=8).asInput
        val i2 = UInt(width=8).asInput
        val o1 = UInt(width=8).asOutput
        val o2 = UInt(width=8).asOutput
      }
      io.o2 := io.o1 + io.i2
      io.o1 := io.i1
    }
    class BindingTestInternal extends Module {
      val io = new Bundle {
        val in1 = UInt(width=8).asInput
        val in2 = UInt(width=8).asInput
        val in3 = UInt(width=8).asInput
        val in4 = UInt(width=8).asInput
        val out1 = UInt(width=8).asOutput
        val out2 = UInt(width=8).asOutput
        val out3 = UInt(width=8).asOutput
        val out4 = UInt(width=8).asOutput
        val out5 = UInt(width=8).asOutput
        val out6 = UInt(width=8).asOutput
        val out7 = UInt(width=8).asOutput
        val out8 = UInt(width=8).asOutput
        val out9 = UInt(width=8).asOutput
      }
      val cb5 = Module(new CrossingBlock)
      val cb4 = Module(new CrossingBlock)
      val cb3 = Module(new CrossingBlock)
      val cb2 = Module(new CrossingBlock)
      val cb1 = Module(new CrossingBlock)

      cb1.io.i1 := io.in1
      cb1.io.i2 := io.in2
      io.out1 := cb1.io.o1
      io.out2 := cb1.io.i2

      cb2.io.i1 := cb1.io.o2
      cb2.io.i2 := io.out1
      io.out3 := cb2.io.o1
      io.out4 := cb2.io.o2

      cb3.io.i1 := cb1.io.i1 + UInt(1)
      cb3.io.i2 := cb2.io.i1
      io.out5 := cb3.io.o1 + cb3.io.o2 + io.out4

      cb4.io.i1 := io.in3
      cb4.io.i2 := cb4.io.i1
      io.out6 := cb4.io.o1
      io.out7 := cb4.io.o2

      cb5.io.i1 := io.in4
      cb5.io.i2 := cb5.io.o1
      io.out8   := cb5.io.o2

      io.out9 := io.out7
    }

    class BindingTest extends Module {
      val io = new Bundle {
        val in1 = UInt(width=8).asInput
        val in2 = UInt(width=8).asInput
        val in3 = UInt(width=8).asInput
        val in4 = UInt(width=8).asInput
        val out1 = UInt(width=8).asOutput
        val out2 = UInt(width=8).asOutput
        val out3 = UInt(width=8).asOutput
        val out4 = UInt(width=8).asOutput
        val out5 = UInt(width=8).asOutput
        val out6 = UInt(width=8).asOutput
        val out7 = UInt(width=8).asOutput
        val out8 = UInt(width=8).asOutput
        val out9 = UInt(width=8).asOutput
      }
      val myTest = Module(new BindingTestInternal)
      myTest.io <> io
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new BindingTest()))

    assertFile("ConnectWireSuite_BindingTest_1.v")
  }
}
