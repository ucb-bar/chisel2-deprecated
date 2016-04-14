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

class RegStatus extends Bundle {
  val im0 = UInt(width = 2)
  val im1 = UInt(width = 2)
}

/** This testsuite checks interaction of component class
  and runtime hierarchies.
*/
class ConnectSuite extends TestSuite {

  // Test out wiring to a shim made via a scala def
  @Test def testShimConnections() {
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

  // Test different forms of reset propagation
  @Test def testResetConnections() {
    class UsesReset(resetSignal: Bool = null) extends Module(_reset = resetSignal) {
      val io = new BoolIO
      val q = Module(new Queue(Bool(), 1))
      q.io.enq.valid := Bool(true)
      q.io.enq.bits := io.in
      q.io.deq.ready := Bool(true)
      io.out := q.io.deq.bits || reset
    }
    class SuppliesResets extends Module {
      val io = new BoolIO
      val delayed = Reg(next=this.reset)
      val a0 = Module(new UsesReset(this.reset))
      val a1 = Module(new UsesReset(delayed))
      val a2 = Module(new UsesReset(Reg(next=this.reset)))
      a0.io.in := io.in
      a1.io.in := io.in
      a2.io.in := io.in
      io.out := a0.io.out || a1.io.out || a2.io.out || delayed
    }
    class SuppliesResetsParent extends Module {
      val io = new BoolIO
      val srs = Module(new SuppliesResets)
      srs.io.in := io.in
      io.out := srs.io.out
    }
    class SuppliesResetsTests(m: SuppliesResetsParent) extends Tester(m) {
      List(true,false,false,false,false,false).zip(
      List(true, true, false,false,false,false)).map {
        case (i, o) =>
          poke(m.io.in,    int(i))
          step(1)
          expect(m.io.out, int(o))
      }
    }
    launchCppTester((m: SuppliesResetsParent) => new SuppliesResetsTests(m))
  }

  // Connect module hierarchy with unrelated classes
  @Test def testUnrelatedSubmodules() {
    class A extends Module {
      val io = new UIntIO
      io.out := io.in
    }
    class B extends Module {
      val io = new UIntIO
      val a = Module(new A)
      a.io.in := io.in
      io.out := a.io.out
    }
    class Unrelated extends Module {
      val io = new UIntIO
      val b = Module(new B)
      b.io.in := io.in
      io.out := b.io.out
    }
    class UnrelatedSubmodulesTests(m: Unrelated) extends Tester(m) {
      (0 until 4).map { i =>
        poke(m.io.in, i)
        step(1)
        expect(m.io.out, i)
      }
    }
    launchCppTester((m: Unrelated) => new UnrelatedSubmodulesTests(m))
  }

  // Test multiple instantiations of single submodule
  @Test def testLogicBtwInstances() {
    class A extends Module {
      val io = new UIntIO
      io.out := io.in
    }
    class LogicBtwInstances extends Module {
      val io = new UIntIO
      val a1 = Module(new A)
      a1.io.in := io.in
      val x = Reg(next = a1.io.out)
      val a2 = Module(new A)
      a2.io.in := x
      io.out := a2.io.out
    }
    class LogicBtwInstancesTests(m: LogicBtwInstances) extends Tester(m) {
      (0 until 4).map { i =>
        poke(m.io.in, i)
        step(1)
        expect(m.io.out, if(i == 0) 0 else i)
      }
    }
    launchCppTester((m: LogicBtwInstances) => new LogicBtwInstancesTests(m))
  }

  /** Instantiate a component of the same class (*A*) at two levels
    of a component class hierarchy (*B* < *C*). */
  @Test def testOneInstancePerRelation() {
    class A extends Module {
      val io = new UIntIO
      io.out := io.in
    }
    class B extends Module {
      val io = new UIntIO
      val aInB = Module(new A)
      aInB.io.in := io.in
      io.out := aInB.io.out
    }
    class C extends B {
      val aInC = Module(new A)
      aInC.io.in := io.in
      io.out := aInC.io.out | aInB.io.out
    }
    class OneInstancePerRelationTests(m: C) extends Tester(m) {
      (0 until 4).map { i =>
        poke(m.io.in, i)
        step(1)
        expect(m.io.out, i)
      }
    }
    launchCppTester((m: C) => new OneInstancePerRelationTests(m))
  }

  /** Instantiate a component superclass inside a component */
  @Test def testInstanceSuperclass() {
    class A extends Module {
      val io = new UIntIO
      io.out := io.in
    }
    class B extends A {
      val aInB = Module(new A)
      aInB.io.in := io.in
      io.out := aInB.io.out
    }
    class InstanceSuperclassTests(m: B) extends Tester(m) {
      (0 until 4).map { i =>
        poke(m.io.in, i)
        step(1)
        expect(m.io.out, i)
      }
    }
    launchCppTester((m: B) => new InstanceSuperclassTests(m))
  }

  /** Test hooking-up Registers. */
  @Test def testRegisterHook() {
    class A extends Module {
      val io = new Bundle {
        val wen   = Bool(INPUT)
        val wdata = UInt(INPUT, 4)
        val status = new RegStatus().asOutput
      }
      val reg_status = Reg(new RegStatus, init = new RegStatus().fromBits(UInt(0)))
      when (io.wen) {
        reg_status := new RegStatus().fromBits(io.wdata)
      }
      io.status := reg_status
    }
    class RegisterHookTests(m: A) extends Tester(m) {
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
    launchCppTester((m: A) => new RegisterHookTests(m))
  }

  // tests flagging bad cross module references
  @Test def testCrossModuleReferences() {
    try {
      class ForeignMod extends Module {
        val io = new Bundle {
          val input  = UInt(INPUT, width = 8)
          val output = UInt(OUTPUT, width = 8)
        }
        val add = io.input + io.input
        io.output := add + UInt(1)
      }

      class ForeignRef extends Module {
        val io = new Bundle {
          val input  = Bits(INPUT, width = 8)
          val output = Bits(OUTPUT, width = 8)
        }
        val foreign = Module(new ForeignMod)
        io.output := io.input + foreign.add
      }

      chiselMain(Array[String]("--v",
        "--targetDir", dir.getPath.toString()),
        () => Module(new ForeignRef()))

    } catch {
      case _ : Throwable => ;
    }
    assertTrue(ChiselError.hasErrors);
  }

  @Test def testVecInput() {
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

  @Test def testVecOutput() {
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

  /** Test repeatedwirenames209
   *
   */
  @Test def testRepeatedWireNames209() {
    try {
      class CordicIO extends Bundle() {
        val out_broken = Bool(INPUT)
      }

      class CordicStage extends Module {
        val io = new CordicIO()
        val counter = Reg(init = UInt(0, 1))
        counter := counter + UInt(1)
        io.out_broken := counter(0)
      }

      class Cordic extends Module {
        val io = new CordicIO()
        val stage1 = Module(new CordicStage)
        val stage2 = Module(new CordicStage)
      }

      chiselMain(Array[String]("--v"), () => Module(new Cordic))
    } catch {
      case _ : Throwable => ;
    }
    assertTrue(ChiselError.hasErrors);
  }

  /** Test Unspecified Bundle Values.
   *
   */
  @Test def testUnspecifiedBundleValues () {
    println("\ntestUnspecifiedBundleValues ...")

    class myBundle extends Bundle
    {
       val a = Bool()
       val b = Bool()
       override def cloneType = new myBundle().asInstanceOf[this.type]
    }

    class UnspecifiedBundleValues extends Module {

       val io = new Bundle
       {
          val in = new myBundle().asInput
          val something = Bool(INPUT)
          val out = Bool(OUTPUT)
       }

       def NullMyBundle(): myBundle =
       {
          val bun = new myBundle
          bun.a := Bool(false)
          bun
       }

       val my_reg = Reg(init= NullMyBundle)

       when (io.something)
       {
          my_reg := io.in
       }

       io.out := my_reg.a || my_reg.b


       printf("Hello World!\n")
    }

    chiselMain(Array[String]("--backend", "v",
        "--targetDir", dir.getPath.toString()),
        () => Module(new UnspecifiedBundleValues()))
    assertFile("ConnectSuite_UnspecifiedBundleValues_1.v")
  }

  /* Signals only used as resets are trimmed (#346)
   *
   *
   */
  @Test def testUnconnectedResets() {
    class SubModule(reset: Bool) extends Module(null, reset) {
      class IO extends Bundle {
        val in = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
      val io = new IO

      val r = Reg(init = Bool(true))
      r := io.in
      io.out := r
    }

    class UnconnectedResets extends Module {
      class IO extends Bundle {
        val in  = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
      val io = new IO

      val regs = Reg(Vec(3, Bool()))
      regs(0) := reset
      for (i <- 1 until 3)
        regs(i) := regs(i-1)

      val sub = Module(new SubModule(regs(2)))
      sub.io.in := io.in
      io.out := sub.io.out /* | regs(2) */
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new UnconnectedResets()))

    assertFile("ConnectSuite_UnconnectedResets_1.v")
  }

  // Should be able to use submodule inputs to drive own logic
  @Test def testSubmoduleInputUse() {
    class PassThrough extends Module {
      val io = new Bundle {
        val ptin  = UInt(width=8).asInput
        val ptout = UInt(width=8).asOutput
      }
      io.ptout := io.ptin
    }
    class SubmoduleInputUse extends Module {
      val io = new Bundle {
        val in  = UInt(width=8).asInput
        val out1 = UInt(width=8).asOutput
        val out2a = UInt(width=8).asOutput
        val out2b = UInt(width=8).asOutput
        val out3  = UInt(width=8).asOutput
      }
      // The ordering of these vals is important
      //   as this order triggered an old (fixed) bug
      val pt3 = Module(new PassThrough)
      val pt2b = Module(new PassThrough)
      val pt2a = Module(new PassThrough)
      val pt1 = Module(new PassThrough)

      pt1.io.ptin := io.in
      pt2a.io.ptin := pt1.io.ptout
      pt2b.io.ptin := pt2a.io.ptin
      pt3.io.ptin := io.out2b

      io.out3  := pt3.io.ptout
      io.out2b := pt2b.io.ptout
      io.out2a := pt2a.io.ptout
      io.out1  := pt2a.io.ptin
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new SubmoduleInputUse()))

    assertFile("ConnectSuite_SubmoduleInputUse_1.v")
  }

  // More extensive test that submodule bindings are connected to appropriate logic
  @Test def testAddBindings() {
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

    assertFile("ConnectSuite_BindingTest_1.v")
  }

  /* Unused top level IOs are preserved (#172)
   *
   *
   */
  @Test def testUnconnectedIOs() {
    println("\ntestUnconnectedIOs ...")
    class SubModule(reset: Bool) extends Module(null, reset) {
      class IO extends Bundle {
        val in = Bool(INPUT)
        val out = Bool(OUTPUT)
        val ncIn = Bool(INPUT)
        val ncOut = Bool(OUTPUT)
      }
      val io = new IO

      val r = Reg(init = Bool(true))
      r := io.in
      io.out := r
    }

    class UnconnectedIOs extends Module {
      class IO extends Bundle {
        val in  = Bool(INPUT)
        val out = Bool(OUTPUT)
        val ncIn = Bool(INPUT)
        val ncOut = Bool(OUTPUT)
      }
      val io = new IO

      val regs = Reg(Vec(3, Bool()))
      regs(0) := reset
      for (i <- 1 until 3)
        regs(i) := regs(i-1)

      val sub = Module(new SubModule(regs(2)))
      sub.io.in := io.in
      io.out := sub.io.out /* | regs(2) */
    }

    chiselMain(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString()),
      () => Module(new UnconnectedIOs()))

    assertFile("ConnectSuite_UnconnectedIOs_1.h")
  }

  /* Missing default produces reasonable error message
   * (and not a bare "java.lang.IllegalArgumentException: Flat hash tables cannot contain null elements."
   * Issue #513
   */
  @Test def testNoDefaultIOs() {
    println("\ntestNoDefaultIOs ...")
    class NoDefaultIOs extends Module {
      class IO extends Bundle {
        val in  = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
      val io = new IO
      when(io.in) {
        io.out := Bool(true)
      }
    }

    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString())
    intercept[IllegalStateException] {
      // This should fail since io.out does not have a default value.
      chiselMain(testArgs, () => Module(new NoDefaultIOs))
    }
    assertTrue(ChiselError.hasErrors)
  }
}
