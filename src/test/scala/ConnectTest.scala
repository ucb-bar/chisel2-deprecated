import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
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
        val out = Decoupled(UInt())
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
    class ShimConnectionsTests(m: UsesShimParent) extends MapTester(m, Array(m.io)) {
      defTests {
        val vars = new HashMap[Node, Node]() 
        (0 until 4).map { i =>
          vars(m.io.in.bits) = UInt(i)
          vars(m.io.in.valid) = Bool(true)
          vars(m.io.in.ready) = Bool(true)
          vars(m.io.out.bits) = UInt(i+1)
          vars(m.io.out.valid) = Bool(true)
          vars(m.io.out.ready) = Bool(true)
          step(vars)
        } reduce(_&&_)
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
    class SuppliesResetsTests(m: SuppliesResetsParent) extends MapTester(m, Array(m.io)) {
      defTests {
        val vars = new HashMap[Node, Node]() 
        List(true,false,false,false,false,false).zip(
        List(true,true,true,false,false,false)).map {
          case (i, o) =>
            vars(m.io.in) = Bool(i)
            vars(m.io.out) = Bool(o)
            step(vars)
        } reduce(_&&_)
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
    class UnrelatedSubmodulesTests(m: Unrelated) extends MapTester(m, Array(m.io)) {
      defTests {
        val vars = new HashMap[Node, Node]() 
        (0 until 4).map { i =>
          vars(m.io.in) = UInt(i)
          vars(m.io.out) = UInt(i)
          step(vars)
        } reduce(_&&_)
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
    class LogicBtwInstancesTests(m: LogicBtwInstances) extends MapTester(m, Array(m.io)) {
      defTests {
        val vars = new HashMap[Node, Node]() 
        (0 until 4).map { i =>
          vars(m.io.in) = UInt(i)
          vars(m.io.out) = if(i == 0) UInt(0) else UInt(i-1)
          step(vars)
        } reduce(_&&_)
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
    class OneInstancePerRelationTests(m: C) extends MapTester(m, Array(m.io)) {
      defTests {
        val vars = new HashMap[Node, Node]() 
        (0 until 4).map { i =>
          vars(m.io.in) = UInt(i)
          vars(m.io.out) = UInt(i)
          step(vars)
        } reduce(_&&_)
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
    class InstanceSuperclassTests(m: B) extends MapTester(m, Array(m.io)) {
      defTests {
        val vars = new HashMap[Node, Node]() 
        (0 until 4).map { i =>
          vars(m.io.in) = UInt(i)
          vars(m.io.out) = UInt(i)
          step(vars)
        } reduce(_&&_)
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
      val reg_status = Reg(new RegStatus)
      when (io.wen) {
        reg_status := new RegStatus().fromBits(io.wdata)
      }
      io.status := reg_status
    }
    class RegisterHookTests(m: A) extends MapTester(m, Array(m.io)) {
      defTests {
        val vars = new HashMap[Node, Node]() 
        List(1,     2,     4,     6,     8,     12,    15,   15).zip(
        List(false, true,  true,  false, true,  false, true, false)).zip(
        List((0,0), (0,0), (0,2), (1,0), (1,0), (2,0), (2,0), (3,3), (3,3))).map { 
          case ((in, en), (im0, im1)) =>
            vars(m.io.wdata) = UInt(in)
            vars(m.io.wen) = Bool(en)
            vars(m.io.status.im0) = UInt(im0)
            vars(m.io.status.im1) = UInt(im1)
            step(vars)
        } reduce(_&&_)
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
        "--targetDir", dir.getPath.toString), 
        () => Module(new ForeignRef()))

    } catch {
      case _ : Throwable => ;
    }
    assertTrue(!ChiselError.ChiselErrors.isEmpty);
  }

}
