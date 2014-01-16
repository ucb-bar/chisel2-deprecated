import scala.collection.mutable.ArrayBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._

class RegStatus extends Bundle {
  val im = UInt(width = 8)
  val zero = UInt(width = 7)
  val vm = Bool()
  val s64 = Bool()
  val u64 = Bool()
  val s = Bool()
  val ps = Bool()
  val ec = Bool()
  val ev = Bool()
  val ef = Bool()
  val et = Bool()
}


/** This testsuite checks interaction of component class
  and runtime hierarchies.
*/
class ConnectSuite extends TestSuite {

  @Test def testShimConnections() {
    println("\n### testShimConnections ###")
    try{
    class UsesShim extends Module {
      val io = new Bundle {
        val in = Decoupled(UInt(width = 1)).flip
        val out = Decoupled(UInt(width = 1))
      }

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
      val io = new Bundle {
        val in = Decoupled(UInt(width = 1)).flip
        val out = Decoupled(UInt(width = 1))
      }
      val us = Module(new UsesShim)
      us.io.in <> io.in
      io.out <> us.io.out
    }
    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new UsesShimParent))
    } catch {
      case e => e.printStackTrace()
    }
    assertFile("ConnectSuite_UsesShimParent_1.v")
  }

    /** XXX The following text output was generated with scala 2.10
      running on a java 1.6 virtual machine. Though the logic generated
      is the same, the textual output will be different when running on
      a java 1.7 virtual machine due to variable name propagation in constant
      folding optimization. */
  @Ignore("java 1.6 vs 1.7 issue") @Test def testResetConnections() {
    println("\n### testResetConnections ###")
    class UsesReset(resetSignal: Bool = null) extends Module(reset = resetSignal) { 
      val io = new Bundle {
        val in = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
      val q = Module(new Queue(Bool(), 1))
      q.io.enq.valid := Bool(true)
      q.io.enq.bits := io.in
      q.io.deq.ready := Bool(true)
      io.out := q.io.deq.bits || reset
    }
    class SuppliesResets extends Module {
      val io = new Bundle {
        val in = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
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
      val io = new Bundle {
        val in = Bool(INPUT)
        val out = Bool(OUTPUT)
      }
      val srs = Module(new SuppliesResets)
      srs.io.in := io.in
      io.out := srs.io.out
    }
    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new SuppliesResetsParent))
    assertFile("ConnectSuite_SuppliesResetsParent_1.v")
  }

  /** Instantiate a component tree where all component classes have
    no relationship. */
  @Test def testNoClassRelation() {
    println("\n### testNoClassRelation ###")
    class A extends Module {
      val io = new Bundle {
        val a_in = UInt(INPUT, 1)
        val a_out = UInt(OUTPUT, 1)
      }
      io.a_out := io.a_in
    }
    class B extends Module {
      val io = new Bundle {
        val b_in = UInt(INPUT, 1)
        val b_out = UInt(OUTPUT, 1)
      }
      val aComp = Module(new A())
      aComp.io.a_in := io.b_in
      io.b_out := aComp.io.a_out
    }
    class NoClassRelation extends Module {
      val io = new Bundle {
        val c_in = UInt(INPUT, 1)
        val c_out = UInt(OUTPUT, 1)
      }
      val aComp = Module(new B())
      aComp.io.b_in := io.c_in
      io.c_out := aComp.io.b_out
    }
    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new NoClassRelation()))
    assertFile("ConnectSuite_NoClassRelation_1.v")
  }

  /** Instantiate a component of the same class (*A*) twice
    with logic in-between. */
  @Test def testLogicBtwInstances() {
    println("\n### testLogicBtwInstances ###")
    class A extends Module {
      val io = new Bundle {
        val a_in = UInt(INPUT, 1)
        val a_out = UInt(OUTPUT, 1)
      }
      io.a_out := io.a_in
    }
    class LogicBtwInstances extends Module {
      val io = new Bundle {
        val b_in = UInt(INPUT, 1)
        val b_out = UInt(OUTPUT, 1)
      }
      val a1 = Module(new A())
      val x = Reg(UInt(1))
      x := io.b_in
      val a2 = Module(new A())
      a1.io.a_in := io.b_in
      a2.io.a_in := io.b_in
      io.b_out := a1.io.a_out | a2.io.a_out | x
    }
    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new LogicBtwInstances()))
    assertFile("ConnectSuite_LogicBtwInstances_1.v")
  }

  /** Instantiate a component of the same class (*A*) at two levels
    of a component class hierarchy (*B* < *C*). */
  @Test def test2Instance2Level() {
    println("\n### test2Instance2Level ###")
    /* XXX This test will fail to pick up the correct pop sequence
     on the Module stack.
    class A extends Module {
      val io = new Bundle {
        val a_in = UInt(INPUT, 1)
        val a_out = UInt(OUTPUT, 1)
      }
      io.a_out := io.a_in
    }
    class B extends Module {
      val io = new Bundle {
        val b_in = UInt(INPUT, 1)
        val b_out = UInt(OUTPUT, 1)
      }
      val aInBComp = Module(new A())
      aInBComp.io.a_in := io.b_in
    }
    class Instance2Level extends B {
      val aInCComp = Module(new A())
      aInCComp.io.a_in := io.b_in
      io.b_out := aInCComp.io.a_out | aInBComp.io.a_out
    }
    chiselMain(Array[String]("--v"),
//      "--targetDir", dir.getPath.toString()),
      () => Module(new Instance2Level()))
     */
  }

  /** Instantiate a component superclass inside a component */
  @Test def testInstanceSuperclass() {
    println("\n### testInstanceSuperclass ###")
    class A extends Module {
      val io = new Bundle {
        val a_in = UInt(INPUT, 1)
        val a_out = UInt(OUTPUT, 1)
      }
      io.a_out := io.a_in
    }
    class InstanceSuperclass extends A {
      val aInBComp = Module(new A())
      aInBComp.io.a_in := io.a_in
    }
    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new InstanceSuperclass()))
    assertFile("ConnectSuite_InstanceSuperclass_1.v")
  }

  /** Test hooking-up Registers. */
  @Test def testRegisterHook() {
    println("\ntestRegisterHook:")
    class A extends Module {
      val io = new Bundle {
        val status = new RegStatus().asOutput
        val wen   = Bool(INPUT)
        val wdata = UInt(INPUT, 32)
      }

      val reg_status = Reg(new RegStatus) // reset down below

      io.status := reg_status
      when (io.wen) {
        reg_status := new RegStatus().fromBits(io.wdata)
      }
    }
    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new A()))
    assertFile("ConnectSuite_A_4.v")
  }

}
