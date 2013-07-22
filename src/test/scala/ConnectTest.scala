import scala.collection.mutable.ArrayBuffer
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After
import org.junit.rules.TemporaryFolder;

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
class ConnectSuite extends AssertionsForJUnit {

  val tmpdir = new TemporaryFolder();

  @Before def initialize() {
    tmpdir.create()
  }

  @After def done() {
    tmpdir.delete()
  }

  def assertFile( filename: String, content: String ) {
    val source = scala.io.Source.fromFile(filename, "utf-8")
    val lines = source.mkString
    source.close()
    assert(lines === content)
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
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new NoClassRelation()))
    assertFile(tmpdir.getRoot() + "/ConnectSuite_NoClassRelation_1.v",
"""module ConnectSuite_A_1(
    input  io_a_in,
    output io_a_out);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_B_1(
    input  io_b_in,
    output io_b_out);

  wire aComp_io_a_out;

  assign io_b_out = aComp_io_a_out;
  ConnectSuite_A_1 aComp(
       .io_a_in( io_b_in ),
       .io_a_out( aComp_io_a_out ));
endmodule

module ConnectSuite_NoClassRelation_1(
    input  io_c_in,
    output io_c_out);

  wire aComp_io_b_out;

  assign io_c_out = aComp_io_b_out;
  ConnectSuite_B_1 aComp(
       .io_b_in( io_c_in ),
       .io_b_out( aComp_io_b_out ));
endmodule

""")
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
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new LogicBtwInstances()))
    assertFile(tmpdir.getRoot() + "/ConnectSuite_LogicBtwInstances_1.v",
"""module ConnectSuite_A_2(
    input  io_a_in,
    output io_a_out);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_LogicBtwInstances_1(input clk, input reset,
    input  io_b_in,
    output io_b_out);

  wire T0;
  reg[0:0] x;
  wire T1;
  wire T2;
  wire a2_io_a_out;
  wire a1_io_a_out;

  assign io_b_out = T0;
  assign T0 = T2 | x;
  assign T1 = 1'h1/* 1*/ ? io_b_in : x;
  assign T2 = a1_io_a_out | a2_io_a_out;
  ConnectSuite_A_2 a1(
       .io_a_in( io_b_in ),
       .io_a_out( a1_io_a_out ));
  ConnectSuite_A_2 a2(
       .io_a_in( io_b_in ),
       .io_a_out( a2_io_a_out ));

  always @(posedge clk) begin
    x <= T1;
  end
endmodule

""")
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
//      "--targetDir", tmpdir.getRoot().toString()),
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
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new InstanceSuperclass()))
    assertFile(tmpdir.getRoot() + "/ConnectSuite_InstanceSuperclass_1.v",
"""module ConnectSuite_A_3(
    input  io_a_in,
    output io_a_out);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_InstanceSuperclass_1(
    input  io_a_in,
    output io_a_out);


  assign io_a_out = io_a_in;
  ConnectSuite_A_3 aInBComp(
       .io_a_in( io_a_in ),
       .io_a_out(  ));
endmodule

""")
  }

  /** Test hooking-up Registers. */
  @Test def testRegisterHook() {
    println("\n### Hooking-up Registers ###")
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
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new A()))
    assertFile(tmpdir.getRoot() + "/ConnectSuite_A_4.v",
    """module ConnectSuite_A_4(input clk, input reset,
    output[7:0] io_status_im,
    output[6:0] io_status_zero,
    output io_status_vm,
    output io_status_s64,
    output io_status_u64,
    output io_status_s,
    output io_status_ps,
    output io_status_ec,
    output io_status_ev,
    output io_status_ef,
    output io_status_et,
    input  io_wen,
    input [31:0] io_wdata);

  reg[0:0] reg_status_et;
  wire T0;
  wire T1;
  reg[0:0] reg_status_ef;
  wire T2;
  wire T3;
  reg[0:0] reg_status_ev;
  wire T4;
  wire T5;
  reg[0:0] reg_status_ec;
  wire T6;
  wire T7;
  reg[0:0] reg_status_ps;
  wire T8;
  wire T9;
  reg[0:0] reg_status_s;
  wire T10;
  wire T11;
  reg[0:0] reg_status_u64;
  wire T12;
  wire T13;
  reg[0:0] reg_status_s64;
  wire T14;
  wire T15;
  reg[0:0] reg_status_vm;
  wire T16;
  wire T17;
  reg[6:0] reg_status_zero;
  wire[6:0] T18;
  wire[6:0] T19;
  reg[7:0] reg_status_im;
  wire[7:0] T20;
  wire[7:0] T21;

  assign io_status_et = reg_status_et;
  assign T0 = 1'h1/* 1*/ ? T1 : reg_status_et;
  assign T1 = io_wdata[1'h0/* 0*/:1'h0/* 0*/];
  assign io_status_ef = reg_status_ef;
  assign T2 = 1'h1/* 1*/ ? T3 : reg_status_ef;
  assign T3 = io_wdata[1'h1/* 1*/:1'h1/* 1*/];
  assign io_status_ev = reg_status_ev;
  assign T4 = 1'h1/* 1*/ ? T5 : reg_status_ev;
  assign T5 = io_wdata[2'h2/* 2*/:2'h2/* 2*/];
  assign io_status_ec = reg_status_ec;
  assign T6 = 1'h1/* 1*/ ? T7 : reg_status_ec;
  assign T7 = io_wdata[2'h3/* 3*/:2'h3/* 3*/];
  assign io_status_ps = reg_status_ps;
  assign T8 = 1'h1/* 1*/ ? T9 : reg_status_ps;
  assign T9 = io_wdata[3'h4/* 4*/:3'h4/* 4*/];
  assign io_status_s = reg_status_s;
  assign T10 = 1'h1/* 1*/ ? T11 : reg_status_s;
  assign T11 = io_wdata[3'h5/* 5*/:3'h5/* 5*/];
  assign io_status_u64 = reg_status_u64;
  assign T12 = 1'h1/* 1*/ ? T13 : reg_status_u64;
  assign T13 = io_wdata[3'h6/* 6*/:3'h6/* 6*/];
  assign io_status_s64 = reg_status_s64;
  assign T14 = 1'h1/* 1*/ ? T15 : reg_status_s64;
  assign T15 = io_wdata[3'h7/* 7*/:3'h7/* 7*/];
  assign io_status_vm = reg_status_vm;
  assign T16 = 1'h1/* 1*/ ? T17 : reg_status_vm;
  assign T17 = io_wdata[4'h8/* 8*/:4'h8/* 8*/];
  assign io_status_zero = reg_status_zero;
  assign T18 = 1'h1/* 1*/ ? T19 : reg_status_zero;
  assign T19 = io_wdata[4'hf/* 15*/:4'h9/* 9*/];
  assign io_status_im = reg_status_im;
  assign T20 = 1'h1/* 1*/ ? T21 : reg_status_im;
  assign T21 = io_wdata[5'h17/* 23*/:5'h10/* 16*/];

  always @(posedge clk) begin
    if(io_wen) begin
      reg_status_et <= T0;
    end
    if(io_wen) begin
      reg_status_ef <= T2;
    end
    if(io_wen) begin
      reg_status_ev <= T4;
    end
    if(io_wen) begin
      reg_status_ec <= T6;
    end
    if(io_wen) begin
      reg_status_ps <= T8;
    end
    if(io_wen) begin
      reg_status_s <= T10;
    end
    if(io_wen) begin
      reg_status_u64 <= T12;
    end
    if(io_wen) begin
      reg_status_s64 <= T14;
    end
    if(io_wen) begin
      reg_status_vm <= T16;
    end
    if(io_wen) begin
      reg_status_zero <= T18;
    end
    if(io_wen) begin
      reg_status_im <= T20;
    end
  end
endmodule

""")
  }

}
