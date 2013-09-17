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

  @Test def testShimConnections() {
    println("\n### testShimConnections ###")
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
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new UsesShimParent))
    println(scala.io.Source.fromFile(tmpdir.getRoot() + "/ConnectSuite_UsesShimParent_1.v", "utf-8").mkString)
    assertFile(tmpdir.getRoot() + "/ConnectSuite_UsesShimParent_1.v",
"""module ConnectSuite_UsesShim_1(
    output io_in_ready,
    input  io_in_valid,
    input  io_in_bits,
    input  io_out_ready,
    output io_out_valid,
    output io_out_bits
);

  wire s_bits;
  wire T0;
  wire s_valid;
  wire s_ready;

  assign io_out_bits = s_bits;
  assign s_bits = T0;
  assign T0 = io_in_bits + 1'h1/* 1*/;
  assign io_out_valid = s_valid;
  assign s_valid = io_in_valid;
  assign io_in_ready = s_ready;
  assign s_ready = io_out_ready;
endmodule

module ConnectSuite_UsesShimParent_1(
    output io_in_ready,
    input  io_in_valid,
    input  io_in_bits,
    input  io_out_ready,
    output io_out_valid,
    output io_out_bits
);

  wire us_io_out_bits;
  wire us_io_out_valid;
  wire us_io_in_ready;

  assign io_out_bits = us_io_out_bits;
  assign io_out_valid = us_io_out_valid;
  assign io_in_ready = us_io_in_ready;
  ConnectSuite_UsesShim_1 us(
       .io_in_ready( us_io_in_ready ),
       .io_in_valid( io_in_valid ),
       .io_in_bits( io_in_bits ),
       .io_out_ready( io_out_ready ),
       .io_out_valid( us_io_out_valid ),
       .io_out_bits( us_io_out_bits )
  );
endmodule

""")
  }

  @Test def testResetConnections() {
    println("\n### testResetConnections ###")
    class UsesReset(resetSignal: Bool = null) extends Module(_reset = resetSignal) { 
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
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new SuppliesResetsParent))
    println(scala.io.Source.fromFile(tmpdir.getRoot() + "/ConnectSuite_SuppliesResetsParent_1.v", "utf-8").mkString)
    assertFile(tmpdir.getRoot() + "/ConnectSuite_SuppliesResetsParent_1.v",
"""module Queue(input clk, input reset,
    output io_enq_ready,
    input  io_enq_valid,
    input  io_enq_bits,
    input  io_deq_ready,
    output io_deq_valid,
    output io_deq_bits,
    output io_count
);

  wire T0;
  wire[2:0] T1;
  reg[0:0] maybe_full;
  wire T2;
  wire do_deq;
  wire T3;
  wire do_flow;
  wire T4;
  wire do_enq;
  wire T5;
  wire T6;
  wire T7;
  reg [0:0] ram [0:0];
  wire T8;
  wire T9;
  wire T10;
  wire empty;
  wire T11;

  assign io_count = T0;
  assign T0 = T1[1'h0/* 0*/:1'h0/* 0*/];
  assign T1 = {maybe_full, 2'h0/* 0*/};
  assign T2 = do_enq != do_deq;
  assign do_deq = T4 && T3;
  assign T3 = ! do_flow;
  assign do_flow = 1'h0/* 0*/;
  assign T4 = io_deq_ready && io_deq_valid;
  assign do_enq = T6 && T5;
  assign T5 = ! do_flow;
  assign T6 = io_enq_ready && io_enq_valid;
  assign io_deq_bits = T7;
  assign T7 = ram[1'h0/* 0*/];
  assign T9 = io_enq_bits;
  assign io_deq_valid = T10;
  assign T10 = ! empty;
  assign empty = ! maybe_full;
  assign io_enq_ready = T11;
  assign T11 = ! maybe_full;

  always @(posedge clk) begin
    if(reset) begin
      maybe_full <= 1'h0/* 0*/;
    end else if(T2) begin
      maybe_full <= do_enq;
    end
    if (do_enq)
      ram[1'h0/* 0*/] <= T9;
  end
endmodule

module ConnectSuite_UsesReset_2(input clk, input reset,
    input  io_in,
    output io_out
);

  wire T0;
  wire q_io_deq_bits;

  assign io_out = T0;
  assign T0 = q_io_deq_bits || reset;
  Queue q(.clk(clk), .reset(reset),
       //.io_enq_ready(  )
       .io_enq_valid( 1'h1/* 1*/ ),
       .io_enq_bits( io_in ),
       .io_deq_ready( 1'h1/* 1*/ ),
       //.io_deq_valid(  )
       .io_deq_bits( q_io_deq_bits )
       //.io_count(  )
  );
endmodule

module ConnectSuite_SuppliesResets_1(input clk, input reset,
    input  io_in,
    output io_out
);

  reg[0:0] R0;
  reg[0:0] delayed;
  wire T1;
  wire T2;
  wire a2_io_out;
  wire T3;
  wire a1_io_out;
  wire a0_io_out;

  assign io_out = T1;
  assign T1 = T2 || delayed;
  assign T2 = T3 || a2_io_out;
  assign T3 = a0_io_out || a1_io_out;
  ConnectSuite_UsesReset_2 a0(.clk(clk), .reset(reset),
       .io_in( io_in ),
       .io_out( a0_io_out )
  );
  ConnectSuite_UsesReset_2 a1(.clk(clk), .reset(delayed),
       .io_in( io_in ),
       .io_out( a1_io_out )
  );
  ConnectSuite_UsesReset_2 a2(.clk(clk), .reset(R0),
       .io_in( io_in ),
       .io_out( a2_io_out )
  );

  always @(posedge clk) begin
    R0 <= reset;
    delayed <= reset;
  end
endmodule

module ConnectSuite_SuppliesResetsParent_1(input clk, input reset,
    input  io_in,
    output io_out
);

  wire srs_io_out;

  assign io_out = srs_io_out;
  ConnectSuite_SuppliesResets_1 srs(.clk(clk), .reset(reset),
       .io_in( io_in ),
       .io_out( srs_io_out )
  );
endmodule

""")
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
    output io_a_out
);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_B_1(
    input  io_b_in,
    output io_b_out
);

  wire aComp_io_a_out;

  assign io_b_out = aComp_io_a_out;
  ConnectSuite_A_1 aComp(
       .io_a_in( io_b_in ),
       .io_a_out( aComp_io_a_out )
  );
endmodule

module ConnectSuite_NoClassRelation_1(
    input  io_c_in,
    output io_c_out
);

  wire aComp_io_b_out;

  assign io_c_out = aComp_io_b_out;
  ConnectSuite_B_1 aComp(
       .io_b_in( io_c_in ),
       .io_b_out( aComp_io_b_out )
  );
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
    output io_a_out
);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_LogicBtwInstances_1(input clk,
    input  io_b_in,
    output io_b_out
);

  wire T0;
  reg[0:0] x;
  wire T1;
  wire a2_io_a_out;
  wire a1_io_a_out;

  assign io_b_out = T0;
  assign T0 = T1 | x;
  assign T1 = a1_io_a_out | a2_io_a_out;
  ConnectSuite_A_2 a1(
       .io_a_in( io_b_in ),
       .io_a_out( a1_io_a_out )
  );
  ConnectSuite_A_2 a2(
       .io_a_in( io_b_in ),
       .io_a_out( a2_io_a_out )
  );

  always @(posedge clk) begin
    x <= io_b_in;
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
    output io_a_out
);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_InstanceSuperclass_1(
    input  io_a_in,
    output io_a_out
);


  assign io_a_out = io_a_in;
  ConnectSuite_A_3 aInBComp(
       .io_a_in( io_a_in )
       //.io_a_out(  )
  );
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
    """module ConnectSuite_A_4(input clk,
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
    input [31:0] io_wdata
);

  reg[0:0] reg_status_et;
  wire T0;
  reg[0:0] reg_status_ef;
  wire T1;
  reg[0:0] reg_status_ev;
  wire T2;
  reg[0:0] reg_status_ec;
  wire T3;
  reg[0:0] reg_status_ps;
  wire T4;
  reg[0:0] reg_status_s;
  wire T5;
  reg[0:0] reg_status_u64;
  wire T6;
  reg[0:0] reg_status_s64;
  wire T7;
  reg[0:0] reg_status_vm;
  wire T8;
  reg[6:0] reg_status_zero;
  wire[6:0] T9;
  reg[7:0] reg_status_im;
  wire[7:0] T10;

  assign io_status_et = reg_status_et;
  assign T0 = io_wdata[1'h0/* 0*/:1'h0/* 0*/];
  assign io_status_ef = reg_status_ef;
  assign T1 = io_wdata[1'h1/* 1*/:1'h1/* 1*/];
  assign io_status_ev = reg_status_ev;
  assign T2 = io_wdata[2'h2/* 2*/:2'h2/* 2*/];
  assign io_status_ec = reg_status_ec;
  assign T3 = io_wdata[2'h3/* 3*/:2'h3/* 3*/];
  assign io_status_ps = reg_status_ps;
  assign T4 = io_wdata[3'h4/* 4*/:3'h4/* 4*/];
  assign io_status_s = reg_status_s;
  assign T5 = io_wdata[3'h5/* 5*/:3'h5/* 5*/];
  assign io_status_u64 = reg_status_u64;
  assign T6 = io_wdata[3'h6/* 6*/:3'h6/* 6*/];
  assign io_status_s64 = reg_status_s64;
  assign T7 = io_wdata[3'h7/* 7*/:3'h7/* 7*/];
  assign io_status_vm = reg_status_vm;
  assign T8 = io_wdata[4'h8/* 8*/:4'h8/* 8*/];
  assign io_status_zero = reg_status_zero;
  assign T9 = io_wdata[4'hf/* 15*/:4'h9/* 9*/];
  assign io_status_im = reg_status_im;
  assign T10 = io_wdata[5'h17/* 23*/:5'h10/* 16*/];

  always @(posedge clk) begin
    if(io_wen) begin
      reg_status_et <= T0;
    end
    if(io_wen) begin
      reg_status_ef <= T1;
    end
    if(io_wen) begin
      reg_status_ev <= T2;
    end
    if(io_wen) begin
      reg_status_ec <= T3;
    end
    if(io_wen) begin
      reg_status_ps <= T4;
    end
    if(io_wen) begin
      reg_status_s <= T5;
    end
    if(io_wen) begin
      reg_status_u64 <= T6;
    end
    if(io_wen) begin
      reg_status_s64 <= T7;
    end
    if(io_wen) begin
      reg_status_vm <= T8;
    end
    if(io_wen) begin
      reg_status_zero <= T9;
    end
    if(io_wen) begin
      reg_status_im <= T10;
    end
  end
endmodule

""")
  }

}
