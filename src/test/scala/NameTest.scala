/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
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
import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After
import org.junit.rules.TemporaryFolder;

import Chisel._

/* XXX Throws an error "Test NodeSuite.testBindComp3 failed: Parameterized
 Bundle class NodeSuite$Status$1 needs clone method" when Status is declared
 inside testBindComp3 instead of here at the top level. */
class Status extends Bundle {
  val im = UInt(width = 8)
}


/** This testsuite checks the naming of variables in the generated
  verilog and C++ code.
*/
class NameSuite extends AssertionsForJUnit {

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


  /** Checks names are correctly generated in the presence
   of ListLookups. */
  @Test def testListLookups() {
    trait Constants {
      val VXCPTHOLD  = UInt("b00000_00000_00000_0001001110_1111011", 32)
      val VCMD_X = UInt(0, 3)
      val VIMM_X = UInt(0, 1)
      val N = UInt(0, 1);
    }

    class ListLookupsComp extends Module with Constants {
      val io = new Bundle {
        val inst = UInt(INPUT, 32)
        val sigs_valid = Bool(OUTPUT)
      }
      val veccs =
        ListLookup(io.inst,
          List(N, VCMD_X, VIMM_X,   VIMM_X,  N),
          Array(VXCPTHOLD-> List(N, VCMD_X, VIMM_X, VIMM_X,  N)))

      val valid :: veccs0 = veccs
      io.sigs_valid := valid.toBool
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new ListLookupsComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_ListLookupsComp_1.v",
"""module NameSuite_ListLookupsComp_1(
    input [31:0] io_inst,
    output io_sigs_valid
);

  wire T0;
  reg[0:0] valid;
  wire T1;

  assign io_sigs_valid = T0;
  assign T0 = valid;
  always @(*) begin
    casez (io_inst)
      32'b00000000000000000010011101111011/* 0*/ : begin
        valid = 1'h0/* 0*/;
      end
      default: begin
        valid = 1'h0/* 0*/;
      end
    endcase
  end
endmodule

""")
  }

  /** This test checks names are correctly generated in the presence
   of Binding. */
  @Test def testBindFirst() {
    class BlockIO extends Bundle {
      val valid = Bool(INPUT)
      val replay = Bool(OUTPUT)
    }

    class BlockSigs extends Bundle {
      val enq_cmdq = Bool()
      val enq_ximm1q = Bool()
    }

    class BlockDecoder extends Module {
      val io = new Bundle {
        val valid = Bool(INPUT)
        val replay = Bool(OUTPUT)
        val sigs = new BlockSigs().asOutput
      }

      io.replay := io.valid;
    }

    class BindFirstComp extends Module {
      val io = new BlockIO
      val dec = Module(new BlockDecoder());

      val valid_common = io.valid;

      val mask_cmdq_ready = !dec.io.sigs.enq_cmdq;
      val mask_ximm1q_ready = !dec.io.sigs.enq_ximm1q;

      io.replay := valid_common && (
        !mask_cmdq_ready || !mask_ximm1q_ready);

      val dec_replay = dec.io.replay;
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new BindFirstComp()))
   assertFile(tmpdir.getRoot() + "/NameSuite_BindFirstComp_1.v",
"""module NameSuite_BlockDecoder_1(
    input  io_valid,
    output io_replay,
    output io_sigs_enq_cmdq,
    output io_sigs_enq_ximm1q
);


  assign io_replay = io_valid;
endmodule

module NameSuite_BindFirstComp_1(
    input  valid_common,
    output io_replay
);

  wire T0;
  wire T1;
  wire T2;
  wire mask_ximm1q_ready;
  wire dec_io_sigs_enq_ximm1q;
  wire T3;
  wire mask_cmdq_ready;
  wire dec_io_sigs_enq_cmdq;

  assign io_replay = T0;
  assign T0 = valid_common && T1;
  assign T1 = T3 || T2;
  assign T2 = ! mask_ximm1q_ready;
  assign mask_ximm1q_ready = ! dec_io_sigs_enq_ximm1q;
  assign T3 = ! mask_cmdq_ready;
  assign mask_cmdq_ready = ! dec_io_sigs_enq_cmdq;
  NameSuite_BlockDecoder_1 dec(
       //.io_valid(  )
       //.io_replay(  )
       .io_sigs_enq_cmdq( dec_io_sigs_enq_cmdq ),
       .io_sigs_enq_ximm1q( dec_io_sigs_enq_ximm1q )
  );
  `ifdef SYNTHESIS
    assign dec.io_valid = $random();
    assign dec.io_sigs_enq_cmdq = $random();
    assign dec.io_sigs_enq_ximm1q = $random();
  `endif
endmodule

""")
   }

  /** Since *vec* is declared within a if() block and not as a Module
    member, we do not extract the actual name of the module instance
    (i.e. *vec*) which means that if we do not set an arbitrary name on
    that component instance, it will remain empty.
    This would result in bindings starting with a "_" prefix (target
    Module name is empty).

    We thus generate a name for the component instance based on its class name.
    */
  @Test def testBindSecond() {
    class Block extends Module {
      val io = new Bundle() {
        val irq = Bool(INPUT)
        val irq_cause = UInt(OUTPUT, 5)
      }
      io.irq_cause := UInt(2);
    }

    class BindSecondComp(conf: Boolean) extends Module {
      val io = new Bundle() {
        val irq = Bool(INPUT)
        val irq_cause = UInt(OUTPUT, 6)
      }

      if( conf ) {
        val vec = Module(new Block());
        vec.io.irq := io.irq;
        io.irq_cause := UInt(1) ## vec.io.irq_cause;
      }
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new BindSecondComp(true)))
   assertFile(tmpdir.getRoot() + "/NameSuite_BindSecondComp_1.v",
"""module NameSuite_Block_1(
    input  io_irq,
    output[4:0] io_irq_cause
);

  wire[4:0] T0;

  assign io_irq_cause = T0;
  assign T0 = {3'h0/* 0*/, 2'h2/* 2*/};
endmodule

module NameSuite_BindSecondComp_1(
    input  io_irq,
    output[5:0] io_irq_cause
);

  wire[5:0] T0;
  wire[4:0] NameSuite_Block_1_io_irq_cause;

  assign io_irq_cause = T0;
  assign T0 = {1'h1/* 1*/, NameSuite_Block_1_io_irq_cause};
  NameSuite_Block_1 NameSuite_Block_1(
       .io_irq( io_irq ),
       .io_irq_cause( NameSuite_Block_1_io_irq_cause )
  );
endmodule

""")
  }

  /** Propagation of bundle field names through bindings

    At the toplevel *Comp*, variables should be named conn_X
    instead of using a derived name derived from CompIO.
    */
  @Test def testBindThird() {
    class BankToBankIO extends Bundle {
      val ren    = Bool(INPUT)
    }

    class CompIO extends Bundle {
      val in = new BankToBankIO()
      val out = new BankToBankIO().flip
    }


    class Comp extends Module {
      val io = new CompIO()
      io.out.ren := io.in.ren
    }

    class BindThirdComp extends Module {
      val io = new Bundle() {
        val in = new BankToBankIO()
        val result = Bool(OUTPUT)
      }

      val conn = new ArrayBuffer[BankToBankIO]

      var first = true
      for (i <- 0 until 4) {
        val bank = Module(new Comp())
        if (first) {
          bank.io.in <> io.in
          first = false
        } else {
          bank.io.in <> conn.last
        }
        conn += bank.io.out
      }
      io.result := conn(0).ren | conn(1).ren | conn(2).ren | conn(3).ren
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new BindThirdComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_BindThirdComp_1.v",
"""module NameSuite_Comp_1(
    input  io_in_ren,
    output io_out_ren
);


  assign io_out_ren = io_in_ren;
endmodule

module NameSuite_BindThirdComp_1(
    input  io_in_ren,
    output io_result
);

  wire NameSuite_Comp_1_2_io_out_ren;
  wire NameSuite_Comp_1_1_io_out_ren;
  wire NameSuite_Comp_1_0_io_out_ren;
  wire T0;
  wire NameSuite_Comp_1_3_io_out_ren;
  wire T1;
  wire T2;

  assign io_result = T0;
  assign T0 = T1 | NameSuite_Comp_1_3_io_out_ren;
  assign T1 = T2 | NameSuite_Comp_1_2_io_out_ren;
  assign T2 = NameSuite_Comp_1_0_io_out_ren | NameSuite_Comp_1_1_io_out_ren;
  NameSuite_Comp_1 NameSuite_Comp_1_0(
       .io_in_ren( io_in_ren ),
       .io_out_ren( NameSuite_Comp_1_0_io_out_ren )
  );
  NameSuite_Comp_1 NameSuite_Comp_1_1(
       .io_in_ren( NameSuite_Comp_1_0_io_out_ren ),
       .io_out_ren( NameSuite_Comp_1_1_io_out_ren )
  );
  NameSuite_Comp_1 NameSuite_Comp_1_2(
       .io_in_ren( NameSuite_Comp_1_1_io_out_ren ),
       .io_out_ren( NameSuite_Comp_1_2_io_out_ren )
  );
  NameSuite_Comp_1 NameSuite_Comp_1_3(
       .io_in_ren( NameSuite_Comp_1_2_io_out_ren ),
       .io_out_ren( NameSuite_Comp_1_3_io_out_ren )
  );
endmodule

""")
  }

  /** Propagation of bundle field names through bindings

    In this case we don't want *norms* to propagate to the input/output
    of the module.
    */
  @Test def testBindFourth() {
    class CompIO extends Bundle {
      val in = UInt(INPUT, 5)
      val out = UInt(OUTPUT, 5)
    }

    class BindFourthComp extends Module {
      val io = new CompIO()

      var norms = ArrayBuffer[UInt]();
      norms += io.in;
      io.out := norms.last
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new BindFourthComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_BindFourthComp_1.v",
"""module NameSuite_BindFourthComp_1(
    input [4:0] io_in,
    output[4:0] io_out
);


  assign io_out = io_in;
endmodule

""")
  }

  /* An ArrayBuffer is added to after initialization, then later
   the last entry of the ArrayBuffer is attached to a io port
   of a subcomponent.
   */
  @Test def testBindFith() {

    println("\nRunning testBindFith:")

    class UnamedBundle extends Bundle {
      val error = Bool()
      val ppn = UInt(width = 32)

      override def clone = new UnamedBundle().asInstanceOf[this.type]
    }

    class BlockIO extends Bundle {
      val resp = Valid(new UnamedBundle()).flip
    }

    class Block extends Module {
      val io = new Bundle {
        val valid = Bool(INPUT)
        val mine = Vec.fill(2){UInt(width = 32)}.asOutput
        val sub = new BlockIO()
      }
      val tag_ram = Vec.fill(2){ Reg(io.sub.resp.bits.ppn) }
      when (io.valid) {
        tag_ram(UInt(0)) := io.sub.resp.bits.ppn
      }
      io.mine := Mux(io.valid, Mux1H(UInt(1), tag_ram), Mux1H(UInt(0), tag_ram))
    }

    class BindFithComp extends Module {
      val io = new Bundle {
        val imem_ptw = new BlockIO()
        val dmem_ptw = new BlockIO()
        val resp = new BlockIO().asOutput
      }

      val ptw = collection.mutable.ArrayBuffer(io.imem_ptw, io.dmem_ptw)
      if( true ) {
        val vdtlb = Module(new Block())
        ptw += vdtlb.io.sub
        vdtlb.io <> io.imem_ptw
      }
      io.resp := ptw(0)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new BindFithComp))
    assertFile(tmpdir.getRoot() + "/NameSuite_BindFithComp_1.v",
"""module NameSuite_Block_2(input clk,
    input  io_valid,
    output[31:0] io_mine_0,
    output[31:0] io_mine_1,
    //input  io_sub_resp_valid
    //input  io_sub_resp_bits_error
    input [31:0] io_sub_resp_bits_ppn
);

  wire[31:0] T0;
  wire T1;
  wire[31:0] T2;
  reg[31:0] tag_ram_1;
  reg[31:0] tag_ram_0;
  wire[31:0] T3;
  wire T4;

  assign io_mine_1 = T0;
  assign T0 = {31'h0/* 0*/, T1};
  assign T1 = T2[1'h1/* 1*/:1'h1/* 1*/];
  assign T2 = io_valid ? tag_ram_0 : tag_ram_1;
  assign io_mine_0 = T3;
  assign T3 = {31'h0/* 0*/, T4};
  assign T4 = T2[1'h0/* 0*/:1'h0/* 0*/];

  always @(posedge clk) begin
    if(1'h0/* 0*/) begin
      tag_ram_1 <= io_sub_resp_bits_ppn;
    end
    if(io_valid) begin
      tag_ram_0 <= io_sub_resp_bits_ppn;
    end
  end
endmodule

module NameSuite_BindFithComp_1(input clk,
    input  io_imem_ptw_resp_valid,
    input  io_imem_ptw_resp_bits_error,
    input [31:0] io_imem_ptw_resp_bits_ppn,
    input  io_dmem_ptw_resp_valid,
    input  io_dmem_ptw_resp_bits_error,
    input [31:0] io_dmem_ptw_resp_bits_ppn,
    output io_resp_resp_valid,
    output io_resp_resp_bits_error,
    output[31:0] io_resp_resp_bits_ppn
);


  assign io_resp_resp_bits_ppn = io_imem_ptw_resp_bits_ppn;
  assign io_resp_resp_bits_error = io_imem_ptw_resp_bits_error;
  assign io_resp_resp_valid = io_imem_ptw_resp_valid;
  NameSuite_Block_2 NameSuite_Block_2(.clk(clk),
       //.io_valid(  )
       //.io_mine_0(  )
       //.io_mine_1(  )
       //.io_sub_resp_valid(  )
       //.io_sub_resp_bits_error(  )
       //.io_sub_resp_bits_ppn(  )
  );
  `ifdef SYNTHESIS
    assign NameSuite_Block_2.io_valid = $random();
    assign NameSuite_Block_2.io_sub_resp_bits_ppn = $random();
  `endif
endmodule

""")
  }

  /** Appending index to a node name in Vec::apply
    without setting *named* to true.
    */
  @Test def testVec() {
    println("\nRunning testVec:")
    class VecComp extends Module {
      val io = new Bundle {
        val pcr_req_data = UInt(width = 64)

        val r_en   = Bool(INPUT)
        val r_addr = UInt(INPUT, log2Up(32))
        val w_data = UInt(INPUT, 64)

        val status = new Status().asOutput
      }

      val reg_status = Reg(new Status)
      val rdata = UInt();

      val host_pcr_bits_data = Reg(io.pcr_req_data)
      when (io.r_en) {
        host_pcr_bits_data := rdata
      }

      val wdata = Mux(io.r_en, io.w_data, host_pcr_bits_data)

      io.status := reg_status

      val elts = Vec(List[UInt](reg_status.toBits))
      rdata := elts(UInt(0))

      reg_status := new Status().fromBits(wdata)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new VecComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_VecComp_1.v",
"""module NameSuite_VecComp_1(input clk,
    input  io_r_en,
    input [4:0] io_r_addr,
    input [63:0] io_w_data,
    output[7:0] io_status_im
);

  reg[7:0] reg_status_im;
  wire[7:0] T0;
  wire[63:0] wdata;
  reg[63:0] host_pcr_bits_data;
  wire[63:0] T1;
  wire[7:0] rdata;
  wire[7:0] elts_0;
  wire[7:0] T2;
  wire[63:0] io_pcr_req_data;

  assign io_status_im = reg_status_im;
  assign T0 = wdata[3'h7/* 7*/:1'h0/* 0*/];
  assign wdata = io_r_en ? io_w_data : host_pcr_bits_data;
  assign T1 = {56'h0/* 0*/, rdata};
  assign rdata = elts_0;
  assign elts_0 = T2;
  assign T2 = {reg_status_im};

  always @(posedge clk) begin
    reg_status_im <= T0;
    if(io_r_en) begin
      host_pcr_bits_data <= T1;
    end
  end
endmodule

""")
  }

  /** Names for input/output vectors should be generated with index (0-3).
    */
  @Test def testVecSecond() {
    class BlockReq extends Bundle {
      val ready = Bool()

      override def clone = new BlockReq().asInstanceOf[this.type]
    }

    class BlockIO extends Bundle {
      val req = Decoupled(new BlockReq)
    }

    class VecSecondComp extends Module {
      val io = new Bundle {
        val requestor = Vec.fill(4) { new BlockIO() }.flip
        val mem = Bool(OUTPUT)
      }

      io.mem := io.requestor(0).req.ready
      val r_valid = io.requestor.map(r => Reg(next=r.req.ready))

      for(i <- 0 to 3) {
        when (r_valid(i)) {
          io.mem := io.requestor(i).req.ready
        }
      }
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new VecSecondComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_VecSecondComp_1.v",
"""module NameSuite_VecSecondComp_1(input clk,
    output io_requestor_0_req_ready,
    input  io_requestor_0_req_valid,
    input  io_requestor_0_req_bits_ready,
    output io_requestor_1_req_ready,
    input  io_requestor_1_req_valid,
    input  io_requestor_1_req_bits_ready,
    output io_requestor_2_req_ready,
    input  io_requestor_2_req_valid,
    input  io_requestor_2_req_bits_ready,
    output io_requestor_3_req_ready,
    input  io_requestor_3_req_valid,
    input  io_requestor_3_req_bits_ready,
    output io_mem
);

  wire T0;
  wire T1;
  wire T2;
  wire T3;
  reg[0:0] r_valid_0;
  reg[0:0] r_valid_1;
  reg[0:0] r_valid_2;
  reg[0:0] r_valid_3;

  assign io_mem = T0;
  assign T0 = r_valid_3 ? io_requestor_3_req_ready : T1;
  assign T1 = r_valid_2 ? io_requestor_2_req_ready : T2;
  assign T2 = r_valid_1 ? io_requestor_1_req_ready : T3;
  assign T3 = r_valid_0 ? io_requestor_0_req_ready : io_requestor_0_req_ready;

  always @(posedge clk) begin
    r_valid_0 <= io_requestor_0_req_ready;
    r_valid_1 <= io_requestor_1_req_ready;
    r_valid_2 <= io_requestor_2_req_ready;
    r_valid_3 <= io_requestor_3_req_ready;
  end
endmodule

""")
  }


  /** This test checks names are correctly generated in the presence
    of multiple instantiation of the same Module. */
  @Test def testVariation() {
    class BlockIO extends Bundle {
      val valid = Bool(INPUT)
      val replay = Bool(OUTPUT)
    }

    class CompBlock(width: Int) extends Module {
      val io = new BlockIO();

      if( width > 8) {
        io.replay := io.valid
      } else {
        io.replay := Bool(false);
      }
    }

    class VariationComp extends Module {
      val io = new BlockIO();
      val block_0 = Module(new CompBlock(8));
      val block_1 = Module(new CompBlock(8));
      val block_2 = Module(new CompBlock(16));

      block_0.io.valid := io.valid;
      block_1.io.valid := io.valid;
      block_2.io.valid := io.valid;
      io.replay := block_0.io.replay & block_1.io.replay & block_2.io.replay;
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new VariationComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_VariationComp_1.v",
"""module NameSuite_CompBlock_1_0(
    input  io_valid,
    output io_replay
);


  assign io_replay = 1'h0/* 0*/;
endmodule

module NameSuite_CompBlock_1_1(
    input  io_valid,
    output io_replay
);


  assign io_replay = io_valid;
endmodule

module NameSuite_VariationComp_1(
    input  io_valid,
    output io_replay
);

  wire T0;
  wire block_2_io_replay;
  wire T1;
  wire block_1_io_replay;
  wire block_0_io_replay;

  assign io_replay = T0;
  assign T0 = T1 & block_2_io_replay;
  assign T1 = block_0_io_replay & block_1_io_replay;
  NameSuite_CompBlock_1_0 block_0(
       .io_valid( io_valid ),
       .io_replay( block_0_io_replay )
  );
  NameSuite_CompBlock_1_0 block_1(
       .io_valid( io_valid ),
       .io_replay( block_1_io_replay )
  );
  NameSuite_CompBlock_1_1 block_2(
       .io_valid( io_valid ),
       .io_replay( block_2_io_replay )
  );
endmodule

""")
  }

  /** Generated names for memories which are actual modules (ie. not inlined).
    */
  @Test def testMemComp() {
    val SZ_BREGLEN = 8
    val SZ_DATA = 65

    class RegfileIO extends Bundle {
      val ren    = Bool(INPUT)
      val raddr  = UInt(INPUT, SZ_BREGLEN)
      val rdata = UInt(OUTPUT, SZ_DATA)
    }

    class MemComp extends Module {
      val io = new RegfileIO()

      val rfile = Mem(UInt(width = SZ_DATA), 256, seqRead = true)
      val raddr = Reg(UInt())
      when (io.ren) { raddr := io.raddr }
      io.rdata := rfile(raddr)
    }

    chiselMain(Array[String]("--v", "--noInlineMem",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new MemComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_MemComp_1.v",
"""module NameSuite_MemComp_1(input clk, input reset,
    input  io_ren,
    input [7:0] io_raddr,
    output[64:0] io_rdata
);

  wire[64:0] T0;
  reg[7:0] raddr;

  assign io_rdata = T0;
  NameSuite_MemComp_1_rfile rfile (
    .CLK(clk),
    .RST(reset),
    .R0A(io_raddr),
    .R0E(io_ren),
    .R0O(T0)
  );

  always @(posedge clk) begin
    if(io_ren) begin
      raddr <= io_raddr;
    end
  end
endmodule

""")
  }

  /* Add signals which are not registers to the toplevel C++ class declaration.
   */
  @Test def testDebug() {
    class Block extends Module {
      val io = new Bundle {
        val ctrl_wb_wen = Bool(INPUT);
        val ctrl_out = Bool(OUTPUT);
      }
      // writeback definitions
      val wb_reg_ll_wb          = Reg(init=Bool(false));
      val wb_wen = io.ctrl_wb_wen || wb_reg_ll_wb

      when (wb_wen) { wb_reg_ll_wb := io.ctrl_wb_wen }
      io.ctrl_out := wb_reg_ll_wb

      // expose debug signals to testbench
      debug(wb_wen)
    }

    class DebugComp extends Module {
      val io = new Bundle {
        val ctrl_wb_wen = Bool(INPUT);
        val ctrl_out = Bool(OUTPUT);
      }

      val dpath = Module(new Block)
      dpath.io.ctrl_wb_wen := io.ctrl_wb_wen
      io.ctrl_out := dpath.io.ctrl_out
    }

    chiselMain(Array[String]("--backend", "c", "--vcd",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new DebugComp))
    assertFile(tmpdir.getRoot() + "/NameSuite_DebugComp_1.h",
"""#ifndef __NameSuite_DebugComp_1__
#define __NameSuite_DebugComp_1__

#include "emulator.h"

class NameSuite_DebugComp_1_t : public mod_t {
 public:
  dat_t<1> NameSuite_DebugComp_1_dpath__reset;
  dat_t<1> NameSuite_DebugComp_1_dpath__reset__prev;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_wb_wen;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_wb_wen__prev;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_wen;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_out;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_out__prev;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_out;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_out__prev;
  int clk;
  int clk_cnt;

  void init ( bool rand_init = false );
  void clock_lo ( dat_t<1> reset );
  void clock_hi ( dat_t<1> reset );
  int clock ( dat_t<1> reset );
  void print ( FILE* f );
  bool scan ( FILE* f );
  void dump ( FILE* f, int t );
};

#endif
""")
    assertFile(tmpdir.getRoot() + "/NameSuite_DebugComp_1.cpp",
"""#include "NameSuite_DebugComp_1.h"

void NameSuite_DebugComp_1_t::init ( bool rand_init ) {
  if (rand_init) NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.randomize();
}
void NameSuite_DebugComp_1_t::clock_lo ( dat_t<1> reset ) {
  { NameSuite_DebugComp_1_dpath__reset.values[0] = reset.values[0]; }
  { NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0] = NameSuite_DebugComp_1__io_ctrl_wb_wen.values[0]; }
  { NameSuite_DebugComp_1_dpath__wb_wen.values[0] = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0]||NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0]; }
  val_t T0__w0;
  { val_t __mask = -NameSuite_DebugComp_1_dpath__wb_wen.values[0]; T0__w0 = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0] ^ ((NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0] ^ NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen.values[0]) & __mask); }
  { NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow.values[0] = TERNARY(NameSuite_DebugComp_1_dpath__reset.values[0], 0x0L, T0__w0); }
  { NameSuite_DebugComp_1_dpath__io_ctrl_out.values[0] = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb.values[0]; }
  { NameSuite_DebugComp_1__io_ctrl_out.values[0] = NameSuite_DebugComp_1_dpath__io_ctrl_out.values[0]; }
}
void NameSuite_DebugComp_1_t::clock_hi ( dat_t<1> reset ) {
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow;
}
int NameSuite_DebugComp_1_t::clock ( dat_t<1> reset ) {
  uint32_t min = ((uint32_t)1<<31)-1;
  if (clk_cnt < min) min = clk_cnt;
  clk_cnt-=min;
  if (clk_cnt == 0) clock_lo( reset );
  if (clk_cnt == 0) clock_hi( reset );
  if (clk_cnt == 0) clk_cnt = clk-1;
  return min;
}
void NameSuite_DebugComp_1_t::print ( FILE* f ) {
}
bool NameSuite_DebugComp_1_t::scan ( FILE* f ) {
  return(!feof(f));
}
void NameSuite_DebugComp_1_t::dump(FILE *f, int t) {
  if (t == 0) {
    fprintf(f, "$timescale 1ps $end\n");
    fprintf(f, "$scope module NameSuite_DebugComp_1 $end\n");
    fprintf(f, "$var wire 1 N0 reset $end\n");
    fprintf(f, "$var wire 1 N2 io_ctrl_wb_wen $end\n");
    fprintf(f, "$var wire 1 N6 io_ctrl_out $end\n");
    fprintf(f, "$scope module dpath $end\n");
    fprintf(f, "$var wire 1 N1 reset $end\n");
    fprintf(f, "$var wire 1 N3 io_ctrl_wb_wen $end\n");
    fprintf(f, "$var wire 1 N4 wb_reg_ll_wb $end\n");
    fprintf(f, "$var wire 1 N5 io_ctrl_out $end\n");
    fprintf(f, "$upscope $end\n");
    fprintf(f, "$upscope $end\n");
    fprintf(f, "$enddefinitions $end\n");
    fprintf(f, "$dumpvars\n");
    fprintf(f, "$end\n");
  }
  fprintf(f, "#%d\n", t);
  if (t == 0 || (NameSuite_DebugComp_1_dpath__reset != NameSuite_DebugComp_1_dpath__reset__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1_dpath__reset, "N1");
  NameSuite_DebugComp_1_dpath__reset__prev = NameSuite_DebugComp_1_dpath__reset;
  if (t == 0 || (NameSuite_DebugComp_1__io_ctrl_wb_wen != NameSuite_DebugComp_1__io_ctrl_wb_wen__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1__io_ctrl_wb_wen, "N2");
  NameSuite_DebugComp_1__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1__io_ctrl_wb_wen;
  if (t == 0 || (NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen != NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen, "N3");
  NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen__prev = NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
  if (t == 0 || (NameSuite_DebugComp_1_dpath__wb_reg_ll_wb != NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1_dpath__wb_reg_ll_wb, "N4");
  NameSuite_DebugComp_1_dpath__wb_reg_ll_wb__prev = NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  if (t == 0 || (NameSuite_DebugComp_1_dpath__io_ctrl_out != NameSuite_DebugComp_1_dpath__io_ctrl_out__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1_dpath__io_ctrl_out, "N5");
  NameSuite_DebugComp_1_dpath__io_ctrl_out__prev = NameSuite_DebugComp_1_dpath__io_ctrl_out;
  if (t == 0 || (NameSuite_DebugComp_1__io_ctrl_out != NameSuite_DebugComp_1__io_ctrl_out__prev).to_bool())
    dat_dump(f, NameSuite_DebugComp_1__io_ctrl_out, "N6");
  NameSuite_DebugComp_1__io_ctrl_out__prev = NameSuite_DebugComp_1__io_ctrl_out;
}
""")
  }

}
