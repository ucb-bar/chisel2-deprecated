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
  val im = UFix(width = 8)
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
      val VXCPTHOLD  = UFix("b00000_00000_00000_0001001110_1111011", 32)
      val VCMD_X = UFix(0, 3)
      val VIMM_X = UFix(0, 1)
      val N = UFix(0, 1);
    }

    class ListLookupsComp extends Component with Constants {
      val io = new Bundle {
        val inst = UFix(INPUT, 32)
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
      () => module(new ListLookupsComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_ListLookupsComp_1.v",
"""module NameSuite_ListLookupsComp_1(
    input [31:0] io_inst,
    output io_sigs_valid);

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

    class BlockDecoder extends Component {
      val io = new Bundle {
        val valid = Bool(INPUT)
        val replay = Bool(OUTPUT)
        val sigs = new BlockSigs().asOutput
      }

      io.replay := io.valid;
    }

    class BindFirstComp extends Component {
      val io = new BlockIO
      val dec = module(new BlockDecoder());

      val valid_common = io.valid;

      val mask_cmdq_ready = !dec.io.sigs.enq_cmdq;
      val mask_ximm1q_ready = !dec.io.sigs.enq_ximm1q;

      io.replay := valid_common && (
        !mask_cmdq_ready || !mask_ximm1q_ready);

      val dec_replay = dec.io.replay;
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new BindFirstComp()))
   assertFile(tmpdir.getRoot() + "/NameSuite_BindFirstComp_1.v",
"""module NameSuite_BlockDecoder_1(
    input  io_valid,
    output io_replay,
    output io_sigs_enq_cmdq,
    output io_sigs_enq_ximm1q);


  assign io_replay = io_valid;
endmodule

module NameSuite_BindFirstComp_1(
    input  valid_common,
    output io_replay);

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
       .io_valid(  ),
       .io_replay(  ),
       .io_sigs_enq_cmdq( dec_io_sigs_enq_cmdq ),
       .io_sigs_enq_ximm1q( dec_io_sigs_enq_ximm1q ));
endmodule

""")
   }

  /** Since *vec* is declared within a if() block and not as a Component
    member, we do not extract the actual name of the module instance
    (i.e. *vec*) which means that if we do not set an arbitrary name on
    that component instance, it will remain empty.
    This would result in bindings starting with a "_" prefix (target
    Component name is empty).

    We thus generate a name for the component instance based on its class name.
    */
  @Test def testBindSecond() {
    class Block extends Component {
      val io = new Bundle() {
        val irq = Bool(INPUT)
        val irq_cause = UFix(OUTPUT, 5)
      }
      io.irq_cause := UFix(2);
    }

    class BindSecondComp(conf: Boolean) extends Component {
      val io = new Bundle() {
        val irq = Bool(INPUT)
        val irq_cause = UFix(OUTPUT, 6)
      }

      if( conf ) {
        val vec = module(new Block());
        vec.io.irq := io.irq;
        io.irq_cause := UFix(1) ## vec.io.irq_cause;
      }
    }

    chiselMain(Array[String]("--v", "--c",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new BindSecondComp(true)))
   assertFile(tmpdir.getRoot() + "/NameSuite_BindSecondComp_1.v",
"""module NameSuite_Block_1(
    input  io_irq,
    output[4:0] io_irq_cause);

  wire[4:0] T0;

  assign io_irq_cause = T0;
  assign T0 = {3'h0/* 0*/, 2'h2/* 2*/};
endmodule

module NameSuite_BindSecondComp_1(
    input  io_irq,
    output[5:0] io_irq_cause);

  wire[5:0] T0;
  wire[4:0] NameSuite_Block_1_io_irq_cause;

  assign io_irq_cause = T0;
  assign T0 = {1'h1/* 1*/, NameSuite_Block_1_io_irq_cause};
  NameSuite_Block_1 NameSuite_Block_1(
       .io_irq( io_irq ),
       .io_irq_cause( NameSuite_Block_1_io_irq_cause ));
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


    class Comp extends Component {
      val io = new CompIO()
      io.out.ren := io.in.ren
    }

    class BindThirdComp extends Component {
      val io = new Bundle() {
        val in = new BankToBankIO()
        val result = Bool(OUTPUT)
      }

      val conn = new ArrayBuffer[BankToBankIO]

      var first = true
      for (i <- 0 until 4) {
        val bank = module(new Comp())
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

    chiselMain(Array[String]("--v", "--c",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new BindThirdComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_BindThirdComp_1.v",
"""module NameSuite_Comp_1(
    input  io_in_ren,
    output io_out_ren);


  assign io_out_ren = io_in_ren;
endmodule

module NameSuite_BindThirdComp_1(
    input  io_in_ren,
    output io_result);

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
       .io_out_ren( NameSuite_Comp_1_0_io_out_ren ));
  NameSuite_Comp_1 NameSuite_Comp_1_1(
       .io_in_ren( NameSuite_Comp_1_0_io_out_ren ),
       .io_out_ren( NameSuite_Comp_1_1_io_out_ren ));
  NameSuite_Comp_1 NameSuite_Comp_1_2(
       .io_in_ren( NameSuite_Comp_1_1_io_out_ren ),
       .io_out_ren( NameSuite_Comp_1_2_io_out_ren ));
  NameSuite_Comp_1 NameSuite_Comp_1_3(
       .io_in_ren( NameSuite_Comp_1_2_io_out_ren ),
       .io_out_ren( NameSuite_Comp_1_3_io_out_ren ));
endmodule

""")
  }

  /** Propagation of bundle field names through bindings

    In this case we don't want *norms* to propagate to the input/output
    of the module.
    */
  @Test def testBindFourth() {
    class CompIO extends Bundle {
      val in = UFix(INPUT, 5)
      val out = UFix(OUTPUT, 5)
    }

    class BindFourthComp extends Component {
      val io = new CompIO()

      var norms = ArrayBuffer[UFix]();
      norms += io.in;
      io.out := norms.last
    }

    chiselMain(Array[String]("--v", "--c",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new BindFourthComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_BindFourthComp_1.v",
"""module NameSuite_BindFourthComp_1(
    input [4:0] io_in,
    output[4:0] io_out);


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
      val ppn = UFix(width = 32)

      override def clone = new UnamedBundle().asInstanceOf[this.type]
    }

    class BlockIO extends Bundle {
      val resp = new PipeIO(new UnamedBundle()).flip

      override def clone = new BlockIO().asInstanceOf[this.type]
    }

    class Block extends Component {
      val io = new Bundle {
        val valid = Bool(INPUT)
        val mine = Vec.fill(2){UFix(width = 32)}.asOutput
        val sub = new BlockIO()
      }
      val tag_ram = Vec.fill(2){ Reg(io.sub.resp.bits.ppn) }
      when (io.valid) {
        tag_ram(UFix(0)) := io.sub.resp.bits.ppn
      }
      io.mine := Mux(io.valid, Mux1H(UFix(1), tag_ram), Mux1H(UFix(0), tag_ram))
    }

    class BindFithComp extends Component {
      val io = new Bundle {
        val imem_ptw = new BlockIO()
        val dmem_ptw = new BlockIO()
        val resp = new BlockIO().asOutput
      }

      val ptw = collection.mutable.ArrayBuffer(io.imem_ptw, io.dmem_ptw)
      if( true ) {
        val vdtlb = module(new Block())
        ptw += vdtlb.io.sub
        vdtlb.io <> io.imem_ptw
      }
      io.resp := ptw(0)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new BindFithComp))
    assertFile(tmpdir.getRoot() + "/NameSuite_BindFithComp_1.v",
"""module NameSuite_Block_2(input clk, input reset,
    input  io_valid,
    output[31:0] io_mine_0,
    output[31:0] io_mine_1,
    input  io_sub_resp_valid,
    input  io_sub_resp_bits_error,
    input [31:0] io_sub_resp_bits_ppn);

  wire[31:0] T0;
  wire T1;
  wire[31:0] T2;
  wire[31:0] T3;
  wire[31:0] T4;
  wire[31:0] T5;
  reg[31:0] tag_ram_0;
  wire[31:0] T6;
  wire[31:0] T7;
  wire T8;

  assign io_mine_1 = T0;
  assign T0 = {31'h0/* 0*/, T1};
  assign T1 = T2[1'h1/* 1*/:1'h1/* 1*/];
  assign T2 = io_valid ? T4 : T3;
  assign T3 = {31'h0/* 0*/, 1'h0/* 0*/};
  assign T4 = tag_ram_0 | T5;
  assign T5 = {31'h0/* 0*/, 1'h0/* 0*/};
  assign T6 = 1'h1/* 1*/ ? io_sub_resp_bits_ppn : tag_ram_0;
  assign io_mine_0 = T7;
  assign T7 = {31'h0/* 0*/, T8};
  assign T8 = T2[1'h0/* 0*/:1'h0/* 0*/];

  always @(posedge clk) begin
    if(io_valid) begin
      tag_ram_0 <= T6;
    end
  end
endmodule

module NameSuite_BindFithComp_1(input clk, input reset,
    input  io_imem_ptw_resp_valid,
    input  io_imem_ptw_resp_bits_error,
    input [31:0] io_imem_ptw_resp_bits_ppn,
    input  io_dmem_ptw_resp_valid,
    input  io_dmem_ptw_resp_bits_error,
    input [31:0] io_dmem_ptw_resp_bits_ppn,
    output io_resp_resp_valid,
    output io_resp_resp_bits_error,
    output[31:0] io_resp_resp_bits_ppn);


  assign io_resp_resp_bits_ppn = io_imem_ptw_resp_bits_ppn;
  assign io_resp_resp_bits_error = io_imem_ptw_resp_bits_error;
  assign io_resp_resp_valid = io_imem_ptw_resp_valid;
  NameSuite_Block_2 NameSuite_Block_2(.clk(clk), .reset(reset),
       .io_valid(  ),
       .io_mine_0(  ),
       .io_mine_1(  ),
       .io_sub_resp_valid(  ),
       .io_sub_resp_bits_error(  ),
       .io_sub_resp_bits_ppn(  ));
endmodule

""")
  }

  /** Appending index to a node name in Vec::apply
    without setting *named* to true.
    */
  @Test def testVec() {
    class VecComp extends Component {
      val io = new Bundle {
        val pcr_req_data = UFix(width = 64)

        val r_en   = Bool(INPUT)
        val r_addr = UFix(INPUT, log2Up(32))
        val w_data = UFix(INPUT, 64)

        val status = new Status().asOutput
      }

      val reg_status = Reg(new Status)
      val rdata = UFix();

      val host_pcr_bits_data = Reg(io.pcr_req_data)
      when (io.r_en) {
        host_pcr_bits_data := rdata
      }

      val wdata = Mux(io.r_en, io.w_data, host_pcr_bits_data)

      io.status := reg_status

      val elts = Vec(List[UFix](reg_status.toBits))
      rdata := elts(UFix(0))

      reg_status := new Status().fromBits(wdata)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new VecComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_VecComp_1.v",
"""module NameSuite_VecComp_1(input clk, input reset,
    input  io_r_en,
    input [4:0] io_r_addr,
    input [63:0] io_w_data,
    output[7:0] io_status_im);

  reg[7:0] reg_status_im;
  wire[7:0] T0;
  wire[7:0] T1;
  wire[63:0] wdata;
  reg[63:0] host_pcr_bits_data;
  wire[63:0] T2;
  wire[63:0] T3;
  wire[7:0] rdata;
  wire[7:0] elts_0;
  wire[7:0] T4;
  wire[63:0] io_pcr_req_data;

  assign io_status_im = reg_status_im;
  assign T0 = 1'h1/* 1*/ ? T1 : reg_status_im;
  assign T1 = wdata[3'h7/* 7*/:1'h0/* 0*/];
  assign wdata = io_r_en ? io_w_data : host_pcr_bits_data;
  assign T2 = 1'h1/* 1*/ ? T3 : host_pcr_bits_data;
  assign T3 = {56'h0/* 0*/, rdata};
  assign rdata = elts_0;
  assign elts_0 = T4;
  assign T4 = {reg_status_im};

  always @(posedge clk) begin
    reg_status_im <= T0;
    if(io_r_en) begin
      host_pcr_bits_data <= T2;
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
      val req = new FIFOIO(new BlockReq)
    }

    class VecSecondComp extends Component {
      val io = new Bundle {
        val requestor = Vec.fill(4) { new BlockIO() }.flip
        val mem = Bool(OUTPUT)
      }

      io.mem := io.requestor(0).req.ready
      val r_valid = io.requestor.map(r => RegUpdate(r.req.ready))

      for(i <- 0 to 3) {
        when (r_valid(i)) {
          io.mem := io.requestor(i).req.ready
        }
      }
    }

    chiselMain(Array[String]("--v", "--c",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new VecSecondComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_VecSecondComp_1.v",
"""module NameSuite_VecSecondComp_1(input clk, input reset,
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
    output io_mem);

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
    of multiple instantiation of the same Component. */
  @Test def testVariation() {
    class BlockIO extends Bundle {
      val valid = Bool(INPUT)
      val replay = Bool(OUTPUT)
    }

    class CompBlock(width: Int) extends Component {
      val io = new BlockIO();

      if( width > 8) {
        io.replay := io.valid
      } else {
        io.replay := Bool(false);
      }
    }

    class VariationComp extends Component {
      val io = new BlockIO();
      val block_0 = module(new CompBlock(8));
      val block_1 = module(new CompBlock(8));
      val block_2 = module(new CompBlock(16));

      block_0.io.valid := io.valid;
      block_1.io.valid := io.valid;
      block_2.io.valid := io.valid;
      io.replay := block_0.io.replay & block_1.io.replay & block_2.io.replay;
    }

    chiselMain(Array[String]("--v", "--c",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new VariationComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_VariationComp_1.v",
"""module NameSuite_CompBlock_1_0(
    input  io_valid,
    output io_replay);


  assign io_replay = 1'h0/* 0*/;
endmodule

module NameSuite_CompBlock_1_1(
    input  io_valid,
    output io_replay);


  assign io_replay = io_valid;
endmodule

module NameSuite_VariationComp_1(
    input  io_valid,
    output io_replay);

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
       .io_replay( block_0_io_replay ));
  NameSuite_CompBlock_1_0 block_1(
       .io_valid( io_valid ),
       .io_replay( block_1_io_replay ));
  NameSuite_CompBlock_1_1 block_2(
       .io_valid( io_valid ),
       .io_replay( block_2_io_replay ));
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
      val raddr  = UFix(INPUT, SZ_BREGLEN)
      val rdata = UFix(OUTPUT, SZ_DATA)
    }

    class MemComp extends Component {
      val io = new RegfileIO()

      val rfile = Mem(256, UFix(width = SZ_DATA), seqRead = true)
      val raddr = Reg(UFix())
      when (io.ren) { raddr := io.raddr }
      io.rdata := rfile(raddr)
    }

    chiselMain(Array[String]("--v", "--noInlineMem",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new MemComp()))
    assertFile(tmpdir.getRoot() + "/NameSuite_MemComp_1.v",
"""module NameSuite_MemComp_1(input clk, input reset,
    input  io_ren,
    input [7:0] io_raddr,
    output[64:0] io_rdata);

  wire[64:0] T0;
  wire[7:0] T1;
  reg[7:0] raddr;

  assign io_rdata = T0;
  assign T1 = 1'h1/* 1*/ ? io_raddr : raddr;
  NameSuite_MemComp_1_rfile rfile (
    .CLK(clk),
    .RST(reset),
    .R0A(T1),
    .R0E(io_ren),
    .R0O(T0)
  );

  always @(posedge clk) begin
    if(io_ren) begin
      raddr <= T1;
    end
  end
endmodule

""")
  }

  /* Add signals which are not registers to the toplevel C++ class declaration.
   */
  @Test def testDebug() {
    class Block extends Component {
      val io = new Bundle {
        val ctrl_wb_wen = Bool(INPUT);
        val ctrl_out = Bool(OUTPUT);
      }
      // writeback definitions
      val wb_reg_ll_wb          = RegReset(Bool(false));
      val wb_wen = io.ctrl_wb_wen || wb_reg_ll_wb

      when (wb_wen) { wb_reg_ll_wb := io.ctrl_wb_wen }
      io.ctrl_out := wb_reg_ll_wb

      // expose debug signals to testbench
      debug(wb_wen)
    }

    class DebugComp extends Component {
      val io = new Bundle {
        val ctrl_wb_wen = Bool(INPUT);
        val ctrl_out = Bool(OUTPUT);
      }

      val dpath = module(new Block)
      dpath.io.ctrl_wb_wen := io.ctrl_wb_wen
      io.ctrl_out := dpath.io.ctrl_out
    }

    chiselMain(Array[String]("--c",
      "--targetDir", tmpdir.getRoot().toString()),
      () => module(new DebugComp))
    assertFile(tmpdir.getRoot() + "/NameSuite_DebugComp_1.h",
"""#ifndef __NameSuite_DebugComp_1__
#define __NameSuite_DebugComp_1__

#include "emulator.h"

class NameSuite_DebugComp_1_t : public mod_t {
 public:
  dat_t<1> NameSuite_DebugComp_1_dpath__reset;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_wb_wen;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_wb_wen;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_wen;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_reg_ll_wb;
  dat_t<1> NameSuite_DebugComp_1_dpath__wb_reg_ll_wb_shadow;
  dat_t<1> NameSuite_DebugComp_1_dpath__io_ctrl_out;
  dat_t<1> NameSuite_DebugComp_1__io_ctrl_out;

  void init ( bool rand_init = false );
  void clock_lo ( dat_t<1> reset );
  void clock_hi ( dat_t<1> reset );
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
void NameSuite_DebugComp_1_t::print ( FILE* f ) {
}
bool NameSuite_DebugComp_1_t::scan ( FILE* f ) {
  return(!feof(f));
}
void NameSuite_DebugComp_1_t::dump(FILE *f, int t) {
}
""")
  }

}
