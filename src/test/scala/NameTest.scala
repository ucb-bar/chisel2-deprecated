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

import Chisel._

/* XXX Throws an error "Test NodeSuite.testBindComp3 failed: Parameterized
 Bundle class NodeSuite$Status$1 needs clone method" when Status is declared
 inside testBindComp3 instead of here at the top level. */
class Status extends Bundle {
  val im = Bits(width = 8)
}


/** This testsuite checks the naming of variables in the generated
  verilog and C++ code.
*/
class NameSuite extends AssertionsForJUnit {

  /** Checks names are correctly generated in the presence
   of ListLookups. */
  @Test def testListLookups() {
    trait Constants {
      val VXCPTHOLD  = Bits("b00000_00000_00000_0001001110_1111011", 32)
      val VCMD_X = UFix(0, 3)
      val VIMM_X = UFix(0, 1)
      val N = Bits(0, 1);
    }

    class ListLookupsComp extends Component with Constants {
      val io = new Bundle {
        val inst = Bits(INPUT, 32)
        val sigs_valid = Bool(OUTPUT)
      }
      val veccs =
        ListLookup(io.inst,
          List(N, VCMD_X, VIMM_X,   VIMM_X,  N),
          Array(VXCPTHOLD-> List(N, VCMD_X, VIMM_X, VIMM_X,  N)))

      val valid :: veccs0 = veccs
      io.sigs_valid := valid.toBool
    }

    chiselMain(Array[String]("--v", "--c"), () => new ListLookupsComp())
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
      val dec = new BlockDecoder();

      val valid_common = io.valid;

      val mask_cmdq_ready = !dec.io.sigs.enq_cmdq;
      val mask_ximm1q_ready = !dec.io.sigs.enq_ximm1q;

      io.replay := valid_common && (
        !mask_cmdq_ready || !mask_ximm1q_ready);

      val dec_replay = dec.io.replay;
    }

    chiselMain(Array[String]("--v", "--c"), () => new BindFirstComp())
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
        val vec = new Block();
        vec.io.irq := io.irq;
        io.irq_cause := UFix(1) ## vec.io.irq_cause;
      }
    }

    chiselMain(Array[String]("--v", "--c"), () => new BindSecondComp(true))
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

    class BindThird extends Component {
      val io = new Bundle() {
        val in = new BankToBankIO()
        val result = Bool(OUTPUT)
      }

      val conn = new ArrayBuffer[BankToBankIO]

      var first = true
      for (i <- 0 until 4) {
        val bank = new Comp()
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

    chiselMain(Array[String]("--v", "--c"), () => new BindThird())
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

      var norms = ArrayBuffer[Bits]();
      norms += io.in;
      io.out := norms.last
    }

    chiselMain(Array[String]("--v", "--c"), () => new BindFourthComp())
  }

  /* An ArrayBuffer is added to after initialization, then later
   the last entry of the ArrayBuffer is attached to a io port
   of a subcomponent.
   */
  @Test def testBindFith() {

    class BlockIO extends Bundle {
      val resp = new PipeIO()(new Bundle {
        val error = Bool()
        val ppn = UFix(width = 32)
      }).flip
    }

    class Block extends Component {
      val io = new Bundle {
        val valid = Bool(INPUT)
        val mine = Vec(2) { UFix(width = 32) }.asOutput
        val sub = new BlockIO()
      }
      val tag_ram = Vec(2) { Reg() { io.sub.resp.bits.ppn.clone } }
      when (io.valid) {
        tag_ram(UFix(0)) := io.sub.resp.bits.ppn
      }
      io.mine := Mux(io.valid, Mux1H(UFix(1), tag_ram), Mux1H(UFix(0), tag_ram))
    }

    class BindFithComp extends Component {
      val io = new Bundle() {
        val imem_ptw = new BlockIO()
        val dmem_ptw = new BlockIO()
        val resp = new BlockIO().asOutput
      }

      val ptw = collection.mutable.ArrayBuffer(io.imem_ptw, io.dmem_ptw)
      if( true ) {
        val vdtlb = new Block()
        ptw += vdtlb.io.sub
        vdtlb.io <> io.imem_ptw
      }
      io.resp := ptw(0)
    }

    chiselMain(Array[String]("--v", "--c"), () => new BindFithComp)
  }

  /** Appending index to a node name in Vec::apply
    without setting *named* to true.
    */
  @Test def testVec() {
    class VecComp extends Component {
      val io = new Bundle {
        val pcr_req_data = Bits(width = 64)

        val r_en   = Bool(INPUT)
        val r_addr = UFix(INPUT, log2Up(32))
        val w_data = Bits(INPUT, 64)

        val status = new Status().asOutput
      }

      val reg_status = Reg{new Status}
      val rdata = Bits();

      val host_pcr_bits_data = Reg{io.pcr_req_data.clone}
      when (io.r_en) {
        host_pcr_bits_data := rdata
      }

      val wdata = Mux(io.r_en, io.w_data, host_pcr_bits_data)

      io.status := reg_status

      val elts = List[Bits](reg_status.toBits)
      rdata := Vec(elts) { elts.head.clone }(UFix(0))

      reg_status := new Status().fromBits(wdata)
    }

    chiselMain(Array[String]("--v", "--c"), () => new VecComp())
  }

  /** Names for input/output vectors should be generated with index (0-3).
    */
  @Test def testVecSecond() {
    class BlockReq extends Bundle {
      val ready = Bool()

      override def clone = new BlockReq().asInstanceOf[this.type]
    }

    class BlockIO extends Bundle {
      val req = (new FIFOIO){ new BlockReq }
    }

    class VecSecondComp extends Component {
      val io = new Bundle {
        val requestor = Vec(4) { new BlockIO() }.flip
        val mem = Bool(OUTPUT)
      }

      io.mem := io.requestor(0).req.ready
      val r_valid = io.requestor.map(r => Reg(r.req.ready))

      for(i <- 0 to 3) {
        when (r_valid(i)) {
          io.mem := io.requestor(i).req.ready
        }
      }
    }

    chiselMain(Array[String]("--v", "--c"), () => new VecSecondComp())
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
      val block_0 = new CompBlock(8);
      val block_1 = new CompBlock(8);
      val block_2 = new CompBlock(16);

      block_0.io.valid := io.valid;
      block_1.io.valid := io.valid;
      block_2.io.valid := io.valid;
      io.replay := block_0.io.replay & block_1.io.replay & block_2.io.replay;
    }

    chiselMain(Array[String]("--v", "--c"), () => new VariationComp())
  }

  /** Generated names for memories which are actual modules (ie. not inlined).
    */
  @Test def testMemComp() {
    val SZ_BREGLEN = 8
    val SZ_DATA = 65

    class RegfileIO extends Bundle {
      val ren    = Bool(INPUT)
      val raddr  = Bits(INPUT, SZ_BREGLEN)
      val rdata = Bits(OUTPUT, SZ_DATA)
    }

    class MemComp extends Component {
      val io = new RegfileIO()

      val rfile = Mem(256, seqRead = true) { Bits(width = SZ_DATA) }
      val raddr = Reg() { Bits() }
      when (io.ren) { raddr := io.raddr }
      io.rdata := rfile(raddr)
    }

    chiselMain(Array[String]("--v", "--noInlineMem"), () => new MemComp())
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
      val wb_reg_ll_wb          = Reg(resetVal = Bool(false));
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

      val dpath = new Block
      dpath.io.ctrl_wb_wen := io.ctrl_wb_wen
      io.ctrl_out := dpath.io.ctrl_out
    }

    chiselMain(Array[String]("--c"), () => new DebugComp)
  }

}
