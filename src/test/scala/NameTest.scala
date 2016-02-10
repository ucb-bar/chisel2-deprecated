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
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

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
class NameSuite extends TestSuite {

  /** Checks names are correctly generated in the presence
   of ListLookups. */
  @Test def testListLookups() {
    println("\nRunning testListLookups:")

    trait Constants {
      val VXCPTHOLD  = UInt("b00000_00000_00000_0001001110_1111011", 32)
      val VXCPTSAVE  = UInt("b00000_00000_00000_0001001010_1111011", 32)
      val VCMD_X = UInt(0, 3)
      val VCMD_S = UInt(1, 3)
      val VCMD_H = UInt(4, 3)
      val N = Bool(false)
      val Y = Bool(true)
    }

    class ListLookupsComp extends Module with Constants {
      val io = new Bundle {
        val inst = UInt(INPUT, 32)
        val sigs_valid = Bool(OUTPUT)
      }
      val veccs =
        ListLookup(io.inst,
                            List(Y, VCMD_X),
          Array(VXCPTHOLD-> List(Y, VCMD_H),
                VXCPTSAVE-> List(N, VCMD_S)))

      io.sigs_valid := (veccs(0) ^ veccs(1)(2)).toBool
    }

    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new ListLookupsComp()))
    assertFile("NameSuite_ListLookupsComp_1.v")
  }

  /** This test checks names are correctly generated in the presence
   of Binding. */
  @Test def testBindFirst() {
    println("\nRunning testBindFirst:")

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
      "--targetDir", dir.getPath.toString()),
      () => Module(new BindFirstComp()))
   assertFile("NameSuite_BindFirstComp_1.v")
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
    println("\nRunning testBindSecond:")

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
      "--targetDir", dir.getPath.toString()),
      () => Module(new BindSecondComp(true)))
   assertFile("NameSuite_BindSecondComp_1.v")
  }

  /** Propagation of bundle field names through bindings

    At the toplevel *Comp*, variables should be named conn_X
    instead of using a derived name derived from CompIO.
    But importantly io names should be derived from their io names.
    */
  @Test def testBindThird() {
    println("\nRunning testBindThird:")

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
      "--targetDir", dir.getPath.toString()),
      () => Module(new BindThirdComp()))
    assertFile("NameSuite_BindThirdComp_1.v")
  }

  /** Propagation of bundle field names through bindings

    In this case we don't want *norms* to propagate to the input/output
    of the module.
    */
  @Test def testBindFourth() {
    println("\nRunning testBindFourth:")

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
      "--targetDir", dir.getPath.toString()),
      () => Module(new BindFourthComp()))
    assertFile("NameSuite_BindFourthComp_1.v")
  }

  /* An ArrayBuffer is added to after initialization, then later
   the last entry of the ArrayBuffer is attached to a io port
   of a subcomponent.
   */
  @Test def testBindFifth() {
    println("\nRunning testBindFifth:")
    class UnamedBundle extends Bundle {
      val error = Bool()
      val ppn = UInt(width = 32)

      override def cloneType = new UnamedBundle().asInstanceOf[this.type]
    }

    class BlockIO extends Bundle {
      val resp = Valid(new UnamedBundle()).flip
    }

    class Block extends Module {
      val io = new Bundle {
        val in = new BlockIO()
        val out = new BlockIO().flip
      }
      val tag_ram = Reg(Vec(2, io.in.resp.bits.ppn))
      when (io.in.resp.valid) {
        tag_ram(UInt(0)) := io.in.resp.bits.ppn
      }
      io.out.resp.bits.ppn := Mux1H(tag_ram(0), tag_ram)
    }

    class BindFifthComp extends Module {
      val io = new Bundle {
        val imem_ptw = new BlockIO()
        val dmem_ptw = new BlockIO()
        val resp = new BlockIO().asOutput
      }

      val ptw = collection.mutable.ArrayBuffer(io.imem_ptw, io.dmem_ptw)
      val vdtlb = Module(new Block())
      io.resp := vdtlb.io.out
      ptw += vdtlb.io.in
      ptw.last <> io.imem_ptw
    }

    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new BindFifthComp))
    assertFile("NameSuite_BindFifthComp_1.v")
  }

  /** Appending index to a node name in Vec::apply
    without setting *named* to true.
    */
  @Test def testVec() {
    println("\nRunning testVec:")
    class VecComp extends Module {
      val io = new Bundle {
        val pcr_req_data = UInt(INPUT, width = 64)

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
      "--targetDir", dir.getPath.toString()),
      () => Module(new VecComp()))
    assertFile("NameSuite_VecComp_1.v")
  }

  /** Names for input/output vectors should be generated with index (0-3).
    */
  @Test def testVecSecond() {
    class BlockReq extends Bundle {
      val ready = Bool()

      override def cloneType = new BlockReq().asInstanceOf[this.type]
    }

    class BlockIO extends Bundle {
      val req = Decoupled(new BlockReq)
    }

    class VecSecondComp extends Module {
      val io = new Bundle {
        val requestor = Vec(4,   new BlockIO() ).flip
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
      "--targetDir", dir.getPath.toString()),
      () => Module(new VecSecondComp()))
    assertFile("NameSuite_VecSecondComp_1.v")
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
      "--targetDir", dir.getPath.toString()),
      () => Module(new VariationComp()))
    assertFile("NameSuite_VariationComp_1.v")
  }

  /** Generated names for memories which are actual modules (ie. not inlined).
    */
  @Test def testMemComp() {
    println("\nRunning testMemComp:")

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

    chiselMain(Array[String]("--noInlineMem", "--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new MemComp()))
    assertFile("NameSuite_MemComp_1.v")
  }

  /* Add signals which are not registers to the toplevel C++ class declaration.
   */
  @Test def testDebug() {
    println("testDebug:")
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
      "--targetDir", dir.getPath.toString()),
      () => Module(new DebugComp))
    assertFile("NameSuite_DebugComp_1.h")
    assertFile("NameSuite_DebugComp_1.cpp")
  }

  /* XXX test case derived from issue #6 on github.
   */
  @Test def testInputPortNameChange() {
    println("testInputPortNameChange:")
    class InputPortNameComp extends Module {
      val io = new Bundle {
        val in = Bits(INPUT, 20)
        val out = Bits(OUTPUT, 20)
      }

      val newName = io.in
      io.out := newName
    }

    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new InputPortNameComp))
    assertFile("NameSuite_InputPortNameComp_1.v");
  }

  /* XXX test case derived from issue #153 on github.
   */
  @Test def testNameItTooEager153() {
    println("testNameItTooEager153:")
    class MyBundle extends Bundle
    {
      val x = Bool()
      val y = Bool()
      val z = Bool()
    }

    class EntryIO(num_ports: Int) extends Bundle
    {
      val vals = Vec(num_ports,   Bool(INPUT) )
      val out = Bool(OUTPUT)
    }

    class Entry(num_ports: Int) extends Module
    {
      val io = new EntryIO(num_ports)
      io.out := io.vals.reduce(_|_)
    }

    class NameItTooEager153 extends Module {
     val io = new Bundle {
         val idx = UInt(INPUT,2)
         val vals = Vec(4,   Bool(INPUT) )
         val z = Bool(OUTPUT)
      }

      val entry_io = Vec(4,   Module(new Entry(4)).io )

      for (i <- 0 until 4)
      {
         for (j <- 0 until 4)
         {
            entry_io(i).vals(j) := io.vals(j)
         }
      }
    }

    chiselMain(Array[String]("--backend", "v",
        "--targetDir", dir.getPath.toString()),
        () => Module(new NameItTooEager153()))

  }

  class KeywordsModule extends Module {
    val io = new Bundle {
      val a = UInt(INPUT, 2)
      val z = UInt(OUTPUT, 2)
    }
    val begin = RegNext(io.a)
    val time  = RegNext(begin)
    val end   = RegNext(time)
    io.z := end
  }

  trait KeywordsModuleTestsCommon extends Tests {
    val values = Vector(3,2,1)
    def init(c: KeywordsModule) {
      values foreach { v =>
        poke(c.io.a, v)
        step(1)
      }
    }
  }

  class KeywordsModulePathTests(c: KeywordsModule) extends Tester(c) with KeywordsModuleTestsCommon {
    init(c)
    expect(peekPath("NameSuite_KeywordsModule.begin_") == 1, "begin -> begin_: ")
    expect(peekPath("NameSuite_KeywordsModule.time_") == 2, "time -> time_: ")
    expect(peekPath("NameSuite_KeywordsModule.end_") == 3, "end -> end_: ")
  }

  class KeywordsModuleNullTests(c: KeywordsModule) extends Tester(c) with KeywordsModuleTestsCommon {
    init(c)
    expect(c.begin, 1)
    expect(c.time, 2)
    expect(c.end, 3)
  }

  @Test def testKeywordsCpp() {
    println("testKeywordsCpp:")
    launchCppTester((c: KeywordsModule) => new KeywordsModulePathTests(c))
  }

  @Test def testKeywordsVerilog() {
    println("testKeywordsVerilog:")
    // We'd like to use something like assume() here, but it generates
    // a TestCanceledException. There should be a programmatic way to skip
    // tests (without failing) but make a note of the fact.
    if (!Driver.isVCSAvailable) {
      assert(true, "vcs unavailable - skipping testKeywordsVerilog")
    } else {
      launchVerilogTester((c: KeywordsModule) => new KeywordsModulePathTests(c))
    }
  }

  @Test def testKeywordsNull() {
    println("testKeywordsNull:")
    // Make sure we actually have a test to execute (the order of test runs is not defined,
    //  so testKeywordsCpp or testKeywordsVerilog may not have run yet.
    chiselMain(Array[String]("--backend", "c", "--genHarness", "--compile", "--targetDir", dir.getPath.toString()), () => Module(new KeywordsModule()))
    launchTester("null", (c: KeywordsModule) => new KeywordsModuleNullTests(c),
      Some((args: Array[String]) => args ++ Array("--testCommand", dir.getPath.toString() + "/NameSuite_KeywordsModule", "-q")))
  }

  /* Multiple directionless IO's don't throw assertion error - issue #459.
   */
  @Test def testMultipleDirectionlessIO459() {
    println("testMultipleDirectionlessIO459:")
    class MultipleDirectionlessIO459 extends Module {
      val io = new Bundle {
        val send = Reg(UInt(0, 8))
        val recv = Reg(UInt(0, 8))
      }
    }

    // This should fail since we don't assign a directiom to the IO ports.
    intercept[IllegalStateException] {
      chiselMain(Array[String]("--backend", "v",
          "--targetDir", dir.getPath.toString()),
          () => Module(new MultipleDirectionlessIO459()))
    }
    assertTrue(ChiselError.hasErrors)
  }
}
