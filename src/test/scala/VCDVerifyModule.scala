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

import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import scala.collection.mutable.HashMap
import scala.sys.process._

import Chisel._

object VCDVerifyModule {
  val backends = List("c") /* ++
    (if (Driver.isVCSAvailable) {
      "v" :: Nil
    } else {
      Nil
    }
    ) */
}

class VCDVerifyModule extends TestSuite {

  // Verify VCD outputs.
  // Use Palmer's vcddiff if it is available (in the default PATH)
  val cmd = "vcddiff"
  lazy val isVCDDiffAvailable = Driver.isCommandAvailable(cmd)
  def assertVCDFile(fileName: String) {
    if (isVCDDiffAvailable) {
      val master = getClass.getResource(fileName)
      // Did we find the resource?
      if (master == null) {
        println("assertVCDFile: \"%s\" not found".format(fileName))
        // Make sure we don't inadvertently pass this test.
        assertResult(fileName) { "" }
        return
      }
      val masterName = master.getPath()
      val testName = dir.getPath + "/" + fileName
      // vcsdiff counts each clock change as a cycle/step, so to skip all the cycles during reset, we tell it to skip 10 steps.
      val resetCycles= (5 * 2).toString
      val cmdAndArgs = Seq(cmd, "--skip", resetCycles, masterName, testName)
      println(cmdAndArgs.mkString(" "))
      val result = Process(cmdAndArgs).! == 0
      assert(result == true, "%s %s failed".format(cmd, fileName))
    } else {
      assertFile(fileName)
    }
  }

  @Test def verifyVCD1() {
    class Hz extends Module {
      val io = new Bundle {
              val input = Bool(INPUT)
              val output = Bool(OUTPUT)
      }
      val reg = Reg(init = Bool(false))
      when (io.input) {
              reg := Bool(true)
      }
      io.output := reg
    }
    
    class Top extends Module {
      val io = new Bundle {
              val input = Bool(INPUT)
              val output = Bool(OUTPUT)
      }
      val hz = Module(new Hz)
      hz.io <> io
    }
    
    class VCDTester(c: Top) extends Tester(c) {
      step(1)
      poke(c.io.input, true)
      step(5)
    }

    for (backend <- VCDVerifyModule.backends) {
      chiselMain(Array[String]("--backend", backend, "--compile", "--genHarness", "--test", "--vcd",
          "--targetDir", dir.getPath.toString()),
          () => Module(new Top()), (c: Top) => new VCDTester(c))
      assertVCDFile("VCDVerifyModule_Top_1.vcd")
    }
  }

  @Test def verifyVCD2() {
    class BubbleFifo(size: Int) extends Module {
      val io = new Bundle {
        val din = UInt(INPUT, size)
        val write = Bool(INPUT)
        val read = Bool(INPUT)
        val ready = Bool(OUTPUT)
        val full = Bool(OUTPUT)
        val dout = UInt(OUTPUT, size)
        // just for debugging
        val stateReg = Bool(OUTPUT)
      }
    
      val empty :: full :: Nil = Enum(UInt(), 2)
      val stateReg = Reg(init = empty)
      // TODO: maybe just specify the size and not a reset value
      val dataReg = Reg(init = Bits(0, size))
      
      when (stateReg === empty) {
        when (io.write) {
          stateReg := full
          dataReg := io.din
        }
      } . elsewhen(stateReg === full) {
        when (io.read) {
          stateReg := empty
        }
      } .otherwise {
        // There should not be an otherwise state
      }
    
      io.ready := (stateReg === empty)
      io.full := (stateReg === full)
      io.dout := dataReg
      
      io.stateReg := stateReg
    }
    
    
    /**
     * Test the design.
     */
    class VCDTester(dut: BubbleFifo) extends Tester(dut) {
    
      // some defaults for all signals
      poke(dut.io.write, 0)
      poke(dut.io.din, 0xab)
      poke(dut.io.read, 0)
      step(1)
      peek(dut.io.ready)
      
      // write into the buffer
      poke(dut.io.din, 0x12)
      poke(dut.io.write, 1)
      step(1)
      peek(dut.io.ready)
      
      poke(dut.io.din, 0x34)
      poke(dut.io.write, 0)
      step(1)
      peek(dut.io.ready)
      
      // read out
      poke(dut.io.read, 1)
      step(1)
      poke(dut.io.read, 0)
      step(1)
      
      // write next
      poke(dut.io.din, 0x56)
      poke(dut.io.write, 1)
      step(1)
      peek(dut.io.ready)
    }
    

    for (backend <- VCDVerifyModule.backends) {
      chiselMain(Array[String]("--backend", backend, "--compile", "--genHarness", "--test", "--vcd",
          "--targetDir", dir.getPath.toString()),
          () => Module(new BubbleFifo(8)), (c: BubbleFifo) => new VCDTester(c))
      assertVCDFile("VCDVerifyModule_BubbleFifo_1.vcd")
    }
  }

}

