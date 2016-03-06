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

import Chisel._


class RegVcdSuite extends TestSuite {

  // We currently ignore this test until assertVCDFile is updated to deal with redundant signal values.
  @Test def testRegVcd() {
    println("\ntestRegVcd...")
    class RegVcdTest(size: Int) extends Module {
      val io = new Bundle {
        val din = UInt(INPUT, size)
        val write = Bool(INPUT)
        val read = Bool(INPUT)
        val ready = Bool(OUTPUT)
        val full = Bool(OUTPUT)
        val dout = UInt(OUTPUT, size)
        // for debugging
        val stateReg = Bool(OUTPUT)
      }

      val empty :: full :: Nil = Enum(UInt(), 2)
      val stateReg = Reg(init = empty)
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
      }

      io.ready := (stateReg === empty)
      io.full := (stateReg === full)
      io.dout := dataReg

      io.stateReg := stateReg
    }


    /**
     * Test the design.
     */
    class RegVcdTestTester(dut: RegVcdTest) extends Tester(dut) {

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

    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
          "--genHarness", "--test", "--backend", "c", "--compile", "--vcd")
    chiselMainTest(testArgs, () => Module(new RegVcdTest(8))) {
          f => new RegVcdTestTester(f)
    }
    assertVCDFile("RegVcdSuite_RegVcdTest_1.vcd")
  }
}
