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

//package ChiselTests
import org.junit.Test

import Chisel._

class ROMSuite extends TestSuite {

  @Test def testROM() {
    println("\ntestROM...")
    class DUT extends Module {

      val DATAWIDTH = 133

      def matchLookUpRows() = {
        val inits = (1 to 2).map(i => UInt(i, width = DATAWIDTH))
        ROM(inits)
      }

      val io = new Bundle {
        val m = UInt(INPUT, width = log2Up(DATAWIDTH))
        val fState = Bits(OUTPUT, width = DATAWIDTH)
      }

      val inpBitStrReg = RegInit(Bits(0, width = DATAWIDTH))

      inpBitStrReg := matchLookUpRows.read(io.m)
      io.fState := inpBitStrReg
    }

    class DUTTester(c: DUT) extends Tester(c) {
      poke(c.io.m, 0)
      peek(c.inpBitStrReg)
      step(1)
      peek(c.inpBitStrReg)
    }
    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
      "--backend", "c", "--genHarness", "--compile", "--test", "--debug")
    chiselMainTest(testArgs, () => Module(new DUT())){ c => new DUTTester(c) }
  }
}
