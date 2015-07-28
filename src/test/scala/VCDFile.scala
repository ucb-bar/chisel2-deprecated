/*
 Copyright (c) 2011, 2012, 2013, 2015 The Regents of the University of
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

import Chisel._
import collection.mutable.HashMap
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

class VCDFile extends Module {
  val io = new Bundle {
    val out0 = Bool(OUTPUT)
    val out1 = Bool(OUTPUT)
    val out2 = Bool(OUTPUT)
  }

  val clk1 = new Clock()
  val clk2 = new Clock()
  val reg0 = Reg(outType=Bool(), init=Bool(false))
  val reg1 = Reg(outType=Bool(), init=Bool(false), clock = clk1)
  val reg2 = Reg(outType=Bool(), init=Bool(false), clock = clk2)

  reg0 := !reg0
  reg1 := !reg1
  reg2 := !reg2

  io.out0 := reg0
  io.out1 := reg1
  io.out2 := reg2
}

class VCDFileTester(c : VCDFile) extends Tester(c) {
  setClocks(HashMap(Driver.implicitClock -> 5, c.clk1 -> 6, c.clk2 -> 7))
  step(25)
}

class VCDSuite extends TestSuite {
	@Test def testVCDFile() {
    chiselMainTest(Array[String]("--backend", "c", "--targetDir", dir.getPath.toString(), "--genHarness", 
      "--compile", "--test", "--vcd"), () => Module(new VCDFile())) {
      c => new VCDFileTester(c)
    }
    assertFile("VCDFile.vcd")
  }
}

