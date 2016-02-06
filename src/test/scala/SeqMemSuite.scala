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

import Chisel._
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

class SeqMemSuite extends TestSuite {
  // Test out creating a Sequential Memory
  @Test def testSeqMemCreate() {
    println("\ntestSeqMemCreate ...")
    class CreateSeqMem(size: Integer) extends Module {
      val io = new Bundle {
        val wEnable = Bool(INPUT)
        val rEnable = Bool(INPUT)
        val addr    = UInt(INPUT, log2Ceil(size))
        val value   = UInt(INPUT, 32)
        val out    = UInt(OUTPUT, 32)
      }
      val mem = SeqMem(size, UInt(width = 32))
      when(io.wEnable) {
        mem.write(io.addr, io.value)
      }
      val rdata = mem.read(io.addr, io.rEnable)
      io.out := rdata
    }

    class CreateSeqMemTester(c: CreateSeqMem, size: Int) extends Tester(c) {
      for (t <- 0 until 4) {
        val test_addr = rnd.nextInt(size)
        val test_value = rnd.nextInt(log2Ceil(size))
        poke(c.io.addr, test_addr)
        poke(c.io.value, test_value)
        poke(c.io.wEnable, 1)
        poke(c.io.rEnable, 1)
        step(2)
        expect(c.io.out, test_value)
      }
    }
    val size = 1024
    val testArgs = chiselEnvironmentArguments() ++ Array("--compile", "--genHarness", "--test", "--targetDir", dir.getPath)
    chiselMainTest(testArgs, () => Module(new CreateSeqMem(size))){
      c => new CreateSeqMemTester(c, size)}
  }
}
