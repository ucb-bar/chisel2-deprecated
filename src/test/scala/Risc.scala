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
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._

class RiscSuite extends TestSuite {

  @Test def testRisc() {
    println("\ntestRisc...")
    class Risc extends Module {
      val io = new Bundle {
        val isWr   = Bool(INPUT)
        val wrAddr = UInt(INPUT, 8)
        val wrData = Bits(INPUT, 32)
        val boot   = Bool(INPUT)
        val valid  = Bool(OUTPUT)
        val out    = Bits(OUTPUT, 32)
      }
      val file = Mem(256, Bits(width = 32))
      val code = Mem(256, Bits(width = 32))
      val pc   = Reg(init=UInt(0, 8))

      val add_op :: imm_op :: Nil = Enum(Bits(width = 8), 2)

      val inst = code(pc)
      val op   = inst(31,24)
      val rci  = inst(23,16)
      val rai  = inst(15, 8)
      val rbi  = inst( 7, 0)

      val ra = Mux(rai === Bits(0), Bits(0), file(rai))
      val rb = Mux(rbi === Bits(0), Bits(0), file(rbi))
      val rc = Bits(width = 32)

      io.valid := Bool(false)
      io.out   := Bits(0)
      rc       := Bits(0)

      when (io.isWr) {
        code(io.wrAddr) := io.wrData
      } .elsewhen (io.boot) {
        pc := UInt(0)
      } .otherwise {
        switch(op) {
          is(add_op) { rc := ra + rb }
          is(imm_op) { rc := (rai << UInt(8)) | rbi }
        }
        io.out := rc
        when (rci === UInt(255)) {
          io.valid := Bool(true)
        } .otherwise {
          file(rci) := rc
        }
        pc := pc + UInt(1)
      }
    }

    class RiscTester(c: Risc) extends Tester(c) {
      def wr(addr: BigInt, data: BigInt)  = {
        poke(c.io.isWr,   1)
        poke(c.io.wrAddr, addr)
        poke(c.io.wrData, data)
        step(1)
      }
      def boot()  = {
        poke(c.io.isWr, 0)
        poke(c.io.boot, 1)
        step(1)
      }
      def I (op: UInt, rc: Int, ra: Int, rb: Int) = {
        // val cr = Cat(op, UInt(rc, 8), UInt(ra, 8), UInt(rb, 8)).litValue()
        val cr = op.litValue() << 24 | rc << 16 | ra << 8 | rb
        println("I = " + cr)
        cr
      }

      val app  = Array(I(c.imm_op,   1, 0, 1), // r1 <- 1
                       I(c.add_op,   1, 1, 1), // r1 <- r1 + r1
                       I(c.add_op,   1, 1, 1), // r1 <- r1 + r1
                       I(c.add_op, 255, 1, 0)) // rh <- r1
      wr(0, 0) // skip reset
      for (addr <- 0 until app.length)
        wr(addr, app(addr))
      def dump(k: Int) {
        println("K = " + k)
        peek(c.ra)
        peek(c.rb)
        peek(c.rc)
        peek(c.io.out)
        peek(c.pc)
        peek(c.inst)
        peek(c.op)
        peek(c.rci)
        peek(c.rai)
        peek(c.rbi)
        peekAt(c.file, 1)
      }
      boot()
      dump(0)
      poke(c.io.boot, 0)
      var k = 0
      do {
        val expectedVal = peek(c.rc)
        step(1)
        k += 1
        dump(k)
        expect(peekAt(c.file, 1) == expectedVal, "memory check")
      } while (!(peek(c.io.valid) == 1 || k > 10))
      expect(k <= 10, "TIME LIMIT")
      expect(c.io.out, 4)
    }

    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
          "--backend", "c", "--genHarness", "--compile", "--test", "--debug")
    chiselMainTest(testArgs, () => Module(new Risc())){ c => new RiscTester(c) }
  }
}

