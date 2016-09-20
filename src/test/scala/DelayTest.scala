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
import org.junit.Ignore
import org.junit.Test

import Chisel._


/** This testsuite checks all methods in the Bits class.
*/
class DelaySuite extends TestSuite {

  /** Uninitialized register, update on each clock. */
  @Test def testRegNoInitUpdate() {
    println("\ntestRegNoInitUpdate ...")
    class RegNoInitUpdate extends Module {
      val io = new Bundle() {
        val out = UInt(OUTPUT, 32)
      }
      // XXX BE CAREFUL UInt(32) will create a literal 0x20 6 bits wide.
      val res = Reg(UInt(width=32))
      res := res + UInt(1)
      io.out := res
    }
    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new RegNoInitUpdate()))
    assertFile("DelaySuite_RegNoInitUpdate_1.v")
  }

  /** Initialized register, update on each clock. */
  @Test def testRegInitUpdate() {
    println("\ntestRegInitUpdate ...")
    class RegInitUpdate extends Module {
      val io = new Bundle() {
        val out = UInt(OUTPUT, 32)
      }
      /* The width will be set to 1 bit while it will be set to 32 bit
       when we use backward propagation. Using either results is arbitrary
       and should propably lead to a warning.
       */
      val res = Reg(init=UInt(0))
      res := res + UInt(1)
      io.out := res
    }
    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new RegInitUpdate()))
    assertFile("DelaySuite_RegInitUpdate_1.v")
  }

  /** Initialized register, conditional update. */
  @Test def testRegInitCondUpdate() {
    println("\ntestRegInitCondUpdate ...")
    class RegInitCondUpdate extends Module {
      val io = new Bundle() {
        val in = Bool(INPUT)
        val out = UInt(OUTPUT, 32)
      }
      val res = Reg(init=UInt(0))
      when( io.in ) {
        res := res + UInt(1)
      }
      io.out := res
    }
    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new RegInitCondUpdate()))
    assertFile("DelaySuite_RegInitCondUpdate_1.v")
  }

  /** Uninitialized sram, one read on each clock. */
  @Test def testMemRead() {
    println("\ntestMemRead ...")
    class MemReadModule extends Module {
      val io = new Bundle() {
        val addr = UInt(INPUT, width=32)
        val out = UInt(OUTPUT)
      }
      val mem= Mem(8, UInt(width=32))
      io.out := mem(io.addr)
    }
    chiselMain(Array[String]("--v", "--inlineMem",
      "--targetDir", dir.getPath.toString()),
      () => Module(new MemReadModule()))
    assertFile("DelaySuite_MemReadModule_1.v")
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadWrite() {
    println("\ntestReadWrite ...")
    class ReadWriteModule extends Module {
      val io = new Bundle() {
        val addr = UInt(INPUT, width=32)
        val out = UInt(OUTPUT)
      }
      val mem = Mem(8, UInt(width=32))
      mem(io.addr) := mem(io.addr) + UInt(1)
      io.out := mem(io.addr)
    }
    chiselMain(Array[String]("--v", "--inlineMem",
      "--targetDir", dir.getPath.toString()),
      () => Module(new ReadWriteModule()))
    assertFile("DelaySuite_ReadWriteModule_1.v")
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadCondWrite() {
    println("\ntestReadCondWrite ...")
    class ReadCondWriteModule extends Module {
      val io = new Bundle() {
        val enable = Bool(INPUT)
        val addr = UInt(INPUT, width=32)
        val out = UInt(OUTPUT)
      }
      val mem= Mem(8, UInt(width=32))
      when( io.enable ) {
        mem(io.addr) := mem(io.addr) + UInt(1)
      }
      .otherwise {
        mem(io.addr) := mem(io.addr + UInt(4))
      }
      io.out := mem(io.addr)
    }
    chiselMain(Array[String]("--v", "--inlineMem",
      "--targetDir", dir.getPath.toString()),
      () => Module(new ReadCondWriteModule()))
    assertFile("DelaySuite_ReadCondWriteModule_1.v")
  }

  /** Uninitialized sram, one read and one write on each clock. */
  @Test def testReadCondMaskedWrite() {
    class ReadCondMaskedWrite extends Module {
      val io = new Bundle() {
        val enable = Bool(INPUT)
        val addr = UInt(INPUT, width=32)
        val out = UInt(OUTPUT)
      }
      val mem= Mem(8, UInt(width=32))
      when( io.enable ) {
        mem.write(io.addr, mem(io.addr), UInt(0xff00))
      }
      io.out := mem(io.addr)
    }
    chiselMain(Array[String]("--v", "--inlineMem",
      "--targetDir", dir.getPath.toString()),
      () => Module(new ReadCondMaskedWrite()))
    assertFile("DelaySuite_ReadCondMaskedWrite_1.v")
  }

  @Test def testSeqReadBundle() {
    class A extends Bundle {
      val a = new Bundle {
        val a = UInt(width = 8)
        val b = UInt(width = 16)
      }
      val a_b = UInt(width = 32) // this tests name mangling
      override def cloneType = new A().asInstanceOf[this.type]
    }
    class SeqReadBundle extends Module {
      val io = new Bundle {
        val ren = Bool(INPUT)
        val raddr = UInt(INPUT, width = 4)
        val wen = Bool(INPUT)
        val waddr = UInt(INPUT, width = 4)
        val in = Vec(2, new A()).asInput
        val out = Vec(2, new A()).asOutput
      }
      val mem = Mem(io.in.cloneType, 16, seqRead = true)
      when (io.wen) { mem(io.waddr) := io.in }
      io.out := mem(RegEnable(io.raddr, io.ren))
    }
    chiselMain(Array[String](
      "--targetDir", dir.getPath.toString()),
      () => Module(new SeqReadBundle()))
    // We used to:
    //    assertFile("DelaySuite_SeqReadBundle_1.h")
    // but this fails the name-mangling test since the
    // choice of name to mangle isn't deterministic
    assertFile("DelaySuite_SeqReadBundle_1.cpp")
  }

  /** Initialized ROM. */

  @Test def testROM() {
    class ROMModule extends Module {
      val io = new Bundle() {
        val addr = UInt(INPUT, width = 2)
        val out = UInt(OUTPUT, width = 4)
      }
      val a = UInt(1,width = 4)
      val b = UInt(2,width = 4)
      val c = UInt(3,width = 4)
      val rom = Vec(Array(a,b,c))
      io.out := rom(io.addr)
    }

    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new ROMModule()))
    assertFile("DelaySuite_ROMModule_1.v")

    chiselMain(Array[String](
      "--targetDir", dir.getPath.toString()),
      () => Module(new ROMModule()))
    assertFile("DelaySuite_ROMModule_1.h")
    assertFile("DelaySuite_ROMModule_1.cpp")
  }
}
