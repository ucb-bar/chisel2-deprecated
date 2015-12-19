/*
 Copyright (c) 2011, 2012, 2013, 2014, 2015 The Regents of the University of
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

class ExtractSuite extends TestSuite {

  /** Bad Width Inference in Extract #621
   *
   */
  @Test def extractWidthInfer() {
    println("\nextractWidthInfer ...")
    class ExtractWidthInfer extends Module {
      val io = new Bundle {
              val in = UInt(INPUT, width = 64)
              val data = UInt(OUTPUT)
      }
      val offset = Reg(UInt(width = 32))
      io.data := io.in(UInt(63) - offset, UInt(0))
    }

    class TestExtractWidthInfer(m: ExtractWidthInfer) extends Tester(m) {
      List(0, 16, 4, 7, 12, 64).map { i =>
        val data = BigInt(i)
        poke(m.io.in, data)
        step(1)
        expect(m.io.data, data & 0xffffffff)
      }
    }

    chiselMain(Array[String]("--backend", "v", "--targetDir", dir.getPath.toString()),
      () => Module(new ExtractWidthInfer()))
    launchCppTester((m: ExtractWidthInfer) => new TestExtractWidthInfer(m))
  }

  @Test def extractDynamic() {
    println("\nextractDynamic ...")
    class ExtractDynamic extends Module {
      val io = new Bundle {
              val in = UInt(INPUT, width = 64)
              val data = UInt(OUTPUT)
              val hi = UInt(INPUT, width = 5)
              val lo = UInt(INPUT, width = 5)
      }
      io.data := io.in(io.hi, io.lo)
    }

    class TestExtractDynamic(m: ExtractDynamic) extends Tester(m) {
      List((7, 0), (16, 11), (4, 2), (11, 7), (63, 32)).map { case (hi, lo) =>
        val data = BigInt(0x5A5A5A5A5A5A5A5AL)
        val width = hi - lo + 1
        val mask = (BigInt(1) << width) - 1
        poke(m.io.in, data)
        poke(m.io.hi, hi)
        poke(m.io.lo, lo)
        step(1)
        expect(m.io.data, (data >> lo) & mask)
      }
    }

    chiselMain(Array[String]("--backend", "v", "--targetDir", dir.getPath.toString()),
      () => Module(new ExtractDynamic()))
    launchCppTester((m: ExtractDynamic) => new TestExtractDynamic(m))
  }
}
