/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 Sydney. All Rights Reserved.  Redistribution and use in
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
import scala.util.Random
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._

/** This test suite tests the emulator with large integers ( > 64 bits )
  */
class TSBSuite extends TestSuite {
  val testArgs = Array("--backend", "v",
    "--targetDir", dir.getPath.toString()
  )

  @Test def testWrite {
    class WriteTest extends Module {
      val io = new Bundle {
        val in  = UInt(INPUT, 4)
        val en  = Bool(INPUT)
        val out = UInt(OUTPUT, 4)
      }
      val myTSB = new TSB(UInt(width=4))
      val writer = new WriterIO(UInt(width=4))
      writer.write := io.en
      writer.data := io.in
      myTSB.addWriter(writer)
      io.out := myTSB.out
    }

    class WriteTestTests(c : WriteTest) extends Tester(c) {
      poke(c.io.in, BigInt(3))
      poke(c.io.en, BigInt(1))
      expect(c.io.out, BigInt(3))
      poke(c.io.en, BigInt(0))
      expect(c.io.out, BigInt(15))
    }

    launchCppTester((c: WriteTest) => new WriteTestTests(c))
    chiselMain(testArgs,
      () => Module(new WriteTest()))
    assertFile("TSBSuite_WriteTest_1.v")
  }

  @Test def testWriteLow {
    class WriteLowTest extends Module {
      val io = new Bundle {
        val in  = UInt(INPUT, 4)
        val en  = Bool(INPUT)
        val out = UInt(OUTPUT, 4)
      }
      val myTSB = new TSB(UInt(width=4))
      myTSB.pulledHigh = false
      val writer = new WriterIO(UInt(width=4))
      writer.write := io.en
      writer.data := io.in
      myTSB.addWriter(writer)
      io.out := myTSB.out
    }

    class WriteLowTestTests(c : WriteLowTest) extends Tester(c) {
      poke(c.io.in, BigInt(3))
      poke(c.io.en, BigInt(1))
      expect(c.io.out, BigInt(3))
      poke(c.io.en, BigInt(0))
      expect(c.io.out, BigInt(0))
    }

    launchCppTester((c: WriteLowTest) => new WriteLowTestTests(c))
    chiselMain(testArgs,
      () => Module(new WriteLowTest()))
    assertFile("TSBSuite_WriteLowTest_1.v")
  }

  @Test def testMultiWord {
    class WriteMultiTest extends Module {
      val io = new Bundle {
        val in  = UInt(INPUT, 130)
        val en  = Bool(INPUT)
        val out = UInt(OUTPUT, 130)
      }
      val myTSB = new TSB(UInt(width=130))
      val writer = new WriterIO(UInt(width=130))
      writer.write := io.en
      writer.data := io.in
      myTSB.addWriter(writer)
      io.out := myTSB.out
    }

    class WriteMultiTestTests(c : WriteMultiTest) extends Tester(c) {
      poke(c.io.in, BigInt(3))
      poke(c.io.en, BigInt(1))
      expect(c.io.out, BigInt(3))
      poke(c.io.en, BigInt(0))
      expect(c.io.out, (BigInt(1) << 130) - BigInt(1))
    }

    launchCppTester((c: WriteMultiTest) => new WriteMultiTestTests(c))
    chiselMain(testArgs,
      () => Module(new WriteMultiTest()))
    assertFile("TSBSuite_WriteMultiTest_1.v")
  }
}
