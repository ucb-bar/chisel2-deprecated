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

object VCDVerifySuite {
  val backends = List("c") /* ++
    (if (Driver.isVCSAvailable) {
      "v" :: Nil
    } else {
      Nil
    }
    ) */
}

class VCDVerifySuite extends TestSuite {

  // We currently ignore this test until assertVCDFile is updated to deal with redundant signal values.
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

    for (backend <- VCDVerifySuite.backends) {
      chiselMain(Array[String]("--backend", backend, "--compile", "--genHarness", "--test", "--vcd",
          "--targetDir", dir.getPath.toString()),
          () => Module(new Top()), (c: Top) => new VCDTester(c))
      assertVCDFile("VCDVerifySuite_Top_1.vcd")
    }
  }
}

