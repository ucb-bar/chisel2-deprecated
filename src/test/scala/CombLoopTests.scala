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

class CombLoopSuite extends TestSuite {
  @Test def testCombLoop() {
    println("\ntestCombLoop ...")

    class CombLoopModule extends Module {
      val io = new Bundle {
        val in = Decoupled(UInt(width=16)).flip
        val out = Decoupled(UInt(width=16))
      }
      io.in <> io.out
    }

    class CombLoopWrapper extends Module {
      val io = new Bundle {
        val out = Decoupled(UInt(width=16))
      }
      val mod1 = Module(new CombLoopModule)
      val mod2 = Module(new CombLoopModule)
      val mod3 = Module(new CombLoopModule)
      mod1.io.out <> mod2.io.in
      mod2.io.out <> mod3.io.in
      mod3.io.out <> mod1.io.in
      io.out.bits := mod3.io.out.bits
      io.out.valid := mod3.io.out.valid
      mod3.io.out.ready := io.out.ready
    }

    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
          "--minimumCompatibility", "3.0.0", "--wError", "--backend", "c")
    intercept[IllegalStateException] {
      // This should fail since we don't use a Wire wrapper
      chiselMain(testArgs, () => Module(new CombLoopWrapper))
    }
    assertTrue(ChiselError.hasErrors)
  }
}
