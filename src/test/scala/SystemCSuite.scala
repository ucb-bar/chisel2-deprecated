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

//package ChiselTests
import Chisel._


/** This testsuite checks the SystemC backend implementation.
*/
class SystemCSuite extends TestSuite {
  // Test top-level IOs are decoupled.
  @Test def testTopLevelIO() {

    class SystemCModuleGood extends Module {
       val io = new Bundle {
         val a = Decoupled( UInt(width = 16) ).flip()
         val b = Decoupled( UInt(width = 16) )
       }

       io.b.bits := io.a.bits + UInt(10)
       io.a.ready := io.b.ready
       io.b.valid := io.a.valid
    }

    class SystemCModuleBad extends Module {
       val io = new Bundle {
         val a = UInt(INPUT, width = 16)
         val b = UInt(OUTPUT, width = 16)
       }

       io.b := io.a + UInt(10)
    }

    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath, "--backend", "sysc")

    chiselMain(testArgs.toArray, () => Module(new SystemCModuleGood()))
    assertFalse(ChiselError.hasErrors)

    chiselMain(testArgs.toArray, () => Module(new SystemCModuleBad()))
    assertTrue(ChiselError.hasErrors)
  }
}
