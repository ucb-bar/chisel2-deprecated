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
import org.junit.Test
import org.junit.Assert._

class VecSuite extends TestSuite {
  @Test def testMiscVec() {
    println("\ntestMiscVec ...")

    class VecApp(n: Int, W: Int) extends Module {
      class MyBundle(aWidth: Int) extends Bundle {
        val aUInt = UInt(width = aWidth)
      }
      val io = IO(new Bundle {
        val a = UInt(INPUT, n)
        val i = Vec(n, Bits(INPUT, W))
        val d = Bits(OUTPUT, W)
      })
      io.d := io.i(io.a)
    }
    
    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
          "--minimumCompatibility", "3.0.0", "--wError", "--backend", "null")
    chiselMain(testArgs, () => Module(new VecApp(8, 9)))
    assertFalse(ChiselError.hasErrors)
  }

  @Test def testEmptyVec() {
    println("\ntestEmptyVec ...")

    class EmptyVec(n: Int) extends Module {
      class MyBundle(aWidth: Int) extends Bundle {
        val aUInt = UInt(width = aWidth)
      }
      val io = IO(new Bundle {
        val empty = Vec(0, new MyBundle(n).asOutput)
      })
    }
    
    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
          "--minimumCompatibility", "3.0.0", "--wError", "--backend", "null")
    intercept[IllegalStateException] {
      chiselMain(testArgs, () => Module(new EmptyVec(8)))
    }
    assertTrue(ChiselError.hasErrors)
  }
  // Issue #718 - Vec.fill with vec initializer
  @Test def testVecFillFromVec() {
    println("\ntestVecFillFromVec ...")

    class VecApp(n: Int) extends Module {
      val io = IO(new Bundle {
        val a = UInt(INPUT, n)
      })
      val z = Vec(32, UInt(32, width=16))
      var i = 0
      val x = Vec.fill( 32 )
      {
        val y = z(i)
        i = i + 1
        y
      }
    }
    
    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
          /* "--minimumCompatibility", "3.0.0", "--wError",*/ "--backend", "null")
    chiselMain(testArgs, () => Module(new VecApp(32)))
    assertFalse(ChiselError.hasErrors)
    
  }
}
