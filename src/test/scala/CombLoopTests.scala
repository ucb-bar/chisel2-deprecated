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

  // Verify that if we suppress combination loop detection,
  //  we get something we can execute.
  @Test def testCombLoopForwardReference() {
    println("\ntestCombLoopForwardReference ...")


    class test_in extends Module
    {
      val io = new Bundle
      {
          val five  = Bool( INPUT )
          val latch = Bool( INPUT )
          val add   = UInt( INPUT, 16 )
          val out   = UInt( OUTPUT, 16 )
        }
    
      val other = Mux( io.five, UInt(5,16), io.add )
      val latch = Reg( init=UInt(0,16) )
      when( io.latch ) { latch := other }
      io.out := Mux( io.latch, latch, other ) 
    }
    
    class test_add extends Module
    {
      val io = new Bundle
      {
          val ina   = UInt( INPUT, 16 )
          val inb   = UInt( INPUT, 16 )
          val out   = UInt( OUTPUT, 16 )
        }
      io.out := io.ina + io.inb
    }
    
    class testelement extends Module
    {
      val io = new Bundle
      {
        val five_a  = Bool( INPUT )
        val five_b  = Bool( INPUT )
        val latch_a = Bool( INPUT )
        val latch_b = Bool( INPUT )
        }
    
      // Modules
      val ina = Module( new test_in )
      val inb = Module( new test_in )
      val add = Module( new test_add )
    
      // Input A
      ina.io.five   := io.five_a
      ina.io.latch  := io.latch_a
      ina.io.add    := add.io.out
      
      // Input B
      inb.io.five   := io.five_b
      inb.io.latch  := io.latch_b
      inb.io.add    := add.io.out
      
      // Lut X
        add.io.ina    := ina.io.out
        add.io.inb    := inb.io.out
    }
    
    class testelement_Tests(c: testelement) extends Tester(c)
    {
      step( 1 )
    }

    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
          "--compile", "--test", "--genHarness", "--noCombLoop")
    chiselMainTest( testArgs, () => Module( new testelement() ) ) {
      c => new testelement_Tests( c )
    }

  }

}
