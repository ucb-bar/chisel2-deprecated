/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
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
import Chisel._

class WhenSuite extends TestSuite {

  // Using a single when
  @Ignore("For some reason, this generates an IOException")
  @Test def testWhenStatement() {
    class WhenModule extends Module {
      val io = new Bundle {
        val en = Bool(INPUT)
        val in = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      io.out := UInt(0)
      when( io.en ) {
        io.out := io.in
      }
    }

    class WhenModuleTests(m: WhenModule) extends Tester(m, Array(m.io)) {
      defTests {
        val vars = new HashMap[Node, Node]() 
        List(false,true,false,true,false,false,false,true).zipWithIndex.map { 
          case (en, i) =>
            vars(m.io.en) = Bool(en)
            vars(m.io.in) = UInt(i)
            vars(m.io.out) = UInt(if(en) i else 0)
            step(vars)
        } reduce(_&&_)
      }
    }

    chiselMainTest(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString(),"--test"),
      () => Module(new WhenModule)) {
      c => new WhenModuleTests(c)
    }
  }

  /** Put a when() inside another when() */
  @Test def testEmbedWhenStatement() {
    class EmbedWhenComp extends Module {
      val io = new Bundle {
        val in0 = Bool(INPUT)
        val in1 = Bool(INPUT)
        val out = Bool(OUTPUT)
      }

      io.out := Bool(false);
      when( io.in0 ) {
        io.out := io.in0;
        when( io.in1 ) {
          io.out := io.in1;
        }
      }
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new EmbedWhenComp))

    assertFile("WhenSuite_EmbedWhenComp_1.v")
  }

  /** When statement with elsewhen and otherwise clause.
    */
  @Test def testWhenClass() {
    println("\ntestWhenClass:")

    class WhenClassComp extends Module {
      val io = new Bundle {
        val in0 = Bool(INPUT)
        val in1 = Bool(INPUT)
        val out = Bool(OUTPUT)
      }

      when( io.in0 ) {
        io.out := io.in0;
      } .elsewhen( io.in1 ) {
        io.out := io.in1;
      } .otherwise {
        io.out := Bool(false);
      }
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new WhenClassComp))

    assertFile("WhenSuite_WhenClassComp_1.v")
  }

  /** Forgot the dot prefix in elsewhen statement.
  */
  @Ignore("to fix: error message")
  @Test def testForgotDotElseWhen() {
      class ForgotDotElseWhenComp extends Module {
        val io = new Bundle {
          val in0 = Bool(INPUT)
          val in1 = Bool(INPUT)
          val out = Bool(OUTPUT)
        }

        io.out := io.in0;
        when( io.in0 ) {
          io.out := io.in0;
        }
        /**
          XXX Replace the weird overloaded error by a more meaningful message.

          \code
          error: overloaded method value apply with alternatives:
          (bit: Chisel.UInt)Chisel.Bool <and>
          (bit: Int)Chisel.Bool
          cannot be applied to (Unit)
          } elsewhen( io.in1 ) {
          \endcode

          Note: We cannot catch this as an exception since it is happening
          at compile-time.
        */
        /*
        elsewhen( io.in1 ) {
          io.out := io.in1;
        }
        */
      }

      chiselMain(Array[String]("--backend", "v"),
        //      "--targetDir", dir.getPath.toString()),
        () => Module(new ForgotDotElseWhenComp))

      assertFile("WhenSuite_ForgotDotElseWhen_1.v")
  }

  /** instantiate module in a when block.
    */
  @Ignore("to fix: emit correct code with no error messages.")
  @Test def testModuleInWhenBlock() {
      class ModuleInWhenBlockSub extends Module {
        val io = new Bundle {
          val in = Bool(INPUT)
          val out = Bool(OUTPUT)
        }
        /* XXX This generates an error: NO DEFAULT SPECIFIED FOR WIRE */
        io.out := io.in
      }

      class ModuleInWhenBlockComp extends Module {
        val io = new Bundle {
          val in = Bool(INPUT)
          val out = Bool(OUTPUT)
        }

        io.out := io.in;
        when( io.in ) {
          val sub = Module(new ModuleInWhenBlockSub())
          io <> sub.io;
        }
      }

      chiselMain(Array[String]("--backend", "v"),
        //      "--targetDir", dir.getPath.toString()),
        () => Module(new ModuleInWhenBlockComp))

      assertFile("WhenSuite_ModuleInWhenBlockComp.v")
  }

  /** Unless statement with elsewhen and otherwise clause.
    */
  @Test def testUnlessClass() {
    println("\ntestUnlessClass:")

    class UnlessClassComp extends Module {
      val io = new Bundle {
        val in0 = Bool(INPUT)
        val in1 = Bool(INPUT)
        val out = Bool(OUTPUT)
      }

      io.out := io.in0;
      unless( io.in0 ) {
        io.out := io.in1;
      }
      /**XXX why is this impossible?
        .elsewhen( io.in1 ) {
        io.out := io.in1;
      } .otherwise {
        io.out := Bool(false);
      }
      */
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new UnlessClassComp))

    assertFile("WhenSuite_UnlessClassComp_1.v")
  }


  /** switch statement and is clauses.
    */
  @Test def testSwitchClass() {
    class SwitchClassComp extends Module {
      val io = new Bundle {
        val in0 = UInt(INPUT, width=8)
        val in1 = UInt(INPUT, width=8)
        val out = UInt(OUTPUT, width=8)
      }

      switch( io.in0 ) {
        /* XXX This looks like a weird way to initialize the default value. */
        io.out := io.in0
        is(Bits("b0101??01")) { io.out := io.in1 }
      }
    }

    chiselMain(Array[String]("--backend", "c",
      "--targetDir", dir.getPath.toString()),
      () => Module(new SwitchClassComp))

    assertFile("WhenSuite_SwitchClassComp_1.cpp")

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new SwitchClassComp))

    assertFile("WhenSuite_SwitchClassComp_1.v")
  }


}
