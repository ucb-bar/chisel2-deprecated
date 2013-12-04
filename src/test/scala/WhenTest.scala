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

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After
import org.junit.Ignore
import org.junit.rules.TemporaryFolder;

import Chisel._


class WhenSuite extends AssertionsForJUnit {

  val tmpdir = new TemporaryFolder();

  @Before def initialize() {
    tmpdir.create()
  }

  @After def done() {
    tmpdir.delete()
  }

  def assertFile( filename: String, content: String ) {
    val source = scala.io.Source.fromFile(filename, "utf-8")
    val lines = source.mkString
    source.close()
    assert(lines === content)
  }

  /** Using a single when() as intended.
    */
  @Test def testWhenStatement() {
    class WhenComp extends Module {
      val io = new Bundle {
        val in = Bool(INPUT)
        val out = Bool(OUTPUT)
      }

      io.out := Bool(false);
      when( io.in ) {
        io.out := io.in;
      }
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new WhenComp))

    assertFile(tmpdir.getRoot() + "/WhenSuite_WhenComp_1.v",
"""module WhenSuite_WhenComp_1(
    input io_in,
    output io_out
);

  wire T0;

  assign io_out = T0;
  assign T0 = io_in ? io_in : 1'h0/* 0*/;
endmodule

""")
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
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new EmbedWhenComp))

    assertFile(tmpdir.getRoot() + "/WhenSuite_EmbedWhenComp_1.v",
"""module WhenSuite_EmbedWhenComp_1(
    input io_in0,
    input io_in1,
    output io_out
);

  wire T0;
  wire T1;
  wire T2;

  assign io_out = T0;
  assign T0 = T1 ? io_in1 : T2;
  assign T1 = io_in0 && io_in1;
  assign T2 = io_in0 ? io_in0 : 1'h0/* 0*/;
endmodule

""")
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
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new WhenClassComp))

    assertFile(tmpdir.getRoot() + "/WhenSuite_WhenClassComp_1.v",
"""module WhenSuite_WhenClassComp_1(
    input io_in0,
    input io_in1,
    output io_out
);

  wire T0;
  wire T1;
  wire T2;
  wire T3;

  assign io_out = T0;
  assign T0 = T1 ? io_in1 : T3;
  assign T1 = T2 && io_in1;
  assign T2 = ! io_in0;
  assign T3 = io_in0 ? io_in0 : 1'h0/* 0*/;
endmodule

""")
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
        //      "--targetDir", tmpdir.getRoot().toString()),
        () => Module(new ForgotDotElseWhenComp))

      assertFile(tmpdir.getRoot() + "/WhenSuite_ForgotDotElseWhen_1.v",
"""
""")
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
        //      "--targetDir", tmpdir.getRoot().toString()),
        () => Module(new ModuleInWhenBlockComp))

      assertFile(tmpdir.getRoot() + "/WhenSuite_ModuleInWhenBlockComp.v",
"""
""")
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
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new UnlessClassComp))

    assertFile(tmpdir.getRoot() + "/WhenSuite_UnlessClassComp_1.v",
"""module WhenSuite_UnlessClassComp_1(
    input io_in0,
    input io_in1,
    output io_out
);

  wire T0;
  wire T1;

  assign io_out = T0;
  assign T0 = T1 ? io_in1 : io_in0;
  assign T1 = ! io_in0;
endmodule

""")
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
        is(io.in1) { io.out := io.in1 }
      }
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new SwitchClassComp))

    assertFile(tmpdir.getRoot() + "/WhenSuite_SwitchClassComp_1.v",
"""module WhenSuite_SwitchClassComp_1(
    input [7:0] io_in0,
    input [7:0] io_in1,
    output [7:0] io_out
);

  wire [7:0] T0;
  wire T1;

  assign io_out = T0;
  assign T0 = T1 ? io_in1 : io_in0;
  assign T1 = io_in0 == io_in1;
endmodule

""")
  }


}
