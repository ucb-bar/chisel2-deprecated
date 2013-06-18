import scala.collection.mutable.ArrayBuffer
import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After
import org.junit.rules.TemporaryFolder;

import Chisel._

/** This testsuite checks interaction of component class
  and runtime hierarchies.
*/
class ConnectSuite extends AssertionsForJUnit {

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

  /** Instantiate a component tree where all component classes have
    no relationship. */
  @Test def testNoClassRelation() {
    println("\n### testNoClassRelation ###")
    class A extends Component {
      val io = new Bundle {
        val a_in = UFix(INPUT, 1)
        val a_out = UFix(OUTPUT, 1)
      }
      io.a_out := io.a_in
    }
    class B extends Component {
      val io = new Bundle {
        val b_in = UFix(INPUT, 1)
        val b_out = UFix(OUTPUT, 1)
      }
      val aComp = Mod(new A())
      aComp.io.a_in := io.b_in
      io.b_out := aComp.io.a_out
    }
    class NoClassRelation extends Component {
      val io = new Bundle {
        val c_in = UFix(INPUT, 1)
        val c_out = UFix(OUTPUT, 1)
      }
      val aComp = Mod(new B())
      aComp.io.b_in := io.c_in
      io.c_out := aComp.io.b_out
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Mod(new NoClassRelation()))
    assertFile(tmpdir.getRoot() + "/ConnectSuite_NoClassRelation_1.v",
"""module ConnectSuite_A_1(
    input  io_a_in,
    output io_a_out);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_B_1(
    input  io_b_in,
    output io_b_out);

  wire aComp_io_a_out;

  assign io_b_out = aComp_io_a_out;
  ConnectSuite_A_1 aComp(
       .io_a_in( io_b_in ),
       .io_a_out( aComp_io_a_out ));
endmodule

module ConnectSuite_NoClassRelation_1(
    input  io_c_in,
    output io_c_out);

  wire aComp_io_b_out;

  assign io_c_out = aComp_io_b_out;
  ConnectSuite_B_1 aComp(
       .io_b_in( io_c_in ),
       .io_b_out( aComp_io_b_out ));
endmodule

""")
  }

  /** Instantiate a component of the same class (*A*) twice
    with logic in-between. */
  @Test def testLogicBtwInstances() {
    println("\n### testLogicBtwInstances ###")
    class A extends Component {
      val io = new Bundle {
        val a_in = UFix(INPUT, 1)
        val a_out = UFix(OUTPUT, 1)
      }
      io.a_out := io.a_in
    }
    class LogicBtwInstances extends Component {
      val io = new Bundle {
        val b_in = UFix(INPUT, 1)
        val b_out = UFix(OUTPUT, 1)
      }
      val a1 = Mod(new A())
      val x = Reg(UFix(1))
      x := io.b_in
      val a2 = Mod(new A())
      a1.io.a_in := io.b_in
      a2.io.a_in := io.b_in
      io.b_out := a1.io.a_out | a2.io.a_out | x
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Mod(new LogicBtwInstances()))
    assertFile(tmpdir.getRoot() + "/ConnectSuite_LogicBtwInstances_1.v",
"""module ConnectSuite_A_2(
    input  io_a_in,
    output io_a_out);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_LogicBtwInstances_1(input clk, input reset,
    input  io_b_in,
    output io_b_out);

  wire T0;
  reg[0:0] x;
  wire T1;
  wire T2;
  wire a2_io_a_out;
  wire a1_io_a_out;

  assign io_b_out = T0;
  assign T0 = T2 | x;
  assign T1 = 1'h1/* 1*/ ? io_b_in : x;
  assign T2 = a1_io_a_out | a2_io_a_out;
  ConnectSuite_A_2 a1(
       .io_a_in( io_b_in ),
       .io_a_out( a1_io_a_out ));
  ConnectSuite_A_2 a2(
       .io_a_in( io_b_in ),
       .io_a_out( a2_io_a_out ));

  always @(posedge clk) begin
    x <= T1;
  end
endmodule

""")
  }

  /** Instantiate a component of the same class (*A*) at two levels
    of a component class hierarchy (*B* < *C*). */
  @Test def test2Instance2Level() {
    println("\n### test2Instance2Level ###")
    /* XXX This test will fail to pick up the correct pop sequence
     on the Component stack. 
    class A extends Component {
      val io = new Bundle {
        val a_in = UFix(INPUT, 1)
        val a_out = UFix(OUTPUT, 1)
      }
      io.a_out := io.a_in
    }
    class B extends Component {
      val io = new Bundle {
        val b_in = UFix(INPUT, 1)
        val b_out = UFix(OUTPUT, 1)
      }
      val aInBComp = Mod(new A())
      aInBComp.io.a_in := io.b_in
    }
    class Instance2Level extends B {
      val aInCComp = Mod(new A())
      aInCComp.io.a_in := io.b_in
      io.b_out := aInCComp.io.a_out | aInBComp.io.a_out
    }
    chiselMain(Array[String]("--v"),
//      "--targetDir", tmpdir.getRoot().toString()),
      () => Mod(new Instance2Level()))
     */
  }

  /** Instantiate a component superclass inside a component */
  @Test def testInstanceSuperclass() {
    println("\n### testInstanceSuperclass ###")
    class A extends Component {
      val io = new Bundle {
        val a_in = UFix(INPUT, 1)
        val a_out = UFix(OUTPUT, 1)
      }
      io.a_out := io.a_in
    }
    class InstanceSuperclass extends A {
      val aInBComp = Mod(new A())
      aInBComp.io.a_in := io.a_in
    }
    chiselMain(Array[String]("--v",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Mod(new InstanceSuperclass()))
    assertFile(tmpdir.getRoot() + "/ConnectSuite_InstanceSuperclass_1.v",
"""module ConnectSuite_A_3(
    input  io_a_in,
    output io_a_out);


  assign io_a_out = io_a_in;
endmodule

module ConnectSuite_InstanceSuperclass_1(
    input  io_a_in,
    output io_a_out);


  assign io_a_out = io_a_in;
  ConnectSuite_A_3 aInBComp(
       .io_a_in( io_a_in ),
       .io_a_out(  ));
endmodule

""")
  }

}
