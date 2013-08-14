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

import scala.collection.mutable.ArrayBuffer
import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After
import org.junit.rules.TemporaryFolder;

import Chisel._


/** This testsuite checks the generation of dot graphs.
*/
class DotBackendSuite extends AssertionsForJUnit {

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


  /** Checks generation of simple dataflow graph */
  @Test def testSimple() {

    class DAGSubComp extends Module {
      val io = new Bundle {
        val ready = Bool(INPUT)
        val valid = Bool(OUTPUT)
      }
      val stored = Reg(updateData=io.ready)
      io.valid := stored
    }

    class DAGComp extends Module {
      val io = new Bundle {
        val data0 = Bool(INPUT)
        val data1 = Bool(INPUT)
        val result = Bool(OUTPUT) // XXX If we don't explicitely specify
        // OUTPUT here, dot and verilog is generated correctly but
        // not c++. This is an interaction between Module.findRoots
        // and class Bool { def apply(): Bool = Bool(null); }
      }
      val sub = Module(new DAGSubComp())
      sub.io.ready := io.data0 & io.data1
      io.result := sub.io.valid
    }

    chiselMain(Array[String](
      "--backend", "Chisel.DotBackend",
      "--targetDir", tmpdir.getRoot().toString()),
      () => Module(new DAGComp()))
    assertFile(tmpdir.getRoot() + "/DotBackendSuite_DAGComp_1.dot",
"""digraph DotBackendSuite_DAGComp_1{
rankdir = LR;
  subgraph clustersub{
    label = "sub"
    DotBackendSuite_DAGComp_1_sub__io_valid[label="io_valid:Bool"];
    DotBackendSuite_DAGComp_1_sub__stored[shape=square,label="stored:Reg"];
    DotBackendSuite_DAGComp_1_sub__io_ready[label="io_ready:Bool"];
    DotBackendSuite_DAGComp_1_sub__stored -> DotBackendSuite_DAGComp_1_sub__io_valid[label="1"];
    DotBackendSuite_DAGComp_1_sub__io_ready -> DotBackendSuite_DAGComp_1_sub__stored[label="1"];
  }
  T0 -> DotBackendSuite_DAGComp_1_sub__io_ready[label="1"];
  io_result[label="io_result:Bool"];
  T0[label="&"];
  io_data1[label="io_data1:Bool"];
  io_data0[label="io_data0:Bool"];
  DotBackendSuite_DAGComp_1_sub__io_valid -> io_result[label="1"];
  io_data0 -> T0[label="1"];
  io_data1 -> T0[label="1"];
}""")
  }
}
