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

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import org.junit.After
import org.junit.rules.TemporaryFolder;

import Chisel._


/** This testsuite checks the generation of dot graphs.
*/
class DotBackendSuite extends TestSuite {

  /** Checks generation of simple dataflow graph */
  @Test def testSimple() {

    class DAGSubComp extends Module {
      val io = new Bundle {
        val ready = Bool(INPUT)
        val valid = Bool(OUTPUT)
      }
      val stored = Reg(next=io.ready)
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
      "--targetDir", dir.getPath.toString()),
      () => Module(new DAGComp()))
    assertFile("DotBackendSuite_DAGComp_1.dot")
  }
}
