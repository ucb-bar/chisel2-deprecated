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
  @Test def testWhenStatement() {
    class WhenModule extends Module {
      val io = new Bundle {
        val en = Bool(INPUT)
        val in = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      io.out := UInt(0)
      when(io.en) { io.out := io.in }
    }

    class WhenModuleTests(m: WhenModule) extends MapTester(m, Array(m.io)) {
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

    launchCppTester((m: WhenModule) => new WhenModuleTests(m))
  }

  // Put a when inside another when
  @Test def testEmbedWhenStatement() {
    class EmbedWhenModule extends Module {
      val io = new Bundle {
        val en0 = Bool(INPUT)
        val en1 = Bool(INPUT)
        val in = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      io.out := UInt(0)
      when(io.en0) { when(io.en1) { io.out := io.in } }
    }

    class EmbedWhenModuleTests(m: EmbedWhenModule) extends MapTester(m, Array(m.io)) {
      defTests {
        val vars = new HashMap[Node, Node]() 
        List(false, true, false, true,  true, false, true,  true).zip(
        List(false, true, true,  false, true, true,  false, true)).zipWithIndex.map { 
          case ((en0, en1), i) =>
            vars(m.io.en0) = Bool(en0)
            vars(m.io.en1) = Bool(en1)
            vars(m.io.in) = UInt(i)
            vars(m.io.out) = UInt(if(en0 && en1) i else 0)
            step(vars)
        } reduce(_&&_)
      }
    }

    launchCppTester((m: EmbedWhenModule) => new EmbedWhenModuleTests(m))
  }

  // When statement with elsewhen and otherwise clause.
  @Test def testElsewhen() {
    class ElsewhenModule extends Module {
      val io = new Bundle {
        val en0 = Bool(INPUT)
        val en1 = Bool(INPUT)
        val in0 = UInt(INPUT,4)
        val in1 = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      when(io.en0) {
        io.out := io.in0
      } .elsewhen(io.en1) {
        io.out := io.in1
      } .otherwise {
        io.out := UInt(0)
      }
    }

    class ElsewhenModuleTests(m: ElsewhenModule) extends MapTester(m, Array(m.io)) {
      defTests {
        val vars = new HashMap[Node, Node]() 
        List(false, true, false, true,  true, false, true,  true).zip(
        List(false, true, true,  false, true, true,  false, true)).zipWithIndex.map { 
          case ((en0, en1), i) =>
            vars(m.io.en0) = Bool(en0)
            vars(m.io.en1) = Bool(en1)
            vars(m.io.in0) = UInt(i)
            vars(m.io.in1) = UInt(i+1)
            vars(m.io.out) = UInt(if(en0) i else if(en1) i+1 else 0)
            step(vars)
        } reduce(_&&_)
      }
    }

    launchCppTester((m: ElsewhenModule) => new ElsewhenModuleTests(m))
  }

  /** instantiate module in a when block.
    */
  @Test def testModuleInWhenBlock() {
    class Submodule extends Module {
      val io = new Bundle {
        val in = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      io.out := io.in
    }

    class SubmoduleInWhenBlock extends Module {
      val io = new Bundle {
        val en = Bool(INPUT)
        val in = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      io.out := UInt(0)
      when( io.en ) {
        val sub = Module(new Submodule)
        io.out := sub.io.out
        io <> sub.io /* connect only io.in to sub.io.in */
      }
    }

    class SubmoduleInWhenBlockTests(m: SubmoduleInWhenBlock) extends MapTester(m, Array(m.io)) {
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

    launchCppTester((m: SubmoduleInWhenBlock) => new SubmoduleInWhenBlockTests(m))
  }

  // Unless statement with elsewhen and otherwise clause
  @Test def testUnless() {
    class UnlessModule extends Module {
      val io = new Bundle {
        val en = Bool(INPUT)
        val in = UInt(INPUT,4)
        val out = UInt(OUTPUT,4)
      }
      io.out := io.in
      unless(io.en) { io.out := UInt(0) }
    }

    class UnlessModuleTests(m: UnlessModule) extends MapTester(m, Array(m.io)) {
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

    launchCppTester((m: UnlessModule) => new UnlessModuleTests(m))
  }


  // switch statement, is clauses, and ? literals
  @Test def testSwitch() {
    class SwitchModule extends Module {
      val io = new Bundle {
        val in = UInt(INPUT,4)
        val out = Bool(OUTPUT)
      }
      io.out := Bool(false)
      switch(io.in) {
        is(UInt(0)) { io.out := Bool(true) }
        is(Bits("b???1")) { io.out := Bool(true) }
      }
    }

    class SwitchModuleTests(m: SwitchModule) extends MapTester(m, Array(m.io)) {
      defTests {
        val vars = new HashMap[Node, Node]() 
        (0 until 8).map { i =>
          vars(m.io.in) = UInt(i)
          vars(m.io.out) = if(i == 0) UInt(1) else UInt(i % 2)
          step(vars)
        } reduce(_&&_)
      }
    }

    launchCppTester((m: SwitchModule) => new SwitchModuleTests(m))
  }
}
