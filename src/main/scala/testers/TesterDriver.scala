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

package Chisel.testers

import Chisel._
import scala.io.Source
import scala.sys.process._
import java.io._

// Wrapper to run Chisel3-style testers in Chisel2.

object TesterDriver {
  def execute(t: () => BasicTester)(implicit testArgs: Array[String]): Boolean = {
    try {
      // Construct the combined circuit, containing all the required
      //  poke()'s and expect()'s as arrays of data.
      val mod = Driver(testArgs, finishWrapper(t), false)
      if (Driver.isTesting) {
        // Initialize a tester with tracing turned on.
        val c = new Tester(mod, true)
        // Run the testing circuit until we see io.done.
        while(c.peek(mod.io.done) == 0) {
          c.step(1)
        }
        val error = c.peek(mod.io.error)
        val pc = c.peek(mod.io.pc)
        if (error != 0) {
          c.fail
        }

  // Do an additional step to get any printf output.
        c.step(1)
        c.finish
      }
      true
    } catch {
      case e: Throwable =>
        println(e)
        false
    }
  }

  def elaborate(t: () => BasicTester)(implicit testArgs: Array[String]): Module = {
    val mod = Driver(testArgs, finishWrapper(t), false)
    mod
  }

  def finishWrapper(test: () => BasicTester): () => BasicTester = {
    () => {
      val tester = test()
      tester.finish()
      tester
    }
  }
}
