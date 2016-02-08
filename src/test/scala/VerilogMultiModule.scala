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

import Chisel._


class VerilogMultiModule extends TestSuite {
  @Test def testVerilogMultiModule() {
    // A class to generate multiple "filter" sections.
    // The default is two sections.
    // For any module with more than one section, Queues are used to interconnect sections.
    class MultiMultiFIR (nSections: Int = 2) extends Module {
      class MultiFIR (nSections: Int = 2) extends Module {
        class FilterSection(coeffs: Array[Flo]) extends Module {
          val io = new Bundle {
            val in = Decoupled(Flo(INPUT)).flip
            val out = Decoupled(Flo(OUTPUT))

          }
          // Specify the filter function - multiply by two.
          io.out.bits := io.in.bits * Flo(2.0)
          // Connect the valid (output) and ready (input) signals
          io.out.valid := io.in.valid
          io.in.ready := io.out.ready
        }

        val io = new Bundle {
          val in = Decoupled(Flo(INPUT)).flip
          val out = Decoupled(Flo(OUTPUT))
        }
        // Parameterized filter sections. If more than a single section,
        //  insert queues between each section.
        var lastin = io.in
        require(nSections > 0, "MultiFIR: nSections must be greater than 0")
        for (s <- 1 to nSections) {
          val f = Module(new FilterSection(Array(Flo(1.0))))
  //        f.name = "f" + s.toString
          f.io.in <> lastin
          // Do we have a following section?
          if (s < nSections) {
            val q = Module(new Queue(Flo(), 4))
  //          q.name = "q" + s.toString + (s + 1).toString
            q.io.enq <> f.io.out
            lastin = q.io.deq
          } else {
            io.out <> f.io.out
          }
        }
      }
      val io = new Bundle {
        val in = Decoupled(Flo(INPUT)).flip
        val out = Decoupled(Flo(OUTPUT))
      }
      val m1 = Module(new MultiFIR(nSections))
      val m2 = Module(new MultiFIR(nSections))
      m2.io.in <> m1.io.out
    }

    chiselMain(Array[String]("--backend", "v",
        "--targetDir", dir.getPath.toString()),
        () => Module(new MultiMultiFIR(3)))
    assertFile("VerilogMultiModule_MultiMultiFIR_2.v")
  }

}
