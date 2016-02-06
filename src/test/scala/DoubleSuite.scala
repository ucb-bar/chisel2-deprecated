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

import org.junit.Test

import Chisel._

class DoubleSuite extends TestSuite {
  @Test def testCompareDbl() {
    println("\ntestCompareDbl...")
    class CompareDblModule extends Module {
      class IO extends Bundle {
        val in1 = Dbl(INPUT)
        val in2 = Dbl(INPUT)
        val outLT = Bool(OUTPUT)
        val outLE = Bool(OUTPUT)
        val outGT = Bool(OUTPUT)
        val outGE = Bool(OUTPUT)
      }
      val io = new IO()
      val dbl1 = io.in1
      val dbl2 = io.in2
      io.outLT := dbl1 < dbl2
      io.outLE := dbl1 <= dbl2
      io.outGT := dbl1 > dbl2
      io.outGE := dbl1 >= dbl2
    }

    trait CompareDblModuleTests extends Tests {
      def tests(m: CompareDblModule) {
        for (i <- 0 to 100) {
          val dbl1 = rnd.nextDouble
          val dbl2 = rnd.nextDouble

          poke(m.io.in1, dbl1)
          poke(m.io.in2, dbl2)
          expect(m.io.outLT, if (dbl1 < dbl2) 1 else 0)
          expect(m.io.outLE, if (dbl1 <= dbl2) 1 else 0)
          expect(m.io.outGT, if (dbl1 > dbl2) 1 else 0)
          expect(m.io.outGE, if (dbl1 >= dbl2) 1 else 0)
        }
      }
    }

    class CompareDblModuleTester(m: CompareDblModule) extends Tester(m) with CompareDblModuleTests {
      tests(m)
    }

    launchCppTester((m: CompareDblModule) => new CompareDblModuleTester(m))
  }
}
