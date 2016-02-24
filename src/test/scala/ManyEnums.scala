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

//package ChiselTests
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._

class ManyEnumsSuite extends TestSuite {
  @Test def testManyEnums() {
    println("\ntestManyEnums...")
    class ManyEnums extends Module {
      val io = UInt(OUTPUT, 16)
      val states = Enum(UInt(),
        List('e_00, 'e_01, 'e_02, 'e_03, 'e_04, 'e_05, 'e_06, 'e_07,
             'e_08, 'e_09, 'e_10, 'e_11, 'e_12, 'e_13, 'e_14, 'e_15, 'e_16, 'e_17,
             'e_18, 'e_19, 'e_20, 'e_21, 'e_22))
      val state = Reg(UInt(), init = states('e_22))
      io := state
    }

    class ManyEnumsTester(m: ManyEnums) extends Tester(m) {
      expect(m.io, 22)
    }

    launchCppTester((c: ManyEnums) => new ManyEnumsTester(c))
  }
}
