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

class RegWithAgregateDefaultSuite extends TestSuite {
  val myWidth = 32
  val intU = 42
  val intB = false

  @Test def testRegWithAgregateDefault() {
    println("\ntestRegWithAgregateDefault...")

    class RegWithAgregateDefaultModule extends Module {
      val io = new Bundle {
        val outU = UInt(OUTPUT, myWidth)
        val outB = Bool(OUTPUT)
      }

      if (true) {
      val v = Reg(init = Vec(12, UInt(intU, width = myWidth)))
        io.outU := v(0)
        io.outB := Bool(intB)
      } else {
        val r = Reg(init = new Bundle {
          val a = UInt(intU, width = myWidth)
          val b = Bool(intB)
          override def cloneType: this.type = this
        })
        io.outU := r.a
        io.outB := r.b
      }
    }

    class RegWithAgregateDefaultTests(m: RegWithAgregateDefaultModule) extends Tester(m) {
      val expectedBool = if (intB) 1 else 0
      expect(m.io.outU, intU)
      expect(m.io.outB, expectedBool)
    }
    launchCppTester((m: RegWithAgregateDefaultModule) => new RegWithAgregateDefaultTests(m))
  }

  /* Verify that we give a reasonable error message when we can't flatten
   *  a bundle for use as a Reg.
   */
  @Test def testRegWithBundleInit() {
    println("\ntestRegWithBundleInit...")

    class BundleWithInit extends Bundle {
      // This will fail for use as a Reg, mentioning the element "bar".
      val bar = UInt(0, width=8).asOutput
      override def cloneType: this.type = {
        val res = new BundleWithInit()
        res.asInstanceOf[this.type]
      }
    }

    class RegWithBundleInitModule extends Module {
      val io = new Bundle {
        val outU = UInt(OUTPUT, myWidth)
        val outB = Bool(OUTPUT)
      }

      // This will fail, generating a message mentioning the element "bar".
      val v = Reg(new BundleWithInit())
      io.outU := v.bar
      io.outB := Bool(false)
    }

    class RegWithBundleInitTests(m: RegWithBundleInitModule) extends Tester(m) {
      expect(m.io.outU, 0)
      expect(m.io.outB, 0)
    }
    intercept[java.lang.reflect.InvocationTargetException] {
      launchCppTester((m: RegWithBundleInitModule) => new RegWithBundleInitTests(m))
    }
    assertTrue(ChiselError.getErrorList.exists(_.msgFun().contains("element \"bar\" has 1 input")))
  }

}
