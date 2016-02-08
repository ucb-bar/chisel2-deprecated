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

//package ChiselTests
import Chisel._


/** This testsuite checks the Complex class implementation.
*/
class ComplexSuite extends TestSuite {
  // Test Complex assignment
  @Test def testComplexAssignment() {
    class ComplexAssign(W: Int) extends Module {
      val io = new Bundle {
        val e   = /* new */ Bool(INPUT)
        val in  = new Complex(Bits(width = W), Bits(width = W)).asInput
        val out = new Complex(Bits(width = W), Bits(width = W)).asOutput
      }
      when (io.e) {
//        val w = Wire(new Complex(Bits(width = W), Bits(width = W)))
//        w := io.in
        io.out.real := io.in.real
        io.out.imag := io.in.imag
      } .otherwise {
        io.out.real := Bits(0)
        io.out.imag := Bits(0)
      }
    }

    trait ComplexAssignTests extends Tests {
      def tests(c: ComplexAssign) {
        for (t <- 0 until 4) {
          val test_e     = rnd.nextInt(2)
          val test_in_real = rnd.nextInt(256)
          val test_in_imag = rnd.nextInt(256)

          poke(c.io.e,     test_e)
          poke(c.io.in.real, test_in_real)
          poke(c.io.in.imag, test_in_imag)
          step(1)
          expect(c.io.out.real, if (test_e == 1) test_in_real else 0)
          expect(c.io.out.imag, if (test_e == 1) test_in_imag else 0)
        }
      }
    }

    class ComplexAssignTester(c: ComplexAssign) extends Tester(c) with ComplexAssignTests {
      tests(c)
    }

    val testArgs = chiselEnvironmentArguments() ++ Array("--compile", "--genHarness", "--test", "--targetDir", dir.getPath)
    chiselMainTest(testArgs.toArray, () => Module(new ComplexAssign(16))) {
      c => new ComplexAssignTester(c)
    }
  }

  // Test Complex arithmetic
  @Test def testComplex() {
    class ComplexTest(W: Int) extends Module {
      val io = new Bundle {
        val in_t = Complex(SInt(width=W),SInt(width=W)).asInput
        val in_f = Complex(SInt(width=W),SInt(width=W)).asInput
        val cond = Bool(INPUT)
        val out  = Complex(SInt(width=W),SInt(width=W)).asOutput

        val b_t = UInt(width=1).asInput
        val b_f = UInt(width=1).asInput
        val b_o = UInt(width=1).asOutput
      }

      val myLit = Complex(SInt(1, W), SInt(1, W))

      io.out := Mux(io.cond, io.in_t + io.in_f, io.in_t - io.in_f) + myLit

      io.b_o := Mux(io.cond, io.b_t, Bool(false))

    }

    trait ComplexTests extends Tests {
      def tests(c: ComplexTest) {
        for (t <- 0 until 4) {
          val test_cond     = rnd.nextInt(2)
          val test_in_t_real = rnd.nextInt(256)
          val test_in_t_imag = rnd.nextInt(256)
          val test_in_f_real = rnd.nextInt(256)
          val test_in_f_imag = rnd.nextInt(256)
          val test_b_t = rnd.nextInt(2)
          val test_b_f = rnd.nextInt(2)

          poke(c.io.cond,     test_cond)
          poke(c.io.in_t.real, test_in_t_real)
          poke(c.io.in_t.imag, test_in_t_imag)
          poke(c.io.in_f.real, test_in_f_real)
          poke(c.io.in_f.imag, test_in_f_imag)
          poke(c.io.b_t, test_b_t)
          poke(c.io.b_f, test_b_f)
          step(1)
          val result_real = if (test_cond == 1) {
            test_in_t_real + test_in_f_real + 1
          } else {
            test_in_t_real - test_in_f_real + 1
          }
          val result_imag = if (test_cond == 1) {
            test_in_t_imag + test_in_f_imag + 1
          } else {
            test_in_t_imag - test_in_f_imag + 1
          }
          expect(c.io.out.real, result_real)
          expect(c.io.out.imag, result_imag)
          expect(c.io.b_o, if (test_cond == 1) test_b_t else 0)
        }
      }
    }

    class ComplexTester(c: ComplexTest) extends Tester(c) with ComplexTests {
      tests(c)
    }

    val testArgs = chiselEnvironmentArguments() ++ Array("--compile", "--genHarness", "--test", "--targetDir", dir.getPath)
    chiselMainTest(testArgs.toArray, () => Module(new ComplexTest(16))) {
      c => new ComplexTester(c)
    }
  }
}
