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

/** This testsuite checks all arbiter class.
*/
class ArbiterSuite extends TestSuite {

  @Test def testArbiter() {
    class MyArbiter extends Arbiter(UInt(width=4), 4)

    class ArbiterTests(c: MyArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,0,0,0,0)
      val in1_val = Array[BigInt](0,0,0,1,0,0,0,0,0)
      val in2_val = Array[BigInt](0,0,1,1,1,1,1,0,0)
      val in3_val = Array[BigInt](0,0,1,1,1,1,1,1,0)
      val out_rdy = Array[BigInt](0,1,0,1,1,0,1,1,0)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,1,1,0,1,1,0)
      val in1_rdy = Array[BigInt](0,1,0,1,0,0,1,1,0)
      val in2_rdy = Array[BigInt](0,1,0,0,0,0,1,1,0)
      val in3_rdy = Array[BigInt](0,1,0,0,0,0,0,1,0)
      val out_val = Array[BigInt](0,0,1,1,1,1,1,1,0)

      val out_dat = Array[BigInt](3,3,2,1,0,2,2,3,3)

      val trails = 9

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyArbiter) => new ArbiterTests(c))
  }

  @Test def testStableArbiter() {
    class MyStableArbiter extends Arbiter(UInt(width=4), 4, true)

    class ArbiterTests(c: MyStableArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,0,0,0,0)
      val in1_val = Array[BigInt](0,0,0,1,1,1,1,0,1)
      val in2_val = Array[BigInt](0,0,1,1,0,0,0,0,0)
      val in3_val = Array[BigInt](0,0,1,1,1,1,1,1,0)
      val out_rdy = Array[BigInt](0,1,0,1,1,0,1,1,1)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,0,1,0,0,1,1)
      val in1_rdy = Array[BigInt](0,1,0,0,0,0,1,1,1)
      val in2_rdy = Array[BigInt](0,1,0,1,0,0,0,1,0)
      val in3_rdy = Array[BigInt](0,1,0,0,0,0,0,1,0)
      val out_val = Array[BigInt](0,0,1,1,1,1,1,1,1)

      val out_dat = Array[BigInt](3,3,2,2,0,1,1,3,1)

      val trails = 9

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyStableArbiter) => new ArbiterTests(c))
  }

  @Test def testDefaultLockingArbiter() {
    class MyDefaultLockingArbiter extends LockingArbiter(UInt(width=4), 4, 2)

    class ArbiterTests(c: MyDefaultLockingArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,1,1,1,1,0)
      val in1_val = Array[BigInt](0,0,0,1,0,1,1,0,0,0)
      val in2_val = Array[BigInt](0,0,1,1,1,1,1,1,1,1)
      val in3_val = Array[BigInt](0,0,1,1,1,1,1,1,1,1)
      val out_rdy = Array[BigInt](0,1,0,1,1,0,1,1,1,1)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,1,0,0,0,1,1,1)
      val in1_rdy = Array[BigInt](0,1,0,1,1,0,1,0,0,1)
      val in2_rdy = Array[BigInt](0,1,0,0,0,0,0,0,0,1)
      val in3_rdy = Array[BigInt](0,1,0,0,0,0,0,0,0,0)
      val out_val = Array[BigInt](0,0,1,1,0,1,1,1,1,1)

      val out_dat = Array[BigInt](3,3,2,1,1,1,1,0,0,2)

      val trails = 10

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyDefaultLockingArbiter) => new ArbiterTests(c))
  }

  @Test def testDefaultStableLockingArbiter() {
    class MyDefaultStableLockingArbiter extends LockingArbiter(UInt(width=4), 4, 2, None, true)

    class ArbiterTests(c: MyDefaultStableLockingArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,1,1,1,1,0)
      val in1_val = Array[BigInt](0,0,0,1,1,1,1,1,1,1)
      val in2_val = Array[BigInt](0,0,1,1,0,1,1,0,0,0)
      val in3_val = Array[BigInt](0,0,1,1,1,1,1,1,1,1)
      val out_rdy = Array[BigInt](0,1,0,1,1,0,1,1,1,1)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,0,0,0,0,1,1,1)
      val in1_rdy = Array[BigInt](0,1,0,0,0,0,0,0,0,1)
      val in2_rdy = Array[BigInt](0,1,0,1,1,0,1,0,0,0)
      val in3_rdy = Array[BigInt](0,1,0,0,0,0,0,0,0,0)
      val out_val = Array[BigInt](0,0,1,1,0,1,1,1,1,1)

      val out_dat = Array[BigInt](3,3,2,2,2,2,2,0,0,1)

      val trails = 10

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyDefaultStableLockingArbiter) => new ArbiterTests(c))
  }

  @Test def testLockingArbiter() {

    //def hasDat(d: UInt):Bool = d === UInt(0)
    class MyLockingArbiter extends LockingArbiter[UInt](UInt(width=4), 4, 2, Some((d:UInt) => d === UInt(0)))

    class ArbiterTests(c: MyLockingArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,0,1,0,0,0)
      val in1_val = Array[BigInt](0,0,0,1,0,0,1,1,0,0)
      val in2_val = Array[BigInt](0,0,1,1,1,1,1,1,1,0)
      val in3_val = Array[BigInt](0,0,1,1,1,1,1,1,1,1)
      val out_rdy = Array[BigInt](0,1,0,1,1,1,1,1,1,1)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,1,1,1,1,1,1,1)
      val in1_rdy = Array[BigInt](0,1,0,1,0,0,0,1,1,1)
      val in2_rdy = Array[BigInt](0,1,0,0,0,0,0,0,1,1)
      val in3_rdy = Array[BigInt](0,1,0,0,0,0,0,0,0,1)
      val out_val = Array[BigInt](0,0,1,1,1,0,1,1,1,1)

      val out_dat = Array[BigInt](3,3,2,1,0,0,0,1,2,3)

      val trails = 10

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyLockingArbiter) => new ArbiterTests(c))
  }

  @Test def testStableLockingArbiter() {

    //def hasDat(d: UInt):Bool = d === UInt(0)
    class MyStableLockingArbiter extends LockingArbiter[UInt](UInt(width=4), 4, 2, Some((d:UInt) => d === UInt(0)), true)

    class ArbiterTests(c: MyStableLockingArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,0,1,0,0,0)
      val in1_val = Array[BigInt](0,0,0,1,1,1,1,1,0,0)
      val in2_val = Array[BigInt](0,0,1,1,0,0,0,1,1,0)
      val in3_val = Array[BigInt](0,0,1,1,1,1,1,1,1,1)
      val out_rdy = Array[BigInt](0,1,0,1,1,1,1,1,1,1)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,0,1,1,1,1,1,1)
      val in1_rdy = Array[BigInt](0,1,0,0,0,0,0,1,1,1)
      val in2_rdy = Array[BigInt](0,1,0,1,0,0,0,0,1,1)
      val in3_rdy = Array[BigInt](0,1,0,0,0,0,0,0,0,1)
      val out_val = Array[BigInt](0,0,1,1,1,0,1,1,1,1)

      val out_dat = Array[BigInt](3,3,2,2,0,0,0,1,2,3)

      val trails = 10

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyStableLockingArbiter) => new ArbiterTests(c))
  }

  @Test def testRRArbiter() {
    class MyRRArbiter extends RRArbiter(UInt(width=4), 4)

    class ArbiterTests(c: MyRRArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,1,1,1,0)
      val in1_val = Array[BigInt](0,0,0,1,0,0,0,0,0)
      val in2_val = Array[BigInt](0,0,1,1,1,0,0,1,1)
      val in3_val = Array[BigInt](0,0,1,1,1,1,1,0,0)
      val out_rdy = Array[BigInt](0,1,0,1,1,0,1,1,1)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,0,0,0,0,1,0)
      val in1_rdy = Array[BigInt](0,1,0,1,0,0,0,0,1)
      val in2_rdy = Array[BigInt](0,1,0,0,1,0,0,0,1)
      val in3_rdy = Array[BigInt](0,1,0,0,0,0,1,0,0)
      val out_val = Array[BigInt](0,0,1,1,1,1,1,1,1)

      val out_dat = Array[BigInt](3,3,2,1,2,3,3,0,2)

      val trails = 9

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyRRArbiter) => new ArbiterTests(c))
  }

  @Test def testStableRRArbiter() {
    class MyStableRRArbiter extends RRArbiter(UInt(width=4), 4, true)

    class ArbiterTests(c: MyStableRRArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,1,1,0,0)
      val in1_val = Array[BigInt](0,0,0,1,1,1,1,1,0)
      val in2_val = Array[BigInt](0,0,1,1,0,0,0,0,0)
      val in3_val = Array[BigInt](0,0,1,1,1,0,1,1,1)
      val out_rdy = Array[BigInt](0,1,0,1,1,0,1,1,1)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,0,0,0,1,0,0)
      val in1_rdy = Array[BigInt](0,1,0,0,0,0,0,1,0)
      val in2_rdy = Array[BigInt](0,1,0,1,0,0,0,0,1)
      val in3_rdy = Array[BigInt](0,1,0,0,1,0,0,0,1)
      val out_val = Array[BigInt](0,0,1,1,1,1,1,1,1)

      val out_dat = Array[BigInt](3,3,2,2,3,0,0,1,3)

      val trails = 9

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyStableRRArbiter) => new ArbiterTests(c))
  }

  @Test def testDefaultRRLockingArbiter() {
    class MyDefaultRRLockingArbiter extends LockingRRArbiter(UInt(width=4), 4, 2)

    class ArbiterTests(c: MyDefaultRRLockingArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,1,1,1,1,1)
      val in1_val = Array[BigInt](0,0,0,1,0,1,1,0,0,0)
      val in2_val = Array[BigInt](0,0,1,1,1,1,1,1,1,0)
      val in3_val = Array[BigInt](0,0,1,1,1,1,1,1,1,1)
      val out_rdy = Array[BigInt](0,1,0,1,1,0,1,1,1,1)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,0,0,0,0,0,0,0)
      val in1_rdy = Array[BigInt](0,1,0,1,1,0,1,0,0,0)
      val in2_rdy = Array[BigInt](0,1,0,0,0,0,0,1,1,0)
      val in3_rdy = Array[BigInt](0,1,0,0,0,0,0,0,0,1)
      val out_val = Array[BigInt](0,0,1,1,0,1,1,1,1,1)

      val out_dat = Array[BigInt](3,3,2,1,1,1,1,2,2,3)

      val trails = 10

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyDefaultRRLockingArbiter) => new ArbiterTests(c))
  }

  @Test def testDefaultStableRRLockingArbiter() {
    class MyDefaultStableRRLockingArbiter extends LockingRRArbiter(UInt(width=4), 4, 2, None, true)

    class ArbiterTests(c: MyDefaultStableRRLockingArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,1,1,1,1,1)
      val in1_val = Array[BigInt](0,0,0,1,1,1,1,1,1,1)
      val in2_val = Array[BigInt](0,0,1,1,0,1,1,0,0,0)
      val in3_val = Array[BigInt](0,0,1,1,1,1,1,1,1,0)
      val out_rdy = Array[BigInt](0,1,0,1,1,0,1,1,1,1)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,0,0,0,0,0,0,1)
      val in1_rdy = Array[BigInt](0,1,0,0,0,0,0,0,0,0)
      val in2_rdy = Array[BigInt](0,1,0,1,1,0,1,0,0,0)
      val in3_rdy = Array[BigInt](0,1,0,0,0,0,0,1,1,0)
      val out_val = Array[BigInt](0,0,1,1,0,1,1,1,1,1)

      val out_dat = Array[BigInt](3,3,2,2,2,2,2,3,3,0)

      val trails = 10

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyDefaultStableRRLockingArbiter) => new ArbiterTests(c))
  }

  @Test def testRRLockingArbiter() {

    //def hasDat(d: UInt):Bool = d === UInt(0)
    class MyRRLockingArbiter extends LockingRRArbiter[UInt](UInt(width=4), 4, 2, Some((d:UInt) => d === UInt(3)))

    class ArbiterTests(c: MyRRLockingArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,1,1,1,0)
      val in1_val = Array[BigInt](0,0,0,1,0,0,0,0,0)
      val in2_val = Array[BigInt](0,0,0,0,0,1,1,1,1)
      val in3_val = Array[BigInt](0,0,1,1,1,0,1,1,0)
      val out_rdy = Array[BigInt](0,1,0,1,1,1,1,1,1)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,0,0,0,0,1,0)
      val in1_rdy = Array[BigInt](0,1,0,1,0,0,0,0,1)
      val in2_rdy = Array[BigInt](0,1,0,0,1,0,0,0,1)
      val in3_rdy = Array[BigInt](0,1,0,0,1,1,1,0,0)
      val out_val = Array[BigInt](0,0,1,1,1,0,1,1,1)

      val out_dat = Array[BigInt](3,3,3,1,3,3,3,0,2)

      val trails = 9

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyRRLockingArbiter) => new ArbiterTests(c))
  }

  @Test def testStableRRLockingArbiter() {

    //def hasDat(d: UInt):Bool = d === UInt(0)
    class MyStableRRLockingArbiter extends LockingRRArbiter[UInt](UInt(width=4), 4, 2, Some((d:UInt) => d === UInt(3)), true)

    class ArbiterTests(c: MyStableRRLockingArbiter) extends Tester(c) {

      // driver
      val in0_val = Array[BigInt](0,0,0,0,1,1,1,0,0)
      val in1_val = Array[BigInt](0,0,0,1,1,1,0,0,0)
      val in2_val = Array[BigInt](0,0,0,0,0,0,0,1,0)
      val in3_val = Array[BigInt](0,0,1,1,0,1,0,0,0)
      val out_rdy = Array[BigInt](0,1,0,1,1,1,1,1,1)

      // expected output
      val in0_rdy = Array[BigInt](0,1,0,0,0,0,1,0,1)
      val in1_rdy = Array[BigInt](0,1,0,0,0,0,0,1,1)
      val in2_rdy = Array[BigInt](0,1,0,0,0,0,0,1,1)
      val in3_rdy = Array[BigInt](0,1,0,1,1,1,0,0,1)
      val out_val = Array[BigInt](0,0,1,1,0,1,1,1,0)

      val out_dat = Array[BigInt](3,3,3,3,3,3,0,2,3)

      val trails = 9

      for(i <- 0 until 4) {
        poke(c.io.in(i).bits, i)
        poke(c.io.in(i).valid, 0)
      }

      for(t <- 0 until trails) {
        poke(c.io.in(0).valid, in0_val(t))
        poke(c.io.in(1).valid, in1_val(t))
        poke(c.io.in(2).valid, in2_val(t))
        poke(c.io.in(3).valid, in3_val(t))
        poke(c.io.out.ready, out_rdy(t))

        expect(c.io.in(0).ready, in0_rdy(t))
        expect(c.io.in(1).ready, in1_rdy(t))
        expect(c.io.in(2).ready, in2_rdy(t))
        expect(c.io.in(3).ready, in3_rdy(t))
        expect(c.io.out.valid, out_val(t))

        expect(c.io.out.bits, out_dat(t))

        step(1)
      }
    }

    launchCppTester((c: MyStableRRLockingArbiter) => new ArbiterTests(c))
  }

}
