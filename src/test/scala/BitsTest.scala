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
import org.junit.Ignore


import Chisel._


/** This testsuite checks all methods in the Bits class.
*/
class BitsSuite extends TestSuite {

  /** Extract a bit from a constant at a fixed position */
  @Test def testExtractConstantFixed() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = UInt(5)(0)
      assertTrue( res.getWidth == 1 )
      assertTrue( res.litValue() == 1 )
    }
    val dummyInst = Module(new Dummy)
  }

  /** Extract from a constant a fixed range of bits */
  @Test def testExtractConstantRangeFixed() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = UInt(5)((1, 0))
      assertTrue( res.getWidth == 2 )
    }
    val dummyInst = Module(new Dummy)
  }

  /** Equality */
  @Test def testEql() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = Bits(2) === Bits(2)
      assertTrue( res.getWidth == 1 )
      assertTrue( res.litValue() == 1 )
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testEqlBundle() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = Bits(2) === new Bundle{ val abc = Bits(2) }.toBits
      assertTrue( res.getWidth == 1 )
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testEqlVec() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = Bits(2) === Vec(Bits(2) :: Nil).toBits
      assertTrue( res.getWidth == 1 )
    }
    val dummyInst = Module(new Dummy)
  }

  @Test def testNeg() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = ~Bits(2)
      assertTrue( res.getWidth == 2 )
      assertTrue( res.litValue() == 1 )
    }
    val dummyInst = Module(new Dummy)
  }

  /* AND Reduction */
  @Test def testAndR() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = Bits(5).andR
      assertTrue( res.getWidth == 1 )
    }
    val dummyInst = Module(new Dummy)
  }

  /* OR Reduction */
  @Test def testOrR() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = Bits(5).orR
      assertTrue( res.getWidth == 1)
    }
    val dummyInst = Module(new Dummy)
  }

  /* XOR Reduction */
  @Test def testXorR() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = Bits(5).xorR
      assertTrue( res.getWidth == 1)
    }
    val dummyInst = Module(new Dummy)
  }

  /* inequality */
  @Test def testNeq() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = Bits(5) =/= Bits(4)
      assertTrue( res.getWidth == 1 )
      assertTrue( res.litValue() == 1 )
    }
    val dummyInst = Module(new Dummy)
  }

  /* bitwise and */
  @Test def testAnd() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = Bits(5) & Bits(4)
      assertTrue( res.getWidth == 3 )
      assertTrue( res.litValue() == 4 )
    }
    val dummyInst = Module(new Dummy)
  }

  /* bitwise or */
  @Test def testOr() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = Bits(5) | Bits(4)
      assertTrue( res.getWidth == 3 )
      assertTrue( res.litValue() == 5 )
    }
    val dummyInst = Module(new Dummy)
  }

  /* bitwise xor */
  @Test def testXor() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      val res = Bits(5) ^ Bits(4)
      assertTrue( res.getWidth == 3 )
      assertTrue( res.litValue() == 1 )
    }
  }

  /* Concatenation */
  @Test def testCat() {
    class Dummy extends Module {
      val io = UInt(INPUT, 0)
      try {
        val res = Bits(5) ## Bits(4)
        assertTrue( res.getWidth == 6 )
        assertTrue( res.litValue() == 44 )
      } catch {
	case e : Throwable => e.printStackTrace()
      }
    }
    val dummyInst = Module(new Dummy)
  }
}
