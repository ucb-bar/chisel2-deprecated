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

import scala.collection.mutable.ArrayBuffer
import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore


import Chisel._


/** This testsuite checks all methods in the Bits class.
*/
class BitsSuite extends AssertionsForJUnit {

  /** Extract a bit from a constant at a fixed position */
  @Test def testExtractConstantFixed() {
    val res = UInt(5)(0)
    assertTrue( res.getWidth == 1 )
    assertTrue( res.node.asInstanceOf[Literal].value == 1 )
  }

  /** Extract a bit from a constant at a variable position */
  @Test def testExtractConstantVariable() {
      val res = UInt(5)(Bool(INPUT))
      /* XXX Until we specialize the extract-one-bit operation
       the width is unknown at this point.

      assertTrue( res.getWidth == 1 )
       */
      assertTrue( res.node.isInstanceOf[AndOp] )
  }

  /** Extract a bit from a variable at a variable position */
  @Test def testExtractVariableVariable() {
      val res = UInt(INPUT, 32)(Bool(INPUT))
      /* XXX Until we specialize the extract-one-bit operation
       the width is unknown at this point.

      assertTrue( res.getWidth == 1 )
       */
      assertTrue( res.node.isInstanceOf[ExtractOp] )
  }

  /** Extract from a constant a fixed range of bits */
  @Test def testExtractConstantRangeFixed() {
    val res = UInt(5)((1, 0))
    assertTrue( res.getWidth == 2 )
  }

  /** Extract from a constant a variable range of bits */
  @Test def testExtractConstantRangeVariable() {
    val res = UInt(5)(Bool(INPUT), Bool(INPUT))
    assertTrue( res.node.isInstanceOf[AndOp] )
  }

  /** Equality */
  @Test def testEql() {
    val res = Bits(2) === Bits(2)
    assertTrue( res.getWidth == 1 )
    assertTrue( res.node.asInstanceOf[Literal].value == 1 )
  }

  @Ignore("field not showing in Bundle.elements") @Test def testEqlBundle() {
    val res = Bits(2) === new Bundle{ val abc = Bits(2) }
    assertTrue( res.getWidth == 1 )
  }

  @Test def testEqlVec() {
    val res = Bits(2) === Vec(Bits(2) :: Nil)
    assertTrue( res.getWidth == 1 )
  }

  @Test def testNeg() {
    val res = ~Bits(2)
    assertTrue( res.getWidth == 2 )
    assertTrue( res.node.asInstanceOf[Literal].value == 1 )
  }

  /* AND Reduction */
  @Test def testAndR() {
    val res = Bits(5).andR
    assertTrue( res.getWidth == 1 )
  }

  /* OR Reduction */
  @Test def testOrR() {
    val res = Bits(5).orR
    assertTrue( res.getWidth == 1)
  }

  /* XOR Reduction */
  @Test def testXorR() {
    val res = Bits(5).xorR
    assertTrue( res.getWidth == 1)
  }

  /* inequality */
  @Test def testNeq() {
    val res = Bits(5) != Bits(4)
    assertTrue( res.getWidth == 1 )
    assertTrue( res.node.asInstanceOf[Literal].value == 1 )
  }

  /* bitwise and */
  @Test def testAnd() {
    val res = Bits(5) & Bits(4)
    assertTrue( res.getWidth == 3 )
    assertTrue( res.node.asInstanceOf[Literal].value == 4 )
  }

  /* bitwise or */
  @Test def testOr() {
    val res = Bits(5) | Bits(4)
    assertTrue( res.getWidth == 3 )
    assertTrue( res.node.asInstanceOf[Literal].value == 5 )
  }

  /* bitwise xor */
  @Test def testXor() {
    val res = Bits(5) ^ Bits(4)
    assertTrue( res.getWidth == 3 )
    assertTrue( res.node.asInstanceOf[Literal].value == 1 )
  }

  /* Concatenation */
  @Test def testCat() {
    try {
    val res = Bits(5) ## Bits(4)
    assertTrue( res.getWidth == 6 )
    assertTrue( res.node.asInstanceOf[Literal].value == 44 )
    } catch {
      case e => e.printStackTrace()
    }
  }
}
