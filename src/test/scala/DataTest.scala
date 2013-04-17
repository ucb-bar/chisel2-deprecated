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


/*

nameable                (src/main/hcl.scala)
  Node                  (src/main/Node.scala)
    Data                (src/main/Data.scala)
      Bits with proc    (src/main/Bits.scala, src/main/scala/hcl.scala)
        Bool            (src/main/Bool.scala)
        Num             (src/main/Num.scala)
          Fix           (src/main/Fix.scala)
          UFix          (src/main/UFix.scala)
            Eyum        (src/main/UFix.scala)
      Vec               (src/main/Vec.scala)
      Bundle            (src/main/Bundle.scala)
*/

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before

import Chisel._


class DataSuite extends AssertionsForJUnit {

  @Test def testBoolFromValue() {
    val tested = Bool(true);
    assertTrue( tested.dir == OUTPUT );
    assertTrue( tested.assigned );
    assertFalse( tested.named );
  }

  @Test def testBoolFromDir() {
    val tested = Bool(dir = INPUT);
    assertTrue( tested.dir == INPUT );
    assertFalse( tested.assigned );
    assertFalse( tested.named );
  }

  @Test def testBoolFromDefault() {
    val tested = Bool();
    /* XXX In the same situation Fix direction shows up as INPUT */
    assertTrue( tested.dir == null );
    assertFalse( tested.assigned );
    assertFalse( tested.named );
  }

  @Test def testFixFromLit() {
    val fixFromLit = Fix(42);

    assertTrue( fixFromLit.dir == OUTPUT );
    assertTrue( fixFromLit.isSigned ); /* XXX why defined in Node? */
    assertTrue( fixFromLit.assigned );
    assertFalse( fixFromLit.named );
  }

  @Test def testFixFromLitWithWidth() {
    val fixFromLitWithWidth = Fix(42, width = 16);
    assertTrue( fixFromLitWithWidth.dir == OUTPUT );
    assertTrue( fixFromLitWithWidth.isSigned );
    assertTrue( fixFromLitWithWidth.assigned );
    assertFalse( fixFromLitWithWidth.named );
    /* XXX width is -1 here for some reason
    assertTrue( fixFromLitWithWidth.width == 16 );
     */
  }

  @Test def testFixFromWidthDir() {
    val fixFromWidthDir = Fix(width = 8, dir = INPUT);
    assertTrue( fixFromWidthDir.width == 8 );
    assertTrue( fixFromWidthDir.dir == INPUT );
    assertTrue( fixFromWidthDir.isSigned );
    assertFalse( fixFromWidthDir.assigned );
    assertFalse( fixFromWidthDir.named );
  }

}


