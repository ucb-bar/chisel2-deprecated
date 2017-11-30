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
import org.scalatest._
//import org.junit.Assert._
import org.junit.Test

import Chisel._
class VersionSuite extends TestSuite {
  @Test def testValidVersions() {
    val versions = Array[(Version, Version, String)](
      ("", "99.99.99", "!="),
      ("", "", "=="),
      ("3", "2.99.99", "!="),
      ("4", "3", "!="),
      ("3", "3.99.99", "!="),
      ("3.99.99", "3", "!="),
      ("3.0", "2.9", "!="),
      ("3.9", "3", "!="),
      ("3.9", "3.0", "!="),
      ("3.9", "3.9.99", "!="),
      ("3.2.1", "3.2.1", "==")

        )
    for((v1, v2, eq) <- versions) {
      assert(v1 >= v2)
      if (eq == "==") {
        assert(v1 == v2)
      } else {
        assert(v1 != v2)
      }
    }
  }

  @Test def testInvalidVersions() {
    val versions = Array[String](
      ("foo"),
      ("3..5"),
      ("3.4.5.6")
        )
    for(s <- versions) {
      intercept[IllegalArgumentException] {
        val v = Version(s)
      }
    }
  }
}