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

import org.scalatest.Assertions
import java.io.File

import Chisel._
import TestHelpers._

object TestHelpers {

  val dir = new File("test-outputs")
  val blankLines_re = """(?m)^\s*$[\r\n]+""".r

  // Ensure our output directory exists before we run any tests.
  dir.mkdir
  Driver.initChisel(Array[String]())
}

trait TestHelpers extends Assertions {

  def assertFile( filename: String ) {
    val useNewCompare = true
    val url = getClass.getResource(filename)
    // Did we find the resource?
    if (url == null) {
      println("assertFile: \"%s\" not found".format(filename))
      // Make sure we don't inadvertently pass this test.
      assertResult(filename) { "" }
      return
    }
    val reffile = scala.io.Source.fromURL(url)
    val refText = blankLines_re.replaceAllIn(reffile.mkString, "")
    reffile.close()
    val testfile = scala.io.Source.fromFile(
      dir.getPath + "/" + filename, "utf-8")
    val testText = blankLines_re.replaceAllIn(testfile.mkString, "")
    testfile.close()
    if (useNewCompare) {
      val comparator = new TextComparator()
      val testTextWithSubstitutions = comparator.substituteTextIfPossible(refText, testText)
      assertResult(refText) { testTextWithSubstitutions }
    } else {
      assertResult(refText) { testText }
    }
  }

  def assertVCDFile( filename: String ) {
    val masterPath = getClass.getResource(filename)
    // Did we find the resource?
    if (masterPath == null) {
      println("assertFile: \"%s\" not found".format(filename))
      // Make sure we don't inadvertently pass this test.
      assertResult(filename) { "" }
      return
    }
    val reffile = scala.io.Source.fromURL(masterPath)
    val refText = reffile.getLines.filter(_ != "").toArray
    reffile.close()
    val testPath = dir.getPath + "/" + filename
    val testfile = scala.io.Source.fromFile(testPath, "utf-8")
    val testText = testfile.getLines.filter(_ != "").toArray
    testfile.close()
    val comparator = new VCDComparator(masterPath.getPath(), testPath)
    val result = comparator.compare(refText.toIterator, testText.toIterator)
    assert(result == None)
  }

  class BoolIO extends Bundle {
    val in  = Bool(INPUT)
    val out = Bool(OUTPUT)
  }

  class UIntIO extends Bundle {
    val in  = UInt(INPUT, 4)
    val out = UInt(OUTPUT, 4)
  }

  class DecoupledUIntIO extends Bundle {
    val in = Decoupled(UInt(width = 4)).flip
    val out = Decoupled(UInt(width = 4))
  }

  class EnableIO extends Bundle {
    val en  = Bool(INPUT)
    val in  = UInt(INPUT, 4)
    val out = UInt(OUTPUT, 4)
  }
  // Create the singleton object,and as a side-effect, ensure the test output directory exists.
  val dir = TestHelpers.dir
}
