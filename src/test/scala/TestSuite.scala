/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
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

import org.scalatest.junit.JUnitSuite
import org.junit.Before
import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.collection.JavaConversions
import Chisel._
import TestSuite._

object TestSuite {

  // Should we enable some global settings?
  val partitionIslandsParameterName = "partitionIslands"
  val partitionIslandsArguments = chiselEnvironmentArguments(partitionIslandsParameterName)
}

abstract class TestSuite extends JUnitSuite {

  val dir = new File("test-outputs")
  val blankLines_re = """(?m)^\s*$[\r\n]+""".r
  @Before def initialize() {
    dir.mkdir
  }

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


  def launchTester[M <: Module : ClassTag, T <: Tester[M]](b: String, t: M => T) {
    val ctor = implicitly[ClassTag[M]].runtimeClass.getConstructors.head

    val testArgs = Array[String]("--backend", b,
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test")
    chiselMainTest(testArgs,
      () => Module(ctor.newInstance(this).asInstanceOf[M])) {t}
    // If this is a test of the Cpp backend, launch it again with some Cpp specific arguments,
    //  if "partitionIslands" is defined in the environment and isn't one of the specified test arguments.
    if (b == "c" && partitionIslandsArguments.size != 0 && !testArgs.contains("--partitionIslands")) {
      chiselMainTest(partitionIslandsArguments ++ testArgs,
        () => Module(ctor.newInstance(this).asInstanceOf[M])) {t}
    }
  }
  def launchCppTester[M <: Module : ClassTag, T <: Tester[M]](t: M => T) = launchTester("c", t)
  def launchVerilogTester[M <: Module : ClassTag, T <: Tester[M]](t: M => T) = launchTester("v", t)


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
}
