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

import org.scalatest.junit.JUnitSuite
import org.scalatest._
import org.junit.Before
import org.scalatest._
import scala.reflect.ClassTag
import scala.collection.JavaConversions
import Chisel._
import TestSuite._

object TestSuite {

  // Should we enable some global settings?
  val partitionIslandsParameterName = "partitionIslands"
  val partitionIslandsArguments = chiselEnvironmentArguments(partitionIslandsParameterName)
}

abstract class TestSuite extends JUnitSuite with TestHelpers {
  // This functionality has been replaced by the static TestHelper object
  //  and should be elminated.
  @Before def initialize() {
  }

  def launchTester[M <: Module : ClassTag, T <: Tester[M]](b: String, t: M => T, fArg: Option[(Array[String]) => Array[String]] = None) {
    val ctor = implicitly[ClassTag[M]].runtimeClass.getConstructors.head

    val baseArgs = chiselEnvironmentArguments() ++ Array[String]("--backend", b,
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test")
    val testArgs = fArg match {
      case Some(f) => {
        f(baseArgs)
      }
      case None => {
        baseArgs
      }
    }
    chiselMainTest(testArgs,
      () => Module(ctor.newInstance(this).asInstanceOf[M])) {t}
    // If this is a test of the Cpp backend, launch it again with some Cpp specific arguments,
    //  if "partitionIslands" is defined in the environment and isn't one of the specified test arguments.
    if (b == "c" && partitionIslandsArguments.size != 0 && !testArgs.contains("--partitionIslands")) {
      chiselMainTest(partitionIslandsArguments ++ testArgs,
        () => Module(ctor.newInstance(this).asInstanceOf[M])) {t}
    }
  }
  def launchCppTester[M <: Module : ClassTag, T <: Tester[M]](t: M => T, fArg: Option[(Array[String]) => Array[String]] = None) = launchTester("c", t, fArg)
  def launchVerilogTester[M <: Module : ClassTag, T <: Tester[M]](t: M => T, fArg: Option[(Array[String]) => Array[String]] = None) = launchTester("v", t, fArg)
}
