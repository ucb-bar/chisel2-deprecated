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

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.io.Source._
import java.io._

import TextComparator._
import VCDComparator._

class VCDComparator(masterPath: String, testPath: String) {
  def compare(masterLines: Iterator[String], testLines: Iterator[String]): Option[ComparatorError] = {
    var result: Option[ComparatorError] = None
    try {
      // Find the signal definitions.
      val (masterDefs, masterData) = masterLines.partition(_.startsWith("$"))
      val (testDefs, testData) = testLines.partition(_.startsWith("$"))
      val masterSignals = buildSignalDefs(masterDefs)
      val testSignals = buildSignalDefs(testDefs)
      val masterReset = findSignal(masterSignals, "reset", "master")
      val testReset = findSignal(testSignals, "reset", "test")
      // Position to the location where reset is de-asserted.
      val masterResetText = "b0 %s".format(masterReset)
      val testResetText = "b0 %s".format(testReset)
      val masterDataPostReset = masterData.dropWhile((_ != masterResetText))
      val testDataPostReset = testData.dropWhile((_ != testResetText))
      if (!(masterDataPostReset sameElements testDataPostReset)) {
        throw new ComparatorError("%s and %s differ".format(masterPath, testPath))
      }
    } catch {
      case e: ComparatorError => result = Some(e)
    }
    result
    
  }
}

object VCDComparator {
  val signalDefRegex = """\$var wire (\d+) (\S+) (\S+) \$end""".r

  def buildSignalDefs(si: Iterator[String]): Map[String, String] = {
    val tSignalMap = scala.collection.mutable.HashMap[String, String]()
    for (s <- si ) {
      s match {
        case signalDefRegex(width, rep, name) => tSignalMap.put(rep, name)
        case _ => {}
      }
    }
    tSignalMap.toMap
  }

  def findSignal(map: Map[String, String], signalName: String, fileName: String): String = {
    map.find({case (k, v) => v == signalName}) match {
      case Some((key: String, value: String)) => key
      case None => throw new ComparatorError("no signal %s in %s".format(signalName, fileName))
    }
  }
  def compareFiles(masters: Array[String], tests: Array[String]): Option[ComparatorError] = {
    var finalResult: Option[ComparatorError] = None
    try {
      for ((masterFilePath, testFilePath) <- masters zip tests) {
        val masterFile = fromFile(masterFilePath)
        val masterLines = masterFile.getLines
        masterFile.close()
        val testFile = fromFile(testFilePath)
        val testLines = testFile.getLines
        testFile.close()
        val comparator = new VCDComparator(masterFilePath, testFilePath)
        val result = comparator.compare(masterLines.toIterator, testLines.toIterator)
        result match {
          case Some(e: ComparatorError) => throw e
          case _ => {}
        }
      }
    } catch {
      case e: ComparatorError => finalResult = Some(e)
    }
    finalResult
  }

  def main(args: Array[String]) {
    var files = Array[String]()
    var verbose = false
    var master = ""
    var test = ""
    var i = 0
    while (i < args.length) {
      args(i) match {
        case "-v" => verbose = true
        case "-m" => master = {i += 1; args(i)}
        case "-t" => test = {i += 1; args(i)}
        case _ => files :+= args(i)
      }
      i += 1
    }
    val masters = ArrayBuffer[String]()
    val tests = ArrayBuffer[String]()
    if (master == "" && test == "") {
      if (files.length != 2) {
          println("need two files to compare\n")
          sys.exit(2)
      }
      masters += files(0)
      tests += files(1)
    } else {
      val masterFOD = new java.io.File(master)
      val testFOD = new java.io.File(test)
      if (!(masterFOD.isDirectory() && testFOD.isDirectory())) {
        println("master and test must be directories\n")
        sys.exit(2)
      }
      if (files.length == 0)
        files = masterFOD.list()
      for (f <- files) {
        masters += master + "/" + f
        tests += test + "/" + f
      }
    }

    val result = compareFiles(masters.toArray, tests.toArray)
    sys.exit(if (result == None) 0 else 1)
  }
}
