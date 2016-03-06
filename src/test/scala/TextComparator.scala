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

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scala.io.Source._
import java.io._

import TextComparator._

/*
 * Comparator errors.
 */
class ComparatorError (amsg: String, alineNo: Option[Int] = None, alineString: Option[String] = None) extends Exception {
  val msg = "Error: %s" format amsg
  val lineNo = alineNo
  val lineString = alineString
  override def toString = msg
}

object TextComparator {
  val tokenRegEx = """(?!^)\b""".r                      // Break on (but capture) non-word characters.
  val EOLRegex = """.*\n""".r                         // recognize EOL
  val allowedDifferencePrefixRegex = """\b[RT]""".r   // The only words we allow for substitution are Registers and Temporaries
  def tokenize(s: String): Array[String] = {
      tokenRegEx.split(s)
  }
}

class TextComparator {
  // The substitution map is used to map one Register or Temporary name into another.
  // For diagnostics, we also record the line number (and the line itself) which triggered the substitution.
  var substitutions = HashMap[String, (String, Int, String)]()

  /*
    Add new substitutions to the map. If we already have a different substitute for any element,
    throw an exception, otherwise return None (indicating success)
   */
  def addSubstitutions(newSubstitutions: HashMap[String,String], lineNo: Int, lineString: String): Option[ComparatorError] =  {
    for ((original, substitute) <- newSubstitutions) {
      // Do we already have this substitution?
      if (TextComparator.this.substitutions.contains(original)) {
        // If we do, it better be the same one.
        val (oldValue, oldLineNo, oldLineString) = substitutions(original)
        if (oldValue != substitute) {
          // Oops. We've already set up a different map for this value.
          throw new ComparatorError("illegal substitution - \"%s\" -> \"%s\" line %d, \"%s\" - already exists - \"%s\" -> \"%s\" line %d, \"%s\"" format (substitute, original, lineNo, lineString, oldValue, original, oldLineNo, oldLineString), Some(lineNo), Some(lineString))
        }
      } else if (TextComparator.this.substitutions.values.exists(_._1 == substitute)) {
        // The substitution map must be one-to-one, but it appears there is already an entry with this substitution.
        // Retrieve it for the error report.
        val duplicate = TextComparator.this.substitutions.find({ case (k,(v, i, s)) => v == substitute && k != original})
        duplicate match {
          case Some((oldOriginal, (oldValue, oldLineNo, oldLineString))) => {
            throw new ComparatorError("illegal substitution - \"%s\" ->  \"%s\" line %d, \"%s\" - duplicate map - \"%s\" -> \"%s\" line %d, \"%s\"" format (substitute, original, lineNo, lineString, oldOriginal, oldValue, oldLineNo, oldLineString), Some(lineNo), Some(lineString))
          }
          case _ => {
            throw new ComparatorError("reality failure - \"%s\" in map but key not found" format (substitute))
          }
        }
      } else {
        // This is a new substitution map entry.
        substitutions(original) = (substitute, lineNo, lineString )
      }
    }
    None
  }

  /*
      Compare a line from the original and the test strings and return a likely list of substitutions
      to get them to match.
   */
  def substitutesFromLine(lineNo: Int, originalLine: String, testLine: String): Option[HashMap[String, String]] = {
    // Try the simplest test first.
    if (originalLine == testLine)
        return None
    val substitutions = HashMap[String, String]()
    // The lines are different. Tokenize them and compare tokens.
    val originalTokens = tokenize(originalLine)
    val testTokens = tokenize(testLine)
    for ((original, test) <- originalTokens zip testTokens) {
      if (original != test) {
        // Tokens don't match. Currently, we only allow mismatches for registers and temporaries.
        if (original(0) == test(0) && allowedDifferencePrefixRegex.findFirstIn(original) != None) {
          // This looks like a possible substitution. Remember it.
          substitutions(test) = original
        } else {
          throw new ComparatorError("illegal substitution - can\'t substitute \"%s\" for \"%s\"" format (original, test), Some(lineNo), Some(testLine))
        }
      }
    }
    return Some(substitutions)
  }

  def generateSubstitutes(original: String, test: String): Option[ComparatorError] = {
    var result: Option[ComparatorError] = None
    try {
      val originalLines = original.split("\n")
      val testLines = test.split("\n")
      // We should have the same number of lines of each.
      if (originalLines.length != testLines.length) {
        throw new ComparatorError("original(%d) and test(%d) line count differ" format (originalLines.length, testLines.length))
      }
      var lineNo = 0
      for ((originalLine, testLine) <- originalLines zip testLines) {
        lineNo += 1
        val substitutions = substitutesFromLine(lineNo, originalLine, testLine)
        substitutions match {
          case Some(subs) => {
            val result = addSubstitutions(subs, lineNo, testLine)
            if (result != None) {
              return result
            }
          }
          case _ => {}
        }
      }
    } catch {
      case e: ComparatorError => result = Some(e)
    }
  result
  }

  def compareWithSubstitutions(original:String, test: String): Option[ComparatorError] = {
    val originalTokens = tokenize(original)
    val testTokens = tokenize(test)
    if (originalTokens.length != testTokens.length)
        return Some(new ComparatorError("original(%d) and test(%d) token count differ" format (originalTokens.length, testTokens.length)))
    // We maintain the fiction that we're doing line by line comparisons.
    var originalLine = ""
    var testLine = ""
    // Generate the substituted test tokens
    for ((original, test) <- originalTokens zip testTokens) {
      originalLine += original
      testLine += test
      var value: String = ""
      if (substitutions.contains(test)) {
        val substitute = substitutions(test)
        value = substitute._1
      } else {
        value = test
      }
      if (original != value) {
        return Some(new ComparatorError("\"%s\" != \"%s\"" format (originalLine, testLine)))
      }
      // If this is a newline, reset our accumulated line
      if (EOLRegex.findFirstIn(original) != None)
          originalLine = ""
      // NOTE: This is safe since we don't allow substitution of non-(R|T)words,
      // i.e., if the original test token was EOL, it will still be EOL.
      if (EOLRegex.findFirstIn(test) != None)
          testLine = ""
    }
    None
  }

  def compareText(original: String, test: String): Option[ComparatorError] = {
    var aResult: Option[ComparatorError] = None
    // Easy test first.
    if (original != test) {
      aResult = generateSubstitutes(original, test)
      // Do they match?
      if (aResult == None) {
        // They don't match, but we have some substitutions to try.
        // Strictly speaking, if we get this far, we know they will
        // match, since we've been able to generate a consistent set
        // of substitutions.
        aResult = compareWithSubstitutions(original, test)
      }
    }
    aResult
  }

  /* If possible, apply substitutions to convert "test" into "original".
   * If not possible, return "text" unaltered.
   */
  def substituteTextIfPossible(original: String, test: String): String = {
    if (original == test) {
      test
    } else {
      val aResult = generateSubstitutes(original, test)
      // If we can generate subsitutions, we know they match,
      // and we could return "original" here.
      if (aResult == None) {
        val testTokens = tokenize(test)
        // Generate the substituted test tokens
        (testTokens.map(test => { if (substitutions.contains(test)) substitutions(test)._1 else test})).mkString
      } else {
        test
      }
    }
  }
}

object TestComparator {
  def compareFiles(masters: Array[String], tests: Array[String]): Option[ComparatorError] = {
    var finalResult: Option[ComparatorError] = None
    val comparator = new TextComparator()
    for ((masterFilePath, testFilePath) <- masters zip tests) {
      val masterFile = fromFile(masterFilePath)
      val original = masterFile.mkString
      masterFile.close()
      val testFile = fromFile(testFilePath)
      val test = testFile.mkString
      testFile.close()
      val aResult = comparator.compareText(original, test)
      aResult match {
        case Some(e: Exception) => println(e)
        case None =>{}
      }
      if (finalResult == None) {
        finalResult = aResult
      }
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
