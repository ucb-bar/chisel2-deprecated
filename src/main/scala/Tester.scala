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

package Chisel
import Chisel._
import scala.math._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import scala.sys.process._
import Literal._

class Tester[+T <: Module](val c: T, val testNodes: Array[Node]) {
  var testIn: InputStream = null
  var testOut: OutputStream = null
  var testInputNodes: Array[Node] = null
  var testNonInputNodes: Array[Node] = null
  var first = true
  def splitFlattenNodes(args: Seq[Node]): (Seq[Node], Seq[Node]) = {
    if (args.length == 0) {
      (Array[Node](), Array[Node]())
    } else {
      val testNodes = args.map(i => i.maybeFlatten).reduceLeft(_ ++ _).map(x => x.getNode);
      (c.keepInputs(testNodes), c.removeInputs(testNodes))
    }
  }
  def step(svars: HashMap[Node, Node],
           ovars: HashMap[Node, Node] = new HashMap[Node, Node],
           isTrace: Boolean = true): Boolean = {
    if (isTrace) {
        println("---")
        println("INPUTS")
    }
    for (n <- testInputNodes) {
      val v = svars.getOrElse(n, null)
      val i = if (v == null) BigInt(0) else v.litValue() // TODO: WARN
      val s = i.toString(16)
      if (isTrace) println("  " + n + " = " + i)
      for (c <- s) {
        testOut.write(c)
      }
      testOut.write(' ')
    }
    testOut.write('\n')
    testOut.flush()
    if (isTrace) println("OUTPUTS")
    var isSame = true
    var c = testIn.read
    val sb = new StringBuilder()
    for (o <- testNonInputNodes) {
      sb.clear()
      def isSpace(c: Int) : Boolean = c == 0x20 || c == 0x9 || c == 0xD || c == 0xA
      while (isSpace(c)) {
        c = testIn.read
      }
      while (!isSpace(c)) {
        sb += c.toChar
        c   = testIn.read
      }
      val s = sb.toString
      val rv = toLitVal(s)
      if (isTrace) println("  READ " + o + " = " + rv)
      if (!svars.contains(o)) {
        ovars(o) = Literal(rv)
      } else {
        val tv = svars(o).litValue()
        if (isTrace) println("  EXPECTED: " + o + " = " + tv)
        if (tv != rv) {
          isSame = false
          if (isTrace) println("  *** FAILURE ***")
        } else {
          if (isTrace) println("  SUCCESS")
        }
      }
    }
    isSame
  }
  def startTest: Process = {
    val cmd = Module.targetDir + "/" + c.name + (if(Module.backend.isInstanceOf[VerilogBackend]) " -q" else "")
    val process = Process(cmd)
    val pio = new ProcessIO(in => testOut = in, out => testIn = out, err => err.close())
    val p = process.run(pio)
    println("STARTING " + cmd)
    p
  }
  def endTest(p: Process) {
    testOut.close()
    testIn.close()
    p.destroy()
  }
  def withTesting(body: => Boolean): Boolean = {
    var res = false
    var p: Process = null
    try {
      while(testIn == null)

      p = startTest
      res = body
    } finally {
      if (p != null) endTest(p)
    }
    println(if (res) "PASSED" else "*** FAILED ***")
    res
  }
  var tests: () => Boolean = () => { println("DEFAULT TESTS"); true }
  var testVars: Array[Node] = null
  def defTests(body: => Boolean) {
    val (ins, outs) = splitFlattenNodes(testNodes)
    testInputNodes = ins.toArray; testNonInputNodes = outs.toArray
    tests = () => withTesting { body }
  }
}
