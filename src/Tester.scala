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
import Component._

class Tester[+T <: Component](val c: T, val testNodes: Array[Node]) {
  var testIn: InputStream = null
  var testOut: OutputStream = null
  var testInputNodes: Array[Node] = null
  var testNonInputNodes: Array[Node] = null 
  def splitFlattenNodes(args: Seq[Node]): (Seq[Node], Seq[Node]) = {
    if (args.length == 0) {
      (Array[Node](), Array[Node]())
    } else {
      val testNodes = args.map(i => i.maybeFlatten).reduceLeft(_ ++ _).map(x => x.getNode);
      (c.keepInputs(testNodes), c.removeInputs(testNodes))
    }
  }
  def step(svars: HashMap[Node, Node], 
           ovarsI: HashMap[Node, Node] = null, 
           isTrace: Boolean = true): Boolean = {
    val ovars = if (ovarsI == null) svars else ovarsI;
    if (isTrace) {
        println("---")
        println("INPUTS")
    }
    for (n <- testInputNodes) {
      val v = svars.getOrElse(n, null)
      val i = if (v == null) BigInt(0) else v.litValue() // TODO: WARN
      val s = i.toString(16)
      if (isTrace) println("  " + n + " = " + i)
      testOut.write(' ')
      for (c <- s)
        testOut.write(c)
      testOut.write('\n')
      testOut.flush
    }
    if (isTrace) println("OUTPUTS")
    var isSame = true
    var c = testIn.read
    val sb = new StringBuilder()
    for (o <- testNonInputNodes) {
      sb.clear()
      def isSpace(c: Int) = c == 0x20 || c == 0x9 || c == 0xD || c == 0xA
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
    val cmd = targetDir + "/" + c.name
    val process = Process(cmd)
    val pio = new ProcessIO(in => testOut = in, out => testIn = out, err => err.close())
    val p = process.run(pio) 
    println("STARTING " + cmd)
    p
  }
  def endTest(p: Process) = {
    testOut.close()
    testIn.close()
    p.destroy()
  }
  def withTesting(body: => Boolean): Boolean = {
    var res = false
    var p: Process = null
    try {
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
  def defTests(body: => Boolean) = {
    val (ins, outs) = splitFlattenNodes(testNodes)
    testInputNodes = ins.toArray; testNonInputNodes = outs.toArray
    tests = () => withTesting { body }
  }
}
