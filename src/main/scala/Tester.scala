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
import scala.util.Random
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import scala.sys.process._
import Literal._

class Tester[+T <: Module](val c: T, val isTrace: Boolean = true) {
  /*
  val testIn = new Queue[Int]()
  val testOut = new Queue[Int]()
  val testErr = new Queue[Int]()
  */
  var testIn:  InputStream  = null
  var testOut: OutputStream = null
  var testErr: InputStream  = null
  val sb = new StringBuilder()
  var delta = 0
  var t = 0

  def puts(str: String) = {
    while (testOut == null) { Thread.sleep(100) }
    for (e <- str) testOut.write(e);
  }

  def setClocks(clocks: HashMap[Clock, Int]) {
    for (clock <- Module.clocks) {
      puts("set-clocks");
      if (clock.srcClock == null) {
        val s = BigInt(clocks(clock)).toString(16)
        puts(" " + s);
      }
    }
    puts("\n")
    testOut.flush()
  }

  def isSpace(c: Int) : Boolean = c == 0x20 || c == 0x9 || c == 0xD || c == 0xA

  def drainErr () = {
    if (testErr != null) {
      while(testErr.available() > 0) {
        System.err.print(Character.toChars(testErr.read()))
      }
    } else
      println("ERR NULL")
  }

  def gets() = {
    while (testIn == null) { Thread.sleep(100) }
    var c = testIn.read
    sb.clear()
    while (isSpace(c)) {
      c = testIn.read
    }
    while (!isSpace(c)) {
      sb += c.toChar
      c   = testIn.read
    }
    sb.toString
  }

  def dumpName(data: Node): String = {
    if (Module.backend.isInstanceOf[FloBackend]) {
      data.name
    } else
      data.chiselName
  }

  def peekBits(data: Node, off: Int = -1): BigInt = {
    if (dumpName(data) == "") {
      println("Unable to peek data " + data)
      -1
    } else {
      val off = -1;
      puts("peek " + dumpName(data));
      if (off != -1)
        puts(" " + off);
      puts("\n")
      testOut.flush()
      val s = gets()
      val rv = toLitVal(s)
      if (isTrace) println("  PEEK " + dumpName(data) + " " + (if (off >= 0) (off + " ") else "") + "-> " + s)
      drainErr()
      rv
    }
  }

  def peekAt[T <: Bits](data: Mem[T], off: Int): BigInt = {
    peekBits(data, off)
  }

  def peek(data: Bits): BigInt = {
    peekBits(data.getNode)
  }

  def peek(data: Aggregate /*, off: Int = -1 */): Array[BigInt] = {
    data.flatten.map(x => x._2).map(peek(_))
  }

  def reset(n: Int = 1) = {
    puts("reset " + n + "\n");  
    if (isTrace) println("RESET " + n)
    testOut.flush()
    drainErr()
  }

  def pokeBits(data: Node, x: BigInt, off: Int = -1): Unit = {
    if (dumpName(data) == "") {
      println("Unable to poke data " + data)
    } else {
      puts("poke " + dumpName(data));
      if (isTrace) println("  POKE " + dumpName(data) + " " + (if (off >= 0) (off + " ") else "") + "<- " + x)
      if (off != -1)
        puts(" " + off);
      puts(" 0x" + x.toString(16) + "\n");
      testOut.flush()
      drainErr()
    }
  }

  def pokeAt[T <: Bits](data: Mem[T], x: BigInt, off: Int): Unit = {
    pokeBits(data, x, off)
  }

  def poke(data: Bits, x: BigInt): Unit = {
    pokeBits(data, x)
  }

  def poke(data: Aggregate, x: Array[BigInt]): Unit = {
    val kv = (data.flatten.map(x => x._2), x).zipped;
    for ((x, y) <- kv)
      poke(x, y)
  }

  def step(n: Int) = {
    puts("step " + n + "\n");
    testOut.flush()
    val s = gets()
    delta += s.toInt
    if (isTrace) println("STEP " + n + " <- " + s)
    drainErr()
    t += n
  }

  def int(x: Boolean): BigInt = if (x) 1 else 0
  def int(x: Int): BigInt = x
  def int(x: Bits): BigInt = x.litValue()

  var ok = true;

  def expect (good: Boolean, msg: String): Boolean = {
    if (isTrace)
      println(msg + " " + (if (good) "PASS" else "FAIL"))
    if (!good) ok = false;
    good
  }

  def expect (data: Bits, expected: BigInt): Boolean = {
    val got = peek(data)
    expect(got == expected, 
       "EXPECT " + dumpName(data) + " <- " + got + " == " + expected)
  }

  def expect (data: Aggregate, expected: Array[BigInt]): Boolean = {
    val kv = (data.flatten.map(x => x._2), expected).zipped;
    var allGood = true
    for ((d, e) <- kv)
      allGood = expect(d, e) && allGood
    allGood
  }

  val rnd = new Random()
  var process: Process = null

  def startTesting(): Process = {
    val target = Module.targetDir + "/" + c.name
    val cmd = 
      (if (Module.backend.isInstanceOf[FloBackend]) {
         val dir = Module.backend.asInstanceOf[FloBackend].floDir
         dir + "fix-console :is-debug true :filename " + target + ".hex"
      } else {
         target + (if(Module.backend.isInstanceOf[VerilogBackend]) " -q" else "")
      })
    println("STARTING " + cmd)
    val processBuilder = Process(cmd)
    val pio = new ProcessIO(in => testOut = in, out => testIn = out, err => testErr = err)
    process = processBuilder.run(pio)
    // while(testIn == null || testErr == null) { }
    reset(5)
    process
  }

  def endTesting(): Boolean = {
    if (process != null) {
      puts("quit\n")

      if (testOut != null) {
        testOut.flush()
        testOut.close()
      }
      if (testIn != null) {
        testIn.close()
      }
      if (testErr != null) {
        testErr.close()
      }

      process.destroy()
    }
    println("RAN " + t + " CYCLES " + (if (ok) "PASSED" else "FAILED"))
    ok
  }

  startTesting()
}

class MapTester[+T <: Module](c: T, val testNodes: Array[Node]) extends Tester(c, false) {
  def splitFlattenNodes(args: Seq[Node]): (Seq[Node], Seq[Node]) = {
    if (args.length == 0) {
      (Array[Node](), Array[Node]())
    } else {
      val testNodes = args.map(i => i.maybeFlatten).reduceLeft(_ ++ _).map(x => x.getNode);
      (c.keepInputs(testNodes), c.removeInputs(testNodes))
    }
  }
  val (ins, outs) = splitFlattenNodes(testNodes)
  val testInputNodes    = ins.toArray; 
  val testNonInputNodes = outs.toArray
  def step(svars: HashMap[Node, Node],
           ovars: HashMap[Node, Node] = new HashMap[Node, Node],
           isTrace: Boolean = true): Boolean = {
    if (isTrace) { println("---"); println("INPUTS") }
    for (n <- testInputNodes) {
      val v = svars.getOrElse(n, null)
      val i = if (v == null) BigInt(0) else v.litValue() // TODO: WARN
      pokeBits(n, i)
    }
    if (isTrace) println("OUTPUTS")
    var isSame = true
    step(1)
    for (o <- testNonInputNodes) {
      val rv = peekBits(o)
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
  var tests: () => Boolean = () => { println("DEFAULT TESTS"); true }
  def defTests(body: => Boolean) = body

}
