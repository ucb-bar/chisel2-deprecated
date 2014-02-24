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
  var testIn:  InputStream  = null
  var testOut: OutputStream = null
  var testErr: InputStream  = null
  val sb = new StringBuilder()
  var delta = 0

  def setClocks(clocks: HashMap[Clock, Int]) {
    for (clock <- Module.clocks) {
      puts("set-clocks");
      if (clock.srcClock == null) {
        val s = BigInt(clocks(clock)).toString(16)
        puts(" " + s);
      }
    }
    testOut.write('\n')
    testOut.flush()
  }

  def isSpace(c: Int) : Boolean = c == 0x20 || c == 0x9 || c == 0xD || c == 0xA

  def puts(str: String) = {
    for (e <- str) testOut.write(e);
  }

  def drainErr () = {
    while(testErr.available() > 0) {
      System.err.print(Character.toChars(testErr.read()))
    }
  }

  def gets() = {
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

  def peekBits(data: Node, off: Int = -1): BigInt = {
    if (data.chiselName == "") {
      println("Unable to peek data " + data)
      -1
    } else {
      val off = -1;
      puts("peek " + data.chiselName);
      if (off != -1)
        puts(" " + off);
      puts("\n")
      testOut.flush()
      val s = gets()
      val rv = toLitVal(s)
      if (isTrace) println("PEEK " + data.name + " " + (if (off >= 0) (off + " ") else "") + "-> " + s)
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

  def peek(data: CompositeData /*, off: Int = -1 */): Array[BigInt] = {
    data.flatten.map(x => x._2).map(peek(_))
  }

  def reset(n: Int = 1) = {
    puts("reset " + n + "\n");  
    if (isTrace) println("RESET " + n)
    testOut.flush()
    drainErr()
  }

  def pokeBits(data: Node, x: BigInt, off: Int = -1): Unit = {
    if (data.chiselName == "") {
      println("Unable to poke data " + data)
    } else {
      puts("poke " + data.chiselName);
      if (isTrace) println("POKE " + data.name + " " + (if (off >= 0) (off + " ") else "") + "<- " + x)
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

  def poke(data: CompositeData, x: Array[BigInt]): Unit = {
    val kv = (data.flatten.map(x => x._2), x).zipped;
    for ((x, y) <- kv)
      poke(x, y)
  }

  def step(n: Int = 1) = {
    puts("step " + n + "\n");
    testOut.flush()
    val s = gets()
    delta += s.toInt
    if (isTrace) println("STEP " + n + " <- " + s)
    drainErr()
  }

  val rnd = new Random()
  var process: Process = null

  def startTesting(): Process = {
    val cmd = Module.targetDir + "/" + c.name + (if(Module.backend.isInstanceOf[VerilogBackend]) " -q" else "")
    println("STARTING " + cmd)
    val processBuilder = Process(cmd)
    val pio = new ProcessIO(in => testOut = in, out => testIn = out, err => testErr = err)
    process = processBuilder.run(pio)
    while(testIn == null) { }
    reset(5)
    process
  }

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
       "EXPECT " + data.name + " <- " + got + " == " + expected)
  }

  def expect (data: CompositeData, expected: Array[BigInt]): Boolean = {
    val kv = (data.flatten.map(x => x._2), expected).zipped;
    var allGood = true
    for ((d, e) <- kv)
      allGood = expect(d, e) && allGood
    allGood
  }

  def endTesting(): Boolean = {
    if (process != null) {
      puts("quit\n"); testOut.flush();
      testOut.close()
      testIn.close()
      testErr.close()
      process.destroy()
    }
    ok
  }

  startTesting()
}
