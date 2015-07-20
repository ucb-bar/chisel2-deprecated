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

package Chisel
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.immutable.ListSet
import scala.util.Random
import java.io._
import scala.sys.process.{Process, ProcessIO}

class Tester[+T <: Module](c: T, isTrace: Boolean = true) extends FileSystemUtilities {
  var testIn: Option[InputStream] = None
  var testErr: Option[InputStream] = None
  var testOut: Option[OutputStream] = None
  lazy val reader: BufferedReader = new BufferedReader(new InputStreamReader(testIn.get))
  lazy val writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(testOut.get))
  lazy val errReader: BufferedReader = new BufferedReader(new InputStreamReader(testErr.get))
  var t = 0 // simulation time
  var delta = 0
  var testOutputString = "" // output available to test code.
  val sb = new StringBuilder()
  val pokeMap = HashMap[Bits, BigInt]()
  val peekMap = HashMap[Bits, BigInt]()
  lazy val signalMap = Driver.signalMap
  val (inputs: ListSet[Bits], outputs: ListSet[Bits]) = ListSet(c.wires.unzip._2: _*) partition (_.dir == INPUT)
  var isStale = false

  object SIM_CMD extends Enumeration { val RESET, STEP, UPDATE, POKE, PEEK, FIN = Value }
  /**
   * Waits until the emulator streams are ready. This is a dirty hack related
   * to the way Process works. TODO: FIXME.
   */
  def waitForStreams() = {
    var waited = 0
    while (testIn == None || testOut == None || testErr == None) {
      Thread.sleep(100)
      if (waited % 10 == 0 && waited > 30) {
        ChiselError.info("waiting for emulator process streams to be valid ...")
      }
    }
  }

  private def writeln(str: String) {
    writer write str
    writer.newLine
    writer.flush
  }

  private def dumpErrors = {
    var err: Option[String] = None
    do {
      err = Option(errReader.readLine)
      println(err getOrElse "")
    } while (err != None)
    throw new Exception("Errors occurred in simulation")
  }

  private def readln: String = {
    Option(reader.readLine) match {
      case None => dumpErrors 
      case Some(ln) => ln
    }
  }

  private def sendCmd(cmd: SIM_CMD.Value) {
    writeln(cmd.id.toString)
  }

  private val writeMask = int(-1L) 
  private def writeValue(x: Node, v: BigInt) {
    for (i <- ((x.needWidth - 1) >> 6) to 0 by -1) {
      writeln(((v >> (64 * i)) & writeMask).toString(16))
    }
  }

  def dumpName(data: Node): String = Driver.backend match {
    case _: FloBackend | _: CppBackend => data.getNode.name
    case _ => data.getNode.chiselName
  }

  def setClocks(clocks: HashMap[Clock, Int]) {
    var cmd = "set_clocks"
    for (clock <- Driver.clocks) {
      if (clock.srcClock == null) {
        val s = BigInt(clocks(clock)).toString(16)
        cmd = cmd + " " + s
      }
    }
  }

  def signed_fix(dtype: Bits, rv: BigInt): BigInt = {
    val w = dtype.needWidth()
    dtype match {
      /* Any "signed" node */
      case _: SInt | _ : Flo | _: Dbl => (if(rv >= (BigInt(1) << w - 1)) (rv - (BigInt(1) << w)) else rv)
      /* anything else (i.e., UInt) */
      case _ => (rv)
    }
  }

  def peekBits(data: Node, off: Option[Int] = None) = {
    if (signalMap contains data) {
      val id = signalMap(data) + (off getOrElse 0)
      sendCmd(SIM_CMD.PEEK)
      writeln(id.toString)
      BigInt(readln, 16)
    } else {
      BigInt(0)
    }
  }

  def peekAt[T <: Bits](data: Mem[T], off: Int): BigInt = {
    val value = peekBits(data, Some(off))
    if (isTrace) println("  PEEK %s[%d] -> %s".format(data.chiselName, off, value.toString(16)))
    value
  }
  def peek(data: Bits): BigInt = {
    if (isStale) update
    val value = signed_fix(data, peekMap getOrElse (data, peekBits(data.getNode)))
    if (isTrace) println("  PEEK " + data.getNode.chiselName + " -> " + value.toString(16))
    value
  }
  def peek(data: Aggregate): Array[BigInt] = {
    data.flatten.map(x => x._2) map (peek(_))
  }

  private def pokeBits(data: Node, v: BigInt, off: Option[Int] = None): Unit = {
    val id = signalMap(data) + (off getOrElse 0)
    sendCmd(SIM_CMD.POKE)
    writeln(id.toString)
    writeValue(data, v)
  }
  def pokeAt[T <: Bits](data: Mem[T], value: BigInt, off: Int): Unit = {
    if (isTrace) println("  POKE %s[%d] <- %s".format(data.chiselName, off, value.toString(16)))
    pokeBits(data, value, Some(off))
  }
  def poke(data: Bits, x: Boolean) { this.poke(data, int(x)) }
  def poke(data: Bits, x: Int)     { this.poke(data, int(x)) }
  def poke(data: Bits, x: Long)    { this.poke(data, int(x)) }
  def poke(data: Bits, x: BigInt)  {
    val value = if (x >= 0) x else {
      val cnt = (data.needWidth() - 1) >> 6
      ((0 to cnt) foldLeft BigInt(0))((res, i) => res | (int((x >> (64 * i)).toLong) << (64 * i)))
    }
    if (isTrace) println("  POKE " + data.getNode.chiselName + " <- " + value.toString(16))
    if (inputs contains data) 
      pokeMap(data) = value
    else if (signalMap contains data.getNode)
      pokeBits(data.getNode, value)
    else 
      println("  POKE is not suppoted for " + data.getNode.chiselName)
    isStale = true
  }
  def poke(data: Aggregate, x: Array[BigInt]): Unit = {
    val kv = (data.flatten.map(x => x._2), x.reverse).zipped
    for ((x, y) <- kv) poke(x, y)
  }

  private def readOutputs {
    peekMap.clear
    outputs foreach (x => peekMap(x) = BigInt(readln, 16))
  }

  private def writeInputs {
    inputs foreach (x => writeValue(x, pokeMap getOrElse (x, BigInt(0))))
  }

  def reset(n: Int = 1) {
    if (isTrace) println("RESET " + n)
    for (i <- 0 until n) {
      sendCmd(SIM_CMD.RESET)
      readOutputs
    }
  }

  private def update {
    sendCmd(SIM_CMD.UPDATE)
    writeInputs
    readOutputs
    isStale = false
  }

  def step(n: Int) {
    if (isTrace) println("STEP " + n + " -> " + (t + n))
    for (i <- 0 until n) {
      sendCmd(SIM_CMD.STEP)
      writeInputs
      readOutputs
    }
    t += n
    isStale = false
  }

  def int(x: Boolean): BigInt = if (x) 1 else 0
  def int(x: Int):     BigInt = (BigInt(x >>> 1) << 1) | x & 1
  def int(x: Long):    BigInt = (BigInt(x >>> 1) << 1) | x & 1
  def int(x: Bits):    BigInt = x.litValue()

  var ok = true
  var failureTime = -1

  def expect (good: Boolean, msg: String): Boolean = {
    if (isTrace)
      println(msg + " " + (if (good) "PASS" else "FAIL"))
    if (!good) { ok = false; if (failureTime == -1) failureTime = t; }
    good
  }

  def expect (data: Bits, expected: BigInt): Boolean = {
    val mask = (BigInt(1) << data.needWidth) - 1
    val got = peek(data) & mask
    val exp = expected & mask
    expect(got == exp, "EXPECT " + data.getNode.chiselName + " <- " + got.toString(16) + " == " + exp.toString(16))
  }

  def expect (data: Aggregate, expected: Array[BigInt]): Boolean = {
    val kv = (data.flatten.map(x => x._2), expected.reverse).zipped;
    var allGood = true
    for ((d, e) <- kv)
      allGood = expect(d, e) && allGood
    allGood
  }

  /* We need the following so scala doesn't use our "tolerant" Float version of expect.
   */
  def expect (data: Bits, expected: Int): Boolean = {
    expect(data, int(expected))
  }
  def expect (data: Bits, expected: Long): Boolean = {
    expect(data, int(expected))
  }

  /* Compare the floating point value of a node with an expected floating point value.
   * We will tolerate differences in the bottom bit.
   */
  def expect (data: Bits, expected: Float): Boolean = {
    val gotBits = peek(data).toInt
    val expectedBits = java.lang.Float.floatToIntBits(expected)
    var gotFLoat = java.lang.Float.intBitsToFloat(gotBits)
    var expectedFloat = expected
    if (gotFLoat != expectedFloat) {
      val gotDiff = gotBits - expectedBits
      // Do we have a single bit difference?
      if (scala.math.abs(gotDiff) <= 1) {
        expectedFloat = gotFLoat
      }
    }
    expect(gotFLoat == expectedFloat,
       "EXPECT " + data.getNode.chiselName + " <- " + gotFLoat + " == " + expectedFloat)
  }

  val rnd = if (Driver.testerSeedValid) new Random(Driver.testerSeed) else new Random()
  val process: Process = {
    val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName)
    val target = Driver.targetDir + "/" + n
    // If the caller has provided a specific command to execute, use it.
    val cmd = Driver.testCommand match {
      case Some(name: String) => name
      case None => Driver.backend match {
        case b: FloBackend =>
          val command = ArrayBuffer(b.floDir + "fix-console", ":is-debug", "true", ":filename", target + ".hex", ":flo-filename", target + ".mwe.flo")
          if (Driver.isVCD) { command ++= ArrayBuffer(":is-vcd-dump", "true") }
          if (Driver.emitTempNodes) { command ++= ArrayBuffer(":emit-temp-nodes", "true") }
          command ++= ArrayBuffer(":target-dir", Driver.targetDir)
          command.mkString(" ")
        case b: VerilogBackend => target + " -q +vcs+initreg+0 "
        case _ => target
      }
    }
    println("SEED " + Driver.testerSeed)
    println("STARTING " + cmd)
    val processBuilder = Process(cmd)
    val pio = new ProcessIO(
      in => testOut = Option(in), out => testIn = Option(out), err => testErr = Option(err))
    val process = processBuilder.run(pio)
    waitForStreams()
    t = 0
    Driver.backend match {
      case _: VerilogBackend if Driver.isVCD => readln
      case _ =>
    }
    readOutputs
    reset(5)
    process
  }

  def finish {
    sendCmd(SIM_CMD.FIN)
    testIn match { case Some(in) => in.close case None => }
    testErr match { case Some(err) => err.close case None => }
    testOut match { case Some(out) => { out.flush ; out.close } case None => }
    process.destroy()
    println("RAN " + t + " CYCLES " + (if (ok) "PASSED" else "FAILED FIRST AT CYCLE " + failureTime))
    if(!ok) throwException("Module under test FAILED at least one test vector.")
  }
}
