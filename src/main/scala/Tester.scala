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
  var t = 0 // simulation time
  var delta = 0
  var testOutputString = "" // output available to test code.
  val sb = new StringBuilder()
  val pokeMap = HashMap[Bits, BigInt]()
  val peekMap = HashMap[Bits, BigInt]()
  val (inputs: ListSet[Bits], outputs: ListSet[Bits]) = ListSet(c.wires.unzip._2: _*) partition (_.dir == INPUT)

  object SIM_CMD extends Enumeration { val RESET, STEP, FIN = Value }
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
    ChiselError.info("emulator launched after %d trials".format(waited))
  }

  /**
   * Sends a command to the emulator and returns the reply.
   * The standard protocol treats a single line as a command, which always
   * returns a single line of reply.
   */
  // TODO: deprecated
  def emulatorCmd(str: String): String = {
    // validate cmd
    if (str contains "\n") {
      System.err.print(s"emulatorCmd($str): command should not contain newline")
      return "error"
    }

    waitForStreams()

    // send command to emulator
    for (e <- str) testOut.get.write(e);
    testOut.get.write('\n');
    testOut.get.flush()

    // read output from emulator
    var c = testIn.get.read
    sb.clear()
    while (c != '\n' && c != -1) {
      if (c == 0) Thread.sleep(100)
      sb += c.toChar
      // Look for a "PRINT" command.
      if (sb.length == 6 && sb.startsWith("PRINT ")) {
        do {
          c = testIn.get.read
          sb += c.toChar
        } while (c != ' ')
        // Get the PRINT character count.
        val printCommand = """^PRINT (\d+) """.r
        val printCommand(nChars) = sb.toString
        sb.clear()
        for (i <- 0 until nChars.toInt) {
          c = testIn.get.read
          sb += c.toChar
        }
        // Put any generated output somewhere we can access it.
        testOutputString = sb.toString()
        sb.clear()
        System.out.print(testOutputString)
      }
      c = testIn.get.read
    }

    // drain errors
    try {
      while(testErr.get.available() > 0) {
        System.err.print(Character.toChars(testErr.get.read()))
      }
    } catch {
      case e : IOException => testErr = null; println("ERR EXCEPTION")
    }

    if (sb == "error") {
      System.err.print(s"FAILED: emulatorCmd($str): returned error")
      ok = false
    }
    return sb.toString
  }

  def setClocks(clocks: HashMap[Clock, Int]) {
    var cmd = "set_clocks"
    for (clock <- Driver.clocks) {
      if (clock.srcClock == null) {
        val s = BigInt(clocks(clock)).toString(16)
        cmd = cmd + " " + s
      }
    }
    emulatorCmd(cmd)
    // TODO: check for errors in return
  }

  def dumpName(data: Node): String = Driver.backend match {
    case _: FloBackend => data.name
    case _ => data.chiselName
  }

  def peekBits(data: Node, off: Int = -1): BigInt = {
    if (dumpName(data) == "") {
      println("Unable to peek data " + data)
      -1
    } else {
      var cmd = ""
      if (off != -1) {
        cmd = "mem_peek " + dumpName(data) + " " + off;
      } else {
        cmd = "wire_peek " + dumpName(data);
      }
      val s = emulatorCmd(cmd)
      val rv = Literal.toLitVal(s)
      if (isTrace) println("  PEEK " + dumpName(data) + " " + (if (off >= 0) (off + " ") else "") + "-> " + s)
      rv
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

  def peekAt[T <: Bits](data: Mem[T], off: Int): BigInt = {
    // signed_fix(data(1), peekBits(data, off))
    peekBits(data, off)
  }

  def peek(data: Bits): BigInt = {
    val x = signed_fix(data, peekMap getOrElse (data, peekBits(data.getNode)))
    if (isTrace) println("  PEEK " + dumpName(data) + " -> " + x)
    x 
  }

  def peek(data: Aggregate /*, off: Int = -1 */): Array[BigInt] = {
    data.flatten.map(x => x._2).map(peek(_))
  }

  def doPokeBits(data: Node, x: BigInt, off: Int = -1): Unit = {
    if (dumpName(data) == "") {
      println("Unable to poke data " + data)
    } else {
      var cmd = ""
      if (off != -1) {
        cmd = "mem_poke " + dumpName(data) + " " + off;
      } else {
        cmd = "wire_poke " + dumpName(data);
      }
      // Don't prefix negative numbers with "0x"
      val radixPrefix = if (x < 0) " -0x" else " 0x"
      val xval = radixPrefix + x.abs.toString(16)
      cmd = cmd + xval
      if (isTrace) {
        println("  POKE " + dumpName(data) + " " + (if (off >= 0) (off + " ") else "") + "<- " + xval)
      }
      val rtn = emulatorCmd(cmd)
      if (rtn != "ok") {
        System.err.print(s"FAILED: poke(${dumpName(data)}) returned false")
        ok = false
      }
    }
  }

  def pokeBits(data: Node, x: BigInt, off: Int = -1): Unit = {
    doPokeBits(data, x, off)
  }

  def pokeAt[T <: Bits](data: Mem[T], x: BigInt, off: Int): Unit = {
    pokeBits(data, x, off)
  }

  def poke(data: Bits, x: BigInt): Unit = {
    if (isTrace) println("  POKE " + dumpName(data) + " -> " + x)
    if (inputs contains data) 
      pokeMap(data) = x
    else 
      pokeBits(data.getNode, x)
  }

  def poke(data: Aggregate, x: Array[BigInt]): Unit = {
    val kv = (data.flatten.map(x => x._2), x.reverse).zipped;
    for ((x, y) <- kv)
      poke(x, y)
  }

  private def writeln(str: String) {
    writer write str
    writer.newLine
    writer.flush
  }

  private def readln: String = {
    reader.readLine
  }

  private def sendCmd(cmd: SIM_CMD.Value) {
    writeln(cmd.id.toString)
  }

  private def readOutputs {
    peekMap.clear
    outputs foreach (x => peekMap(x) = BigInt(readln, 16))
  }
 
  private def writeInputs {
    inputs foreach ( x => 
      writeln((pokeMap getOrElse (x, BigInt(0))).toString(16)) )
  }

  def reset(n: Int = 1) {
    if (isTrace) println("RESET " + n)
    for (i <- 0 until n) {
      sendCmd(SIM_CMD.RESET)
      readOutputs
    }
  }


  def step(n: Int) = {
    val target = t + n
    // val s = emulatorCmd("step " + n)
    // delta += s.toInt
    if (isTrace) println("STEP " + n + " -> " + target)
    for (i <- 0 until n) {
      sendCmd(SIM_CMD.STEP)
      writeInputs
      readOutputs
    }
    t += n
  }

  def int(x: Boolean): BigInt = if (x) 1 else 0
  def int(x: Int): BigInt = x
  def int(x: Bits): BigInt = x.litValue()

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
    val got = peek(data)

    expect((got & mask) == (expected & mask),
       "EXPECT " + dumpName(data) + " <- " + got + " == " + expected)
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
    expect(data, BigInt(expected))
  }
  def expect (data: Bits, expected: Long): Boolean = {
    expect(data, BigInt(expected))
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
       "EXPECT " + dumpName(data) + " <- " + gotFLoat + " == " + expectedFloat)
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
        case _ => ""
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
