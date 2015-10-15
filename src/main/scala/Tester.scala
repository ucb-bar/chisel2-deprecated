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
import scala.collection.mutable.{ArrayBuffer, HashMap, Queue => ScalaQueue}
import scala.collection.immutable.ListSet
import scala.util.Random
import java.io._
import java.lang.Double.{longBitsToDouble, doubleToLongBits}
import java.lang.Float.{intBitsToFloat, floatToIntBits}
import scala.sys.process.{Process, ProcessIO}

// Provides a template to define tester transactions
trait Tests {
  def t: Int 
  def delta: Int 
  def rnd: Random
  def setClocks(clocks: Iterable[(Clock, Int)]): Unit
  def peek(data: Bits): BigInt
  def peek(data: Aggregate): Array[BigInt]
  def peek(data: Flo): Float
  def peek(data: Dbl): Double
  def peekAt[T <: Bits](data: Mem[T], off: Int): BigInt
  def poke(data: Bits, x: Boolean): Unit
  def poke(data: Bits, x: Int): Unit
  def poke(data: Bits, x: Long): Unit
  def poke(data: Bits, x: BigInt): Unit
  def poke(data: Aggregate, x: Array[BigInt]): Unit
  def poke(data: Flo, x: Float): Unit 
  def poke(data: Dbl, x: Double): Unit
  def pokeAt[T <: Bits](data: Mem[T], value: BigInt, off: Int): Unit
  def reset(n: Int = 1): Unit
  def step(n: Int): Unit
  def int(x: Boolean): BigInt 
  def int(x: Int):     BigInt 
  def int(x: Long):    BigInt 
  def int(x: Bits):    BigInt 
  def expect (good: Boolean, msg: String): Boolean
  def expect (data: Bits, expected: BigInt): Boolean
  def expect (data: Aggregate, expected: Array[BigInt]): Boolean
  def expect (data: Bits, expected: Int): Boolean
  def expect (data: Bits, expected: Long): Boolean
  def expect (data: Flo, expected: Float): Boolean
  def expect (data: Dbl, expected: Double): Boolean
  def accumulatedTestOutput: Array[String]
  def run(s: String): Boolean
}

/** This class is the super class for test cases
  * @param c The module under test
  * @param isTrace print the all I/O operations and tests to stdout, default true
  * @example
  * {{{ class myTest(c : TestModule) extends Tester(c) { ... } }}}
  */
class Tester[+T <: Module](c: T, isTrace: Boolean = true) extends FileSystemUtilities {
  private var _testIn: Option[InputStream] = None
  private var _testErr: Option[InputStream] = None
  private var _testOut: Option[OutputStream] = None
  private lazy val _reader: BufferedReader = new BufferedReader(new InputStreamReader(_testIn.get))
  private lazy val _writer: BufferedWriter = new BufferedWriter(new OutputStreamWriter(_testOut.get))
  private lazy val _logger: BufferedReader = new BufferedReader(new InputStreamReader(_testErr.get))
  var t = 0 // simulation time
  var delta = 0
  private val _pokeMap = HashMap[Bits, BigInt]()
  private val _peekMap = HashMap[Bits, BigInt]()
  private val _signalMap = HashMap[String, Int]()
  private val _clocks = Driver.clocks map (clk => clk -> clk.period.round.toInt)
  private val _clockLens = HashMap(_clocks:_*)
  private val _clockCnts = HashMap(_clocks:_*)
  val (_inputs: ListSet[Bits], _outputs: ListSet[Bits]) = ListSet(c.wires.unzip._2: _*) partition (_.dir == INPUT)
  private var isStale = false
  private val _logs = ArrayBuffer[String]()
  // All the accumulated test output.
  def accumulatedTestOutput = _logs.toArray
  // Return any accumulated module printf output since the last call.
  private var _lastLogIndex = 0
  private def newTestOutputString: String = {
    val result = _logs.slice(_lastLogIndex, _logs.length) mkString("\n")
    _lastLogIndex = _logs.length
    result
  }

  /** Valid commands to send to the Simulator
    * @todo make private? */
  object SIM_CMD extends Enumeration { val RESET, STEP, UPDATE, POKE, PEEK, FORCE, GETID, SETCLK, FIN = Value }
  /**
   * Waits until the emulator streams are ready. This is a dirty hack related
   * to the way Process works. TODO: FIXME.
   */
  def waitForStreams() = {
    var waited = 0
    while (_testIn == None || _testOut == None || _testErr == None) {
      Thread.sleep(100)
      if (waited % 10 == 0 && waited > 30) {
        ChiselError.info("waiting for emulator process streams to be valid ...")
      }
    }
  }

  private def writeln(str: String) {
    _writer write str
    _writer.newLine
    _writer.flush
  }

  private def dumpLogs = {
    while (_logger.ready) {
      _logs += _logger.readLine
    }
  }

  private def readln: String = {
    Option(_reader.readLine) match {
      case None =>
        dumpLogs
        println(newTestOutputString)
        throw new RuntimeException("Errors occurred in simulation")
      case Some(ln) => ln
    }
  }

  private def sendCmd(cmd: SIM_CMD.Value) {
    writeln(cmd.id.toString)
  }

  private def writeValue(v: BigInt) {
    writeln(v.toString(16))
  }

  def dumpName(data: Node): String = Driver.backend match {
    case _: FloBackend => data.getNode.name
    case _ => data.getNode.chiselName
  }

  def setClock(clk: Clock, len: Int) {
    _clockLens(clk) = len
    _clockCnts(clk) = len
    sendCmd(SIM_CMD.SETCLK)
    writeln(clk.name)
    writeValue(len)
  }

  def setClocks(clocks: Iterable[(Clock, Int)]) {
    clocks foreach { case (clk, len) => setClock(clk, len) }
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

  private def peek(id: Int) = {
    sendCmd(SIM_CMD.PEEK)
    writeln(id.toString)
    try { BigInt(readln, 16) } catch { case e: Throwable => BigInt(0) }
  }
  /** Peek at the value of a node based on the path
    */
  def peekPath(path: String) = { 
    peek(_signalMap getOrElseUpdate (path, getId(path)))
  }
  /** Peek at the value of a node
    * @param node Node to peek at
    * @param off The index or offset to inspect */
  def peekNode(node: Node, off: Option[Int] = None) = {
    peekPath(dumpName(node) + ((off map ("[" + _ + "]")) getOrElse ""))
  }
  /** Peek at the value of some memory at an index
    * @param data Memory to inspect
    * @param off Offset in memory to look at */
  def peekAt[T <: Bits](data: Mem[T], off: Int): BigInt = {
    val value = peekNode(data, Some(off))
    if (isTrace) println("  PEEK %s[%d] -> %s".format(dumpName(data), off, value.toString(16)))
    value
  }
  /** Peek at the value of some bits
    * @return a BigInt representation of the bits */
  def peek(data: Bits): BigInt = {
    if (isStale) update
    val value = 
      if (data.isLit) data.litValue()
      else if (data.isTopLevelIO && data.dir == INPUT) _pokeMap(data)
      else signed_fix(data, _peekMap getOrElse (data, peekNode(data.getNode)))
    if (isTrace) println("  PEEK " + dumpName(data) + " -> " + value.toString(16))
    value
  }
  /** Peek at Aggregate data
    * @return an Array of BigInts representing the data */
  def peek(data: Aggregate): Array[BigInt] = {
    data.flatten.map(x => x._2) map (peek(_))
  }
  /** Interpret data as a single precision float */
  def peek(data: Flo): Float = {
    intBitsToFloat(peek(data.asInstanceOf[Bits]).toInt)
  }
  /** Interpret the data as a double precision float */
  def peek(data: Dbl): Double = {
    longBitsToDouble(peek(data.asInstanceOf[Bits]).toLong)
  }

  private def poke(id: Int, v: BigInt, force: Boolean = false) { 
    val cmd = if (!force) SIM_CMD.POKE else SIM_CMD.FORCE
    sendCmd(cmd)
    writeln(id.toString)
    writeValue(v)
  }
  /** set the value of a node with its path
    * @param path The unique path of the node to set
    * @param v The BigInt representing the bits to set
    * @example {{{ poke(path, BigInt(63) << 60, 2) }}}
    */
  def pokePath(path: String, v: BigInt, force: Boolean = false) { 
    poke(_signalMap getOrElseUpdate (path, getId(path)), v, force)
  }
  /** set the value of a node
    * @param node The node to set
    * @param v The BigInt representing the bits to set
    * @param off The offset or index
    */
  def pokeNode(node: Node, v: BigInt, off: Option[Int] = None) {
    pokePath(dumpName(node) + ((off map ("[" + _ + "]")) getOrElse ""), v)
  }
  /** set the value of some memory
    * @param data The memory to write to
    * @param value The BigInt representing the bits to set
    * @param off The offset representing the index to write to memory
    */
  def pokeAt[T <: Bits](data: Mem[T], value: BigInt, off: Int): Unit = {
    if (isTrace) println("  POKE %s[%d] <- %s".format(dumpName(data), off, value.toString(16)))
    pokeNode(data, value, Some(off))
  }
  /** Set the value of some 'data' Node */
  def poke(data: Bits, x: Boolean) { this.poke(data, int(x)) }
  /** Set the value of some 'data' Node */
  def poke(data: Bits, x: Int)     { this.poke(data, int(x)) }
  /** Set the value of some 'data' Node */
  def poke(data: Bits, x: Long)    { this.poke(data, int(x)) }
  /** Set the value of some 'data' Node */
  def poke(data: Bits, x: BigInt)  {
    val value = if (x >= 0) x else {
      val cnt = (data.needWidth() - 1) >> 6
      ((0 to cnt) foldLeft BigInt(0))((res, i) => res | (int((x >> (64 * i)).toLong) << (64 * i)))
    }
    data.getNode match {
      case _: Delay =>
        if (isTrace) println("  POKE " + dumpName(data) + " <- " + value.toString(16))
        pokeNode(data.getNode, value)
        isStale = true
      case _ if data.isTopLevelIO && data.dir == INPUT =>
        if (isTrace) println("  POKE " + dumpName(data) + " <- " + value.toString(16))
        _pokeMap(data) = value
        isStale = true
      case _ =>
        if (isTrace) println("  NOT ALLOWED POKE " + dumpName(data))
    }
  }
  /** Set the value of Aggregate data */
  def poke(data: Aggregate, x: Array[BigInt]): Unit = {
    val kv = (data.flatten.map(x => x._2), x.reverse).zipped
    for ((x, y) <- kv) poke(x, y)
  }
  /** Set the value of a hardware single precision floating point representation */
  def poke(data: Flo, x: Float): Unit = {
    poke(data.asInstanceOf[Bits], BigInt(floatToIntBits(x)))
  }
  /** Set the value of a hardware double precision floating point representation */
  def poke(data: Dbl, x: Double): Unit = {
    poke(data.asInstanceOf[Bits], BigInt(doubleToLongBits(x)))
  }

  private def readOutputs {
    _peekMap.clear
    _outputs foreach (x => _peekMap(x) = try { BigInt(readln, 16) } catch { case e: Throwable => BigInt(0) })
  }

  private def writeInputs {
    _inputs foreach (x => writeValue(_pokeMap getOrElse (x, BigInt(0))))
  }

  /** Send reset to the hardware
    * @param n number of cycles to hold reset for, default 1 */
  def reset(n: Int = 1) {
    if (isTrace) println("RESET " + n)
    for (i <- 0 until n) {
      sendCmd(SIM_CMD.RESET)
      readOutputs
    }
  }

  protected def update {
    sendCmd(SIM_CMD.UPDATE)
    writeInputs
    readOutputs
    isStale = false
  }

  private def calcDelta = {
    val min = (_clockCnts.values foldLeft Int.MaxValue)(math.min(_, _))
    _clockCnts.keys foreach (_clockCnts(_) -= min)
    (_clockCnts filter (_._2 == 0)).keys foreach (k => _clockCnts(k) = _clockLens(k)) 
    min
  }

  protected def takeStep {
    sendCmd(SIM_CMD.STEP)
    writeInputs
    delta += calcDelta
    readOutputs
    dumpLogs
    println(newTestOutputString)
    isStale = false
  }

  protected def getId(path: String) = {
    sendCmd(SIM_CMD.GETID)
    writeln(path)
    readln.toInt
  }

  /** Step time by the smallest amount to the next rising clock edge
    * @note this is defined based on the period of the clock
    * See [[Chisel.Clock$ Clock]]
    */
  def step(n: Int) {
    if (isTrace) println("STEP " + n + " -> " + (t + n))
    (0 until n) foreach (_ => takeStep)
    t += n
  }

  /** Convert a Boolean to BigInt */
  def int(x: Boolean): BigInt = if (x) 1 else 0
  /** Convert an Int to BigInt */
  def int(x: Int):     BigInt = (BigInt(x >>> 1) << 1) | x & 1
  /** Convert a Long to BigInt */
  def int(x: Long):    BigInt = (BigInt(x >>> 1) << 1) | x & 1
  /** Convert Bits to BigInt */
  def int(x: Bits):    BigInt = x.litValue()

  var ok = true
  var failureTime = -1

  /** Expect a value to be true printing a message if it passes or fails
    * @param good If the test passed or not
    * @param msg The message to print out
    */
  def expect (good: Boolean, msg: String): Boolean = {
    if (isTrace)
      println(msg + " " + (if (good) "PASS" else "FAIL"))
    if (!good) { ok = false; if (failureTime == -1) failureTime = t; }
    good
  }

  /** Expect the value of data to have the same bits as a BigInt */
  def expect (data: Bits, expected: BigInt): Boolean = {
    val mask = (BigInt(1) << data.needWidth) - 1
    val got = peek(data) & mask
    val exp = expected & mask
    expect(got == exp, "EXPECT " + dumpName(data) + " <- " + got.toString(16) + " == " + exp.toString(16))
  }

  /** Expect the value of Aggregate data to be have the values as passed in with the array */
  def expect (data: Aggregate, expected: Array[BigInt]): Boolean = {
    val kv = (data.flatten.map(x => x._2), expected.reverse).zipped;
    var allGood = true
    for ((d, e) <- kv)
      allGood = expect(d, e) && allGood
    allGood
  }

  /** Expect the value of 'data' to be 'expected'
    * @return the test passed */
  def expect (data: Bits, expected: Int): Boolean = {
    expect(data, int(expected))
  }
  /** Expect the value of 'data' to be 'expected'
    * @return the test passed */
  def expect (data: Bits, expected: Long): Boolean = {
    expect(data, int(expected))
  }
  /* We need the following so scala doesn't use our "tolerant" Float version of expect.
   */
  /** Expect the value of 'data' to be 'expected'
    * @return the test passed */
  def expect (data: Flo, expected: Float): Boolean = {
    val got = peek(data)
    expect(got == expected, "EXPECT " + dumpName(data) + " <- " + got + " == " + expected)
  }
  /** Expect the value of 'data' to be 'expected'
    * @return the test passed */
  def expect (data: Dbl, expected: Double): Boolean = {
    val got = peek(data)
    expect(got == expected, "EXPECT " + dumpName(data) + " <- " + got + " == " + expected)
  }

  /* Compare the floating point value of a node with an expected floating point value.
   * We will tolerate differences in the bottom bit.
   */
  /** A tolerant expect for Float
    * Allows for a single least significant bit error in the floating point representation */
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

  // Always use a specific seed so results (whenever) are reproducible.
  val rnd = new Random(Driver.testerSeed)
  val process: Process = {
    val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName)
    val target = Driver.targetDir + "/" + n
    // If the caller has provided a specific command to execute, use it.
    val cmd = Driver.testCommand match {
      case Some(cmd) => cmd
      case None => Driver.backend match {
        case b: FloBackend =>
          val command = ArrayBuffer(b.floDir + "fix-console", ":is-debug", "true", ":filename", target + ".hex", ":flo-filename", target + ".mwe.flo")
          if (Driver.isVCD) { command ++= ArrayBuffer(":is-vcd-dump", "true") }
          if (Driver.emitTempNodes) { command ++= ArrayBuffer(":emit-temp-nodes", "true") }
          command ++= ArrayBuffer(":target-dir", Driver.targetDir)
          command.mkString(" ")
        case b: VerilogBackend => List(target, "-q", "+vcs+initreg+0", 
          if (Driver.isVCD) "+vpdfile=%s.vpd".format(Driver.targetDir + c.name)  else "",
          if (Driver.isVCDMem) "+vpdmem" else "") mkString " "
        case _ => target
      }
    }
    println("SEED " + Driver.testerSeed)
    println("STARTING " + cmd)
    val processBuilder = Process(cmd)
    val pio = new ProcessIO(
      in => _testOut = Option(in), out => _testErr = Option(out), err => _testIn = Option(err))
    val process = processBuilder.run(pio)
    waitForStreams()
    t = 0
    readOutputs
    // reset(5)
    for (i <- 0 until 5) {
      sendCmd(SIM_CMD.RESET)
      readOutputs
    }
    while (_logger.ready) println(_logger.readLine)
    process
  }

  /** Complete the simulation and inspect all tests */
  def finish {
    sendCmd(SIM_CMD.FIN)
    _testIn match { case Some(in) => in.close case None => }
    _testErr match { case Some(err) => err.close case None => }
    _testOut match { case Some(out) => { out.flush ; out.close } case None => }
    process.destroy()
    println("RAN " + t + " CYCLES " + (if (ok) "PASSED" else "FAILED FIRST AT CYCLE " + failureTime))
    if(!ok) throwException("Module under test FAILED at least one test vector.")
  }

  _signalMap ++= Driver.signalMap flatMap {
    case (m: Mem[_], id) => 
      (0 until m.n) map (idx => "%s[%d]".format(dumpName(m), idx) -> (id + idx))
    case (node, id) => Seq(dumpName(node) -> id)
  }
}

/** A tester to check a node graph from INPUTs to OUTPUTs directly */
class MapTester[+T <: Module](c: T, val testNodes: Seq[Node]) extends Tester(c, false) {
  val (ins, outs) = testNodes partition { case b: Bits => b.dir == INPUT case _ => false }
  def step(svars: HashMap[Node, Node],
           ovars: HashMap[Node, Node] = HashMap.empty,
           isTrace: Boolean = true): Boolean = {
    if (isTrace) println("---\nINPUTS")
    ins foreach { in =>
      val value = (svars get in) match { case None => BigInt(0) case Some(v) => v.litValue() }
      in match {
        case io: Bits if io.isTopLevelIO => poke(io, value)
        case _ => pokeNode(in, value)
      }
      if (isTrace) println("  WRITE " + dumpName(in) + " = " + value)
    }
    step(1)
    if (isTrace) println("OUTPUTS")
    outs forall { out =>
      val value = out match { 
        case io: Bits if io.isTopLevelIO => peek(io)
        case _ => peekNode(out)
      }
      (ovars get out) match {
        case None => 
          ovars(out) = Literal(value)
          if (isTrace) println("  READ " + dumpName(out) + " = " + value)
          true
        case Some(e) =>
          val expected = e.litValue()
          val pass = expected == value
          if (isTrace) println("  EXPECTED %s: %x == %x -> %s".format(value, expected, if (pass) "PASS" else "FAIL"))
          pass
      }
    }
  }
  var tests: () => Boolean = () => { println("DEFAULT TESTS"); true }
  def defTests(body: => Boolean) = body
}
