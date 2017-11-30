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

package Chisel
import scala.collection.mutable.{ArrayBuffer, HashSet, HashMap, Stack, Queue => ScalaQueue}
import scala.collection.immutable.ListSet
import scala.util.Random
import java.nio.channels.FileChannel
import java.lang.Double.{longBitsToDouble, doubleToLongBits}
import java.lang.Float.{intBitsToFloat, floatToIntBits}
import scala.sys.process.{Process, ProcessLogger}
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

// Provides a template to define tester transactions
trait Tests {
  def t: Long
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
  def expect (good: Boolean, msg: => String): Boolean
  def expect (data: Bits, expected: BigInt): Boolean
  def expect (data: Aggregate, expected: Array[BigInt]): Boolean
  def expect (data: Bits, expected: Int): Boolean
  def expect (data: Bits, expected: Long): Boolean
  def expect (data: Bool, expected: Boolean): Boolean
  def expect (data: Flo, expected: Float): Boolean
  def expect (data: Dbl, expected: Double): Boolean
  def newTestOutputString: String
  def expect (data: Bits, expected: BigInt, msg: => String): Boolean
  def expect (data: Bits, expected: Int, msg: => String): Boolean
  def expect (data: Bits, expected: Long, msg: => String): Boolean
  def expect (data: Bool, expected: Boolean, msg: => String): Boolean
  def expect (data: Flo, expected: Float, msg: => String): Boolean
  def expect (data: Dbl, expected: Double, msg: => String): Boolean
  def printfs: Vector[String]
  def run(s: String): Boolean
}

case class TestApplicationException(exitVal: Int, lastMessage: String) extends RuntimeException(lastMessage)

object Tester {
  private[Chisel] val processes = HashSet[Process]()
  implicit def strToOption(s: String) = if (s.isEmpty) None else Option(s)
  def close {
    processes foreach (_.destroy)
    processes.clear
  }
}

/** This class is the super class for test cases
  * @param c The module under test
  * @param isTrace print the all I/O operations and tests to stdout, default true
  * @param _base base for prints, default 16 (hex)
  * @param testCmd command to run the emulator
  * @param dumpFile vcd/vpd file name
  * @example
  * {{{ class myTest(c : TestModule) extends Tester(c) { ... } }}}
  */
class Tester[+T <: Module](c: T, isTrace: Boolean = true, _base: Int = 16,
    testCmd: Option[String] = Driver.testCommand,
    dumpFile: Option[String] = None) extends FileSystemUtilities {
  // Define events
  abstract class Event
  case class StartEvent(seed: Long, cmd: String) extends Event
  case class FinishEvent(t: Long, pass: Boolean, fail_t: Long) extends Event
  case class MuteEvent() extends Event
  case class UnmuteEvent() extends Event
  case class ResetEvent(n: Int) extends Event
  case class StepEvent(n: Int, t: Long) extends Event
  case class PokeEvent(b: Bits, v: BigInt, good: Boolean = true) extends Event
  case class PokeMemEvent[T <: Data](m: Mem[T], off: Int, v: BigInt) extends Event
  case class PokeFloEvent(b: Flo, v: Float) extends Event
  case class PokeDblEvent(b: Dbl, v: Double) extends Event
  case class PeekEvent(b: Bits, v: Option[BigInt]) extends Event
  case class PeekMemEvent[T <: Data](m: Mem[T], off: Int, v: BigInt) extends Event
  case class PeekFloEvent(b: Flo, v: Float) extends Event
  case class PeekDblEvent(b: Dbl, v: Double) extends Event
  case class ExpectMsgEvent(good: Boolean, msg: String) extends Event
  case class ExpectEvent(b: Bits, got: BigInt, expected: BigInt, msg: String) extends Event
  case class ExpectFloEvent(b: Bits, got: Float, expected: Float, msg: String) extends Event
  case class ExpectDblEvent(b: Dbl, got: Double, expected: Double, msg: String) extends Event
  case class DumpEvent(msg: String) extends Event
  case class NoIdEvent(path: String) extends Event
  // Define observer
  class Observer(base: Int = _base, file: java.io.PrintStream = System.out) {
    private var lock = false
    protected def convt(x: BigInt) = base match {
      case 2  if x < 0 => s"-0b${(-x).toString(base)}"
      case 16 if x < 0 => s"-0x${(-x).toString(base)}"
      case 2  => s"0b${x.toString(base)}"
      case 16 => s"0x${x.toString(base)}"
      case _ => x.toString(base)
    }
    def locked = lock
    def apply(event: Event): Unit = event match {
      case StartEvent(seed, cmd) =>
        file.println(s"SEED ${seed}")
        file.println(s"STARTING ${cmd}")
      case FinishEvent(t, pass, fail_t) =>
        file.println(s"""RAN ${t} CYCLES ${if (pass) "PASSED" else s"FAILED FIRST AT CYCLE ${fail_t}"}""")
      case MuteEvent() => lock = true
      case UnmuteEvent() => lock = false
      case StepEvent(n, t) if !locked => file.println(s"STEP ${n} -> ${t+n}")
      case ResetEvent(n) if !locked => file.println(s"RESET ${n}")
      case PokeEvent(b, v, good) if !locked =>
        val value = if (good) convt(v) else "NOT ALLOWED"
        file.println(s"  POKE ${dumpName(b)} <- ${value}")
      case PokeMemEvent(m, off, v) if !locked =>
        file.println(s"  POKE ${dumpName(m)}[${off}] <- ${convt(v)}")
      case PokeFloEvent(b, v) if !locked =>
        file.println(s"  POKE ${dumpName(b)} <- ${v}")
      case PokeDblEvent(b, v) if !locked =>
        file.println(s"  POKE ${dumpName(b)} <- ${v}")
      case PeekEvent(b, None) if !locked =>
        file.println(s"  PEEK ${dumpName(b)} -> No initial values")
      case PeekEvent(b, Some(v)) if !locked =>
        file.println(s"  PEEK ${dumpName(b)} <- ${convt(v)}")
      case PeekMemEvent(m, off, v) if !locked =>
        file.println(s"  PEEK ${dumpName(m)}[${off}] -> ${convt(v)}")
      case PeekFloEvent(b, v) if !locked =>
        file.println(s"  PEEK ${dumpName(b)} -> ${v}")
      case PeekDblEvent(b, v) if !locked =>
        file.println(s"  PEEK ${dumpName(b)} -> ${v}")
      case ExpectMsgEvent(good, msg) if !locked =>
        file.println(s"""${msg} ${if (good) "PASS" else "FAIL"}""")
      case ExpectEvent(b, got, exp, msg) if !locked => apply(new ExpectMsgEvent(got == exp,
        s"${msg}  EXPECT ${dumpName(b)} -> ${convt(got)} == ${convt(exp)}"))
      case ExpectFloEvent(b, got, exp, msg) if !locked => apply(new ExpectMsgEvent(got == exp,
        s"${msg}  EXPECT ${dumpName(b)} -> ${got} == ${exp}"))
      case ExpectDblEvent(b, got, exp, msg) if !locked => apply(new ExpectMsgEvent(got == exp,
        s"${msg}  EXPECT ${dumpName(b)} -> ${got} == ${exp}"))
      case DumpEvent(msg) if !msg.isEmpty =>
        file.println(msg)
      case NoIdEvent(path) => file.println(s"Can't find id for '${path}'")
      case _ => // silent
    }
  }
  class BasicObserver extends Observer // defulat: prints hex to the screen
  private val observers = ArrayBuffer[Observer]()
  def addObserver(o: Observer) { observers += o }
  def addEvent(e: Event) { observers foreach (_(e)) }
  if (isTrace) addObserver(new BasicObserver)

  implicit def longToInt(x: Long) = x.toInt
  def incTime(n: Int) { _t += n }
  def t = _t
  var _t = 0L // simulation time
  var delta = 0
  private val _pokeMap = HashMap[Bits, BigInt]()
  private val _peekMap = HashMap[Bits, BigInt]()
  private val _signalMap = HashMap[String, Int]()
  private val _chunks = HashMap[String, Int]()
  private val _clocks = Driver.clocks map (clk => clk -> clk.period.round.toInt)
  private val _clockLens = HashMap(_clocks:_*)
  private val _clockCnts = HashMap(_clocks:_*)
  val (_inputs: ListSet[Bits], _outputs: ListSet[Bits]) = ListSet(c.wires.unzip._2: _*) partition (_.dir == INPUT)
  private var isStale = false
  // Return any accumulated module printf output since the last call.
  private var _lastLogIndex = 0
  def newTestOutputString: String = {
    val result = _logs.slice(_lastLogIndex, _logs.length) mkString("\n")
    _lastLogIndex = _logs.length
    result
  }
  private val _logs = new ArrayBuffer[String]()
  def printfs = _logs.toVector

  def throwExceptionIfDead(exitValue: Future[Int]) {
    if (exitValue.isCompleted) {
      val exitCode = Await.result(exitValue, Duration(-1, SECONDS))
      // We assume the error string is the last log entry.
      val errorString = if (_logs.size > 0) {
         _logs.last
      } else {
        "test application exit"
      } + " - exit code %d".format(exitCode)
      addEvent(new DumpEvent(newTestOutputString))
      throw new TestApplicationException(exitCode, errorString)
    }
  }

  // A busy-wait loop that monitors exitValue so we don't loop forever if the test application exits for some reason.
  private def mwhile(block: => Boolean)(loop: => Unit) {
    while (!exitValue.isCompleted && block) {
      loop
    }
    // If the test application died, throw a run-time error.
    throwExceptionIfDead(exitValue)
  }
  private object SIM_CMD extends Enumeration {
    val RESET, STEP, UPDATE, POKE, PEEK, FORCE, GETID, GETCHK, SETCLK, FIN = Value }
  implicit def cmdToId(cmd: SIM_CMD.Value) = cmd.id

  private class Channel(name: String) {
    private lazy val file = new java.io.RandomAccessFile(name, "rw")
    private lazy val channel = file.getChannel
    @volatile private lazy val buffer = {
      /* We have seen runs where buffer.put(0,0) fails with:
[info]   java.lang.IndexOutOfBoundsException:
[info]   at java.nio.Buffer.checkIndex(Buffer.java:532)
[info]   at java.nio.DirectByteBuffer.put(DirectByteBuffer.java:300)
[info]   at Chisel.Tester$Channel.release(Tester.scala:148)
[info]   at Chisel.Tester.start(Tester.scala:717)
[info]   at Chisel.Tester.<init>(Tester.scala:743)
[info]   at ArbiterSuite$ArbiterTests$8.<init>(ArbiterTest.scala:396)
[info]   at ArbiterSuite$$anonfun$testStableRRArbiter$1.apply(ArbiterTest.scala:440)
[info]   at ArbiterSuite$$anonfun$testStableRRArbiter$1.apply(ArbiterTest.scala:440)
[info]   at Chisel.Driver$.apply(Driver.scala:65)
[info]   at Chisel.chiselMain$.apply(hcl.scala:63)
[info]   ...
       */
      val size = channel.size
      assert(size > 16, "channel.size is bogus: %d".format(size))
      channel map (FileChannel.MapMode.READ_WRITE, 0, size)
    }
    implicit def intToByte(i: Int) = i.toByte
    val channel_data_offset_64bw = 4    // Offset from start of channel buffer to actual user data in 64bit words.
    def aquire {
      buffer put (0, 1)
      buffer put (2, 0)
      while((buffer get 1) == 1 && (buffer get 2) == 0) {}
    }
    def release { buffer put (0, 0) }
    def ready = (buffer get 3) == 0
    def valid = (buffer get 3) == 1
    def produce { buffer put (3, 1) }
    def consume { buffer put (3, 0) }
    def update(idx: Int, data: Long) { buffer putLong (8 * idx + channel_data_offset_64bw, data) }
    def update(base: Int, data: String) {
      data.zipWithIndex foreach {case (c, i) => buffer put (base + i + channel_data_offset_64bw, c) }
      buffer put (base + data.size + channel_data_offset_64bw, 0)
    }
    def apply(idx: Int): Long = buffer getLong (8 * idx + channel_data_offset_64bw)
    def close { file.close }
    buffer order java.nio.ByteOrder.nativeOrder
    new java.io.File(name).delete
  }

  def dumpName(data: Node): String = Driver.backend match {
    case _: FloBackend => data.getNode.name
    case _ => data.getNode.chiselName
  }

  def setClock(clk: Clock, len: Int) {
    _clockLens(clk) = len
    _clockCnts(clk) = len
    mwhile(!sendCmd(SIM_CMD.SETCLK)) { }
    mwhile(!sendCmd(clk.name)) { }
    mwhile(!sendValue(len, 1)) { }
  }

  def setClocks(clocks: Iterable[(Clock, Int)]) {
    clocks foreach { case (clk, len) => setClock(clk, len) }
  }

  def signed_fix(dtype: Bits, rv: BigInt): BigInt = {
    val w = dtype.needWidth()
    dtype match {
      /* Any "signed" node */
      case _: SInt | _ : Flo | _: Dbl | _: Fixed => (if(rv >= (BigInt(1) << w - 1)) (rv - (BigInt(1) << w)) else rv)
      /* anything else (i.e., UInt) */
      case _ => (rv)
    }
  }

  private def peek(id: Int, chunk: Int) = {
    mwhile(!sendCmd(SIM_CMD.PEEK)) { }
    mwhile(!sendCmd(id)) { }
    if (exitValue.isCompleted) {
      BigInt(0)
    } else {
      (for {
        _ <- Stream.from(1)
        data = recvValue(chunk)
        if data != None
      } yield data.get).head
    }
  }
  /** Peek at the value of a node based on the path
    */
  def peekPath(path: String): BigInt = {
    val id = _signalMap getOrElseUpdate (path, getId(path))
    if (id == -1) {
      addEvent(new NoIdEvent(path))
      id
    } else {
      peek(id, _chunks getOrElseUpdate (path, getChunk(id)))
    }
  }
  /** Peek at the value of a node
    * @param node Node to peek at
    * @param off The index or offset to inspect */
  def peekNode(node: Node, off: Option[Int] = None) = {
    val i = off match { case Some(p) => s"[${p}]" case None => "" }
    peekPath(s"${dumpName(node)}${i}")
  }
  /** Peek at the value of some memory at an index
    * @param data Memory to inspect
    * @param off Offset in memory to look at */
  def peekAt[T <: Bits](data: Mem[T], off: Int): BigInt = {
    val value = peekNode(data, Some(off))
    addEvent(new PeekMemEvent(data, off, value))
    value
  }
  /** Peek at the value of some bits
    * @return a BigInt representation of the bits */
  def peek(data: Bits): BigInt = {
    if (isStale) update
    val value = if (data.isLit) Some(data.litValue())
      else if (data.isTopLevelIO && data.dir == INPUT) _pokeMap get data
      else Some(signed_fix(data, _peekMap getOrElse (data, peekNode(data.getNode))))
    addEvent(new PeekEvent(data, value))
    value getOrElse BigInt(rnd.nextInt)
  }
  /** Peek at Aggregate data
    * @return an Array of BigInts representing the data */
  def peek(data: Aggregate): Array[BigInt] = {
    data.flatten map (x => peek(x._2))
  }
  /** Interpret data as a single precision float */
  def peek(data: Flo): Float = {
    addEvent(new MuteEvent())
    val value = intBitsToFloat(peek(data.asInstanceOf[Bits]).toInt)
    addEvent(new UnmuteEvent())
    addEvent(new PeekFloEvent(data, value))
    value
  }
  /** Interpret the data as a double precision float */
  def peek(data: Dbl): Double = {
    addEvent(new MuteEvent())
    val value = longBitsToDouble(peek(data.asInstanceOf[Bits]).toLong)
    addEvent(new UnmuteEvent())
    addEvent(new PeekDblEvent(data, value))
    value
  }

  private def poke(id: Int, chunk: Int, v: BigInt, force: Boolean = false) {
    val cmd = if (!force) SIM_CMD.POKE else SIM_CMD.FORCE
    mwhile(!sendCmd(cmd)) { }
    mwhile(!sendCmd(id)) { }
    mwhile(!sendValue(v, chunk)) { }
  }
  /** set the value of a node with its path
    * @param path The unique path of the node to set
    * @param v The BigInt representing the bits to set
    * @example {{{ poke(path, BigInt(63) << 60, 2) }}}
    */
  def pokePath(path: String, v: BigInt, force: Boolean = false) {
    val id = _signalMap getOrElseUpdate (path, getId(path))
    if (id == -1) {
      addEvent(new NoIdEvent(path))
    } else {
      poke(id, _chunks getOrElseUpdate (path, getChunk(id)), v, force)
    }
  }
  /** set the value of a node
    * @param node The node to set
    * @param v The BigInt representing the bits to set
    * @param off The offset or index
    */
  def pokeNode(node: Node, v: BigInt, off: Option[Int] = None, force: Boolean = false) {
    val i = off match { case Some(p) => s"[${p}]" case None => "" }
    pokePath(s"${dumpName(node)}${i}", v, force)
  }
  /** set the value of some memory
    * @param data The memory to write to
    * @param value The BigInt representing the bits to set
    * @param off The offset representing the index to write to memory
    */
  def pokeAt[T <: Bits](data: Mem[T], value: BigInt, off: Int): Unit = {
    addEvent(new PokeMemEvent(data, off, value))
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
        addEvent(new PokeEvent(data, value))
        pokeNode(data.getNode, value)
        isStale = true
      case _ if data.isTopLevelIO && data.dir == INPUT =>
        addEvent(new PokeEvent(data, value))
        _pokeMap(data) = value
        isStale = true
      case _ =>
        addEvent(new PokeEvent(data, value, false))
    }
  }
  /** Set the value of Aggregate data */
  def poke(data: Aggregate, x: Array[BigInt]): Unit = {
    val kv = (data.flatten.map(x => x._2), x.reverse).zipped
    for ((x, y) <- kv) poke(x, y)
  }
  /** Set the value of a hardware single precision floating point representation */
  def poke(data: Flo, x: Float): Unit = {
    addEvent(new PokeFloEvent(data, x))
    addEvent(new MuteEvent())
    poke(data.asInstanceOf[Bits], BigInt(floatToIntBits(x)))
    addEvent(new UnmuteEvent())
  }
  /** Set the value of a hardware double precision floating point representation */
  def poke(data: Dbl, x: Double): Unit = {
    addEvent(new PokeDblEvent(data, x))
    addEvent(new MuteEvent())
    poke(data.asInstanceOf[Bits], BigInt(doubleToLongBits(x)))
    addEvent(new UnmuteEvent())
  }

  private def sendCmd(data: Int) = {
    cmdChannel.aquire
    val ready = cmdChannel.ready
    if (ready) {
      cmdChannel(0) = data
      cmdChannel.produce
    }
    cmdChannel.release
    ready
  }

  private def sendCmd(data: String) = {
    cmdChannel.aquire
    val ready = cmdChannel.ready
    if (ready) {
      cmdChannel(0) = data
      cmdChannel.produce
    }
    cmdChannel.release
    ready
  }

  private def recvResp = {
    outChannel.aquire
    val valid = outChannel.valid
    val resp = if (!valid) None else {
      outChannel.consume
      Some(outChannel(0).toInt)
    }
    outChannel.release
    resp
  }

  private def sendValue(value: BigInt, chunk: Int) = {
    inChannel.aquire
    val ready = inChannel.ready
    if (ready) {
      (0 until chunk) foreach (i => inChannel(i) = (value >> (64*i)).toLong)
      inChannel.produce
    }
    inChannel.release
    ready
  }

  private def recvValue(chunk: Int) = {
    outChannel.aquire
    val valid = outChannel.valid
    val value = if (!valid) None else {
      outChannel.consume
      Some(((0 until chunk) foldLeft BigInt(0))(
        (res, i) => res | (int(outChannel(i)) << (64*i))))
    }
    outChannel.release
    value
  }

  private def sendInputs = {
    inChannel.aquire
    val ready = inChannel.ready
    if (ready) {
      (_inputs.toList foldLeft 0){case (off, in) =>
        val chunk = _chunks(dumpName(in))
        val value = _pokeMap getOrElse (in, BigInt(0))
        (0 until chunk) foreach (i => inChannel(off + i) = (value >> (64 * i)).toLong)
        off + chunk
      }
      inChannel.produce
    }
    inChannel.release
    ready
  }

  private def recvOutputs = {
    _peekMap.clear
    outChannel.aquire
    val valid = outChannel.valid
    if (valid) {
      (_outputs.toList foldLeft 0){case (off, out) =>
        val chunk = _chunks(dumpName(out))
        _peekMap(out) = ((0 until chunk) foldLeft BigInt(0))(
          (res, i) => res | (int(outChannel(off + i)) << (64 * i)))
        off + chunk
      }
      outChannel.consume
    }
    outChannel.release
    valid
  }

  /** Send reset to the hardware
    * @param n number of cycles to hold reset for, default 1 */
  def reset(n: Int = 1) {
    addEvent(new ResetEvent(n))
    for (i <- 0 until n) {
      mwhile(!sendCmd(SIM_CMD.RESET)) { }
      mwhile(!recvOutputs) { }
    }
  }

  protected def update {
    mwhile(!sendCmd(SIM_CMD.UPDATE)) { }
    mwhile(!sendInputs) { }
    mwhile(!recvOutputs) { }
    isStale = false
  }

  private def calcDelta = {
    val min = (_clockCnts.values foldLeft Int.MaxValue)(math.min(_, _))
    _clockCnts.keys foreach (_clockCnts(_) -= min)
    (_clockCnts filter (_._2 == 0)).keys foreach (k => _clockCnts(k) = _clockLens(k))
    min
  }

  protected def takeStep {
    mwhile(!sendCmd(SIM_CMD.STEP)) { }
    mwhile(!sendInputs) { }
    delta += calcDelta
    mwhile(!recvOutputs) { }
    // dumpLogs
    observers map (_(new DumpEvent(newTestOutputString)))
    isStale = false
  }

  protected def getId(path: String) = {
    mwhile(!sendCmd(SIM_CMD.GETID)) { }
    mwhile(!sendCmd(path)) { }
    if (exitValue.isCompleted) {
      0
    } else {
      (for {
        _ <- Stream.from(1)
        data = recvResp
        if data != None
      } yield data.get).head
    }
  }

  protected def getChunk(id: Int) = {
    mwhile(!sendCmd(SIM_CMD.GETCHK)) { }
    mwhile(!sendCmd(id)) { }
    if (exitValue.isCompleted){
      0
    } else {
      (for {
        _ <- Stream.from(1)
        data = recvResp
        if data != None
      } yield data.get).head
    }
  }

  /** Step time by the smallest amount to the next rising clock edge
    * @note this is defined based on the period of the clock
    * See [[Chisel.Clock$ Clock]]
    */
  def step(n: Int) {
    addEvent(new StepEvent(n, t))
    (0 until n) foreach (_ => takeStep)
    incTime(n)
  }

  /** Convert a Boolean to BigInt */
  def int(x: Boolean): BigInt = if (x) 1 else 0
  /** Convert an Int to BigInt */
  def int(x: Int):     BigInt = (BigInt(x >>> 1) << 1) | x & 1
  /** Convert a Long to BigInt */
  def int(x: Long):    BigInt = (BigInt(x >>> 1) << 1) | x & 1
  /** Convert Bits to BigInt */
  def int(x: Bits):    BigInt = x.litValue()

  /** Indicate a failure has occurred.  */
  private var failureTime = -1L
  private var ok = true
  def fail = if (ok) {
    failureTime = t
    ok = false
  }

  /** Expect a value to be true printing a message if it passes or fails
    * @param good If the test passed or not
    * @param msg The message to print out
    */
  def expect (good: Boolean, msg: => String): Boolean = {
    addEvent(new ExpectMsgEvent(good, msg))
    if (!good) fail
    good
  }

  /** Expect the value of data to have the same bits as a BigInt */
  def expect (data: Bits, expected: BigInt, msg: => String): Boolean = {
    addEvent(new MuteEvent())
    val mask = (BigInt(1) << data.needWidth) - 1
    val got = peek(data) & mask
    val exp = expected & mask
    val good = got == exp
    addEvent(new UnmuteEvent())
    addEvent(new ExpectEvent(data, got, exp, msg))
    if (!good) fail
    good
  }
  def expect (data: Bits, expected: BigInt): Boolean = {
    expect(data, expected, "")
  }

  /** Expect the value of Aggregate data to be have the values as passed in with the array */
  def expect (data: Aggregate, expected: Array[BigInt]): Boolean = {
    val kv = (data.flatten.map(x => x._2), expected.reverse).zipped
    kv forall {case (d, e) => expect(d, e)}
  }

  /** Expect the value of 'data' to be 'expected'
    * @return the test passed */
  def expect (data: Bits, expected: Int, msg: => String): Boolean = {
    expect(data, int(expected), msg)
  }
  def expect (data: Bits, expected: Int): Boolean = {
    expect(data, expected, "")
  }

  /** Expect the value of 'data' to be 'expected'
    * @return the test passed */
  def expect (data: Bits, expected: Long, msg: => String): Boolean = {
    expect(data, int(expected), msg)
  }
  def expect (data: Bits, expected: Long): Boolean = {
    expect(data, expected, "")
  }

  /** Expect the value of 'data' to be 'expected'
    * @return the test passed */
  def expect (data: Bool, expected: Boolean, msg: => String): Boolean = {
    expect(data, { if (expected) 1 else 0 }, msg)
  }
  def expect (data: Bool, expected: Boolean): Boolean = {
    expect(data, expected, "")
  }

  /* We need the following so scala doesn't use our "tolerant" Float version of expect.
   */
  /** Expect the value of 'data' to be 'expected'
    * @return the test passed */
  def expect (data: Flo, expected: Float, msg: => String): Boolean = {
    addEvent(new MuteEvent())
    val got = peek(data)
    val good = got == expected
    addEvent(new UnmuteEvent())
    addEvent(new ExpectFloEvent(data, got, expected, msg))
    if (!good) fail
    good
  }
  def expect (data: Flo, expected: Float): Boolean = {
    expect(data, expected, "")
  }

  /** Expect the value of 'data' to be 'expected'
    * @return the test passed */
  def expect (data: Dbl, expected: Double, msg: => String): Boolean = {
    addEvent(new MuteEvent())
    val got = peek(data)
    val good = got == expected
    addEvent(new UnmuteEvent())
    addEvent(new ExpectDblEvent(data, got, expected, msg))
    if (!good) fail
    good
  }
  def expect (data: Dbl, expected: Double): Boolean = {
    expect(data, expected, "")
  }

  /* Compare the floating point value of a node with an expected floating point value.
   * We will tolerate differences in the bottom bit.
   */
  /** A tolerant expect for Float
    * Allows for a single least significant bit error in the floating point representation */
  def expect (data: Bits, expected: Float, msg: => String): Boolean = {
    addEvent(new MuteEvent())
    val gotBits = peek(data).toInt
    var gotFLoat = java.lang.Float.intBitsToFloat(gotBits)
    val expectedBits = java.lang.Float.floatToIntBits(expected)
    var expectedFloat = expected
    if (gotFLoat != expectedFloat) {
      val gotDiff = gotBits - expectedBits
      // Do we have a single bit difference?
      if (scala.math.abs(gotDiff) <= 1) {
        expectedFloat = gotFLoat
      }
    }
    val good = gotFLoat == expectedFloat
    addEvent(new UnmuteEvent())
    addEvent(new ExpectFloEvent(data, gotFLoat, expected, msg))
    if (!good) fail
    good
  }
  def expect (data: Bits, expected: Float): Boolean = {
    expect(data, expected, "")
  }

  _signalMap ++= Driver.signalMap flatMap {
    case (m: Mem[_], id) =>
      (0 until m.n) map (idx => "%s[%d]".format(dumpName(m), idx) -> (id + idx))
    case (node, id) => Seq(dumpName(node) -> id)
  }

  Driver.dfs {
    case m: Mem[_] => (0 until m.n) foreach {idx =>
      val name = s"${dumpName(m)}[${idx}]"
      _chunks(name) = (m.needWidth-1)/64 + 1
    }
    case node if node.isInObject =>
      _chunks(dumpName(node)) = (node.needWidth-1)/64 + 1
    case _ =>
  }

  // Always use a specific seed so results (whenever) are reproducible.
  val rnd = new Random(Driver.testerSeed)
  val targetSubDir = Driver.appendString(Some(c.name),Driver.chiselConfigClassName)
  val target = s"${Driver.targetDir}/${targetSubDir}"
  // If the caller has provided a specific command to execute, use it.
  val cmd = testCmd match {
    case Some(cmd) => cmd
    case None => Driver.backend match {
      case b: FloBackend =>
        val command = ArrayBuffer(b.floDir + "fix-console", ":is-debug", "true",
          ":filename", target + ".hex", ":flo-filename", target + ".mwe.flo")
        if (Driver.isVCD) { command ++= ArrayBuffer(":is-vcd-dump", "true") }
        if (Driver.emitTempNodes) { command ++= ArrayBuffer(":emit-temp-nodes", "true") }
        command ++= ArrayBuffer(":target-dir", Driver.targetDir)
        command.mkString(" ")
      case b: VerilogBackend =>
        val vpd = dumpFile getOrElse s"${Driver.targetDir}/${c.name}.vpd"
        List(target, "-q", "+vcs+initreg+0",
          if (Driver.isVCD) s"+vpdfile=${vpd}" else "",
          if (Driver.isVCDMem) "+vpdmem" else "") mkString " "
      case c: CppBackend =>
        List(target, dumpFile map (vcd => s"+vcdfile=${vcd}") getOrElse "") mkString " "
      case _ => target
    }
  }
  private val (process: Process, exitValue: Future[Int], inChannel, outChannel, cmdChannel) = {
    val processBuilder = Process(cmd)
    val processLogger = ProcessLogger(println, _logs += _) // don't log stdout
    val process = processBuilder run processLogger

    // Set up a Future to wait for (and signal) the test process exit.
    val exitValue: Future[Int] = Future {
      blocking {
        process.exitValue
      }
    }
    // Wait for the startup message
    // NOTE: There may be several messages before we see our startup message.
    val simStartupMessageStart = "sim start on "
    while (!_logs.exists(_ startsWith simStartupMessageStart) && !exitValue.isCompleted) { Thread.sleep(100) }
    // Remove the startup message (and any precursors).
    while (!_logs.isEmpty && !_logs.head.startsWith(simStartupMessageStart)) {
      println(_logs.remove(0))
    }
    if (!_logs.isEmpty) println(_logs.remove(0)) else println("<no startup message>")
    while (_logs.size < 3) {
      // If the test application died, throw a run-time error.
      throwExceptionIfDead(exitValue)
      Thread.sleep(100)
    }
    val in_channel_name = _logs.remove(0)
    val out_channel_name = _logs.remove(0)
    val cmd_channel_name = _logs.remove(0)
    val in_channel = new Channel(in_channel_name)
    val out_channel = new Channel(out_channel_name)
    val cmd_channel = new Channel(cmd_channel_name)

    println(s"inChannelName: ${in_channel_name}")
    println(s"outChannelName: ${out_channel_name}")
    println(s"cmdChannelName: ${cmd_channel_name}")

    in_channel.consume
    cmd_channel.consume
    in_channel.release
    out_channel.release
    cmd_channel.release
    _t = 0

    Tester.processes += process
    (process, exitValue, in_channel, out_channel, cmd_channel)
  }

  private def start {
    mwhile(!recvOutputs) { }
    addEvent(new StartEvent(Driver.testerSeed, cmd))
    // reset(5)
    for (i <- 0 until 5) {
      mwhile(!sendCmd(SIM_CMD.RESET)) { }
      mwhile(!recvOutputs) { }
    }
  }

  def close {
    Tester.processes -= process
    process.destroy
  }

  /** Complete the simulation and inspect all tests */
  def finish: Boolean = {
    try {
      mwhile(!sendCmd(SIM_CMD.FIN)) { }
      while(!exitValue.isCompleted) { }
    }
    catch {
      // Depending on load and timing, we may get a TestApplicationException
      //  when the test application exits.
      //  Check the exit value.
      //  Anything other than 0 is an error.
      case e: TestApplicationException => if (e.exitVal != 0) fail
    }
    addEvent(new DumpEvent(newTestOutputString))
    addEvent(new FinishEvent(t, ok, failureTime))
    _logs.clear
    inChannel.close
    outChannel.close
    cmdChannel.close
    Tester.processes -= process
    ok
  }

  // Once everything has been prepared, we can start the communications.
  start
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
