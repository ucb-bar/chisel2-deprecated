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
import java.io.{File, IOException, InputStream, OutputStream, PrintStream}
import scala.sys.process._
import scala.io.Source._
import Literal._

case class Poke(val node: Node, val index: Int, val value: BigInt);

class Snapshot(val t: Int) {
  val pokes = new ArrayBuffer[Poke]()
}

class ManualTester[+T <: Module]
    (val c: T, 
      val isTrace: Boolean = true,
      val isSnapshotting: Boolean = false, 
      val isLoggingPokes: Boolean = false) {
  var testIn:  InputStream  = null
  var testOut: OutputStream = null
  var testErr: InputStream  = null
  val sb = new StringBuilder()
  var delta = 0
  var t = 0

  /**
   * Waits until the emulator streams are ready. This is a dirty hack related
   * to the way Process works. TODO: FIXME. 
   */
  def waitForStreams() = {
    var waited = 0
    while (testOut == null || testIn == null || testErr == null) {
      Thread.sleep(100)
      if (waited % 10 == 0 && waited > 30) {
        println("waiting for emulator process treams to be valid ...")
      }
    }
  }

  val snapshots = ArrayBuffer[Snapshot]();
  val pokez = ArrayBuffer[Snapshot]();

  val regs  = c.omods.filter(x => x.isInstanceOf[Reg]).map(x => x.getNode);
  val mems  = c.omods.filter(x => x.isInstanceOf[Mem[_]]).map(x => x.getNode);
  val mappings = new HashMap[String, Node]()

  def dump(): Snapshot = {
    val snap = new Snapshot(t)
    for (reg <- regs) 
      snap.pokes += Poke(reg, 0, peekBits(reg))
    for (mem <- mems) 
      for (i <- 0 until mem.depth) 
        snap.pokes += Poke(mem, i, peekBits(mem, i))
    snap
  }

  def snapshot(): Snapshot = {
    val snap = dump()
    snapshots += snap
    snap
  }

  def addPoke(snaps: ArrayBuffer[Snapshot], now: Int, poke: Poke) = {
    if (snaps.length > 0 && snaps.last.t > now) {
      val lastIndex = findSnapshotIndex(snaps, now)
      val amount = snaps.length-lastIndex-1
      println("TRIMMING " + amount + " FROM " + snaps.length)
      if (amount > 0) snaps.trimEnd(amount)
    }
    println("ADDING POKE T=" + now)
    if (snaps.length > 0 && snaps.last.t == now) 
      snaps.last.pokes += poke 
    else { 
      val snap = new Snapshot(now); 
      snap.pokes += poke
      snaps += snap;
    }
  }

  def loadSnapshots(filename: String): ArrayBuffer[Snapshot] = {
    var now = 0
    var lines = io.Source.fromFile(filename).getLines
    val snaps = new ArrayBuffer[Snapshot]()
    println("LOADING")
    for (line <- lines) {
      val words = line.split(" ")
      if (words.length > 0) {
        if (words(0) == "STEP") {
          assert(words.length == 2, "STEP TAKES ONE ARG")
          now += words(1).toInt
          println("  <STEP " + words(1).toInt + " T=" + now)
        } else if (words(0) == "POKE") {
          assert(words.length == 3 || words.length == 4, "POKE TAKES THREE / FOUR ARGS")
          val off = if (words.length == 4) words(3).toInt else -1
          addPoke(snaps, now, Poke(mappings(words(1)), off, words(2).toInt))
          println("  <POKE " + words(1) + " T=" + now)
        }
      }
    }
    println("LOADED " + snaps.length + " SNAPSHOTS")
    for (snap <- snaps)
      println("  SNAP T=" + snap.t + " N=" + snap.pokes.length)
    snaps
  }

  def loadSnapshotsInto(filename: String, snaps: ArrayBuffer[Snapshot]) = {
    snaps.trimStart(snaps.length)
    snaps ++= loadSnapshots(filename)
  }

  def loadPokes(filename: String) = {
    loadSnapshotsInto(filename, pokez)
    checkForPokes(0, t)
  }

  // TODO: MOVE TO SOMEWHERE COMMON TO BACKEND
  def ensureDir(dir: String): String = {
    val d = dir + (if (dir == "" || dir(dir.length-1) == '/') "" else "/")
    new File(d).mkdirs()
    d
  }
  def createOutputFile(name: String): java.io.FileWriter = {
    val baseDir = ensureDir(Module.targetDir)
    new java.io.FileWriter(baseDir + name)
  }

  def dumpSnapshots(filename: String, snapshots: ArrayBuffer[Snapshot]) = {
    var now = 0;
    val f = createOutputFile(filename)
    for (snapshot <- snapshots) {
      if (snapshot.t > now) {
        f.write("STEP " + (snapshot.t - now) + "\n")
        now = snapshot.t
      }
      for (p <- snapshot.pokes) {
        f.write("POKE " + dumpName(p.node) + " " + p.value + (if (p.index == -1) "" else (" " + p.index)) + "\n")
      }
    }
    f.close()
  }

  def load(s: Snapshot) = {
    println("LOADING SNAPSHOT AT " + s.t)
    for (poke <- s.pokes) 
      doPokeBits(poke.node, poke.value, poke.index)
  }

  def findSnapshotIndex(snaps: ArrayBuffer[Snapshot], target: Int): Int = {
    println("LOOKING FOR T=" + target + " OUT OF " + snaps.length + " SNAPS")
    for (i <- 0 until (snaps.length-1)) {
      if (snaps(i+1).t > target) {
        println("  FOUND I=" + i + " AT T=" + snaps(i).t)
        return i
      }
    }
    println("  DEFAULT I=" + (snaps.length-1) + " AT T=" + snaps.last.t)
    return snaps.length-1
  }

  def goto(target: Int) = {
    val lastIndex = findSnapshotIndex(snapshots, target)
    val snap = snapshots(lastIndex)
    snapshots.trimEnd(snapshots.length-lastIndex-1)
    println("FOUND SNAPSHOT AT T=" + snap.t)
    load(snap);
    t = snap.t
    for (tk <- snap.t to target) 
      step(1)
  }

  def puts(str: String) = {
    while (testOut == null) { Thread.sleep(100) }
    for (e <- str) testOut.write(e);
  }

  /**
   * Sends a command to the emulator and returns the reply.
   * The standard protocol treats a single line as a command, which always
   * returns a single line of reply.
   */
  def emulatorCmd(str: String): String = {
    // validate cmd
    if (str contains "\n") {
      System.err.print(s"emulatorCmd($str): command should not contain newline")
      return "error"
    }
    
    waitForStreams()
    
    // send command to emulator
    for (e <- str) testOut.write(e);
    testOut.write('\n');
    testOut.flush()

    // read output from emulator
    var c = testIn.read
    sb.clear()
    while (c != '\n' && c != -1) {
      if (c == 0) {
        Thread.sleep(100)
      }
      sb += c.toChar
      c   = testIn.read
    }
    
    // drain errors
    try {
      while(testErr.available() > 0) {
        System.err.print(Character.toChars(testErr.read()))
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
    for (clock <- Module.clocks) {
      if (clock.srcClock == null) {
        val s = BigInt(clocks(clock)).toString(16)
        cmd = cmd + " " + s
      }
    }
    emulatorCmd(cmd)
    // TODO: check for errors in return
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
      var cmd = ""
      if (off != -1) {
        cmd = "wire_peek " + dumpName(data) + " " + off;
      } else {
        cmd = "wire_peek " + dumpName(data);
      }
      val s = emulatorCmd(cmd)
      val rv = toLitVal(s)
      if (isTrace) println("  PEEK " + dumpName(data) + " " + (if (off >= 0) (off + " ") else "") + "-> " + s)
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
    emulatorCmd("reset " + n)
    // TODO: check for errors in return
    if (isTrace) println("RESET " + n)
  }

  def unstep(n: Int) = 
    goto(max(0, t-n))

  def doPokeBits(data: Node, x: BigInt, off: Int = -1): Unit = {
    if (dumpName(data) == "") {
      println("Unable to poke data " + data)
    } else {
      
      if (isTrace) println("  POKE " + dumpName(data) + " " + (if (off >= 0) (off + " ") else "") + "<- " + x)
      var cmd = ""
      if (off != -1) {
        cmd = "mem_poke " + dumpName(data) + " " + off;
      } else {
        cmd = "wire_poke " + dumpName(data);
      }
      cmd = cmd + " 0x" + x.toString(16);
      val rtn = emulatorCmd(cmd)
      if (rtn != "true") {
        System.err.print(s"FAILED: poke(${dumpName(data)}) returned false")
        ok = false
      }
    }
  }

  def pokeBits(data: Node, x: BigInt, off: Int = -1): Unit = {
    if (isSnapshotting || isLoggingPokes)
      addPoke(pokez, t, Poke(data, off, x))
    doPokeBits(data, x, off)
  }

  def pokeAt[T <: Bits](data: Mem[T], x: BigInt, off: Int): Unit = {
    pokeBits(data, x, off)
  }

  def poke(data: Bits, x: BigInt): Unit = {
    pokeBits(data.getNode, x)
  }

  def poke(data: Aggregate, x: Array[BigInt]): Unit = {
    val kv = (data.flatten.map(x => x._2), x).zipped;
    for ((x, y) <- kv)
      poke(x, y)
  }

  def checkForPokes(start: Int, target: Int) = {
    var pokeIndex = findSnapshotIndex(pokez, start)
    println("CHECKING POKES FROM T=" + start + " TO T=" + target + " POKEZ INDEX " + pokeIndex)
    for (tk <- start to target) {
      if (pokeIndex < pokez.length) {
        val snap = pokez(pokeIndex)
        println("  LOOKING AT POKES(" + pokeIndex + ") T=" + snap.t + " VS T=" + tk + " WITH N=" + snap.pokes.length + " POKES")
        if (snap.t == tk) {
          print("FOUND: ")
          load(snap)
          pokeIndex += 1
        }
      }
    }
  }

  def step(n: Int) = {
    if (isSnapshotting) snapshot()
    val target = t + n
    val s = emulatorCmd("step " + n)
    delta += s.toInt
    if (isTrace) println("STEP " + n + " -> " + target)
    if (isSnapshotting) 
      checkForPokes(t+1, target)
    t += n
  }

  def int(x: Boolean): BigInt = if (x) 1 else 0
  def int(x: Int): BigInt = x
  def int(x: Bits): BigInt = x.litValue()

  var ok = true;
  var failureTime = -1

  def expect (good: Boolean, msg: String): Boolean = {
    if (isTrace)
      println(msg + " " + (if (good) "PASS" else "FAIL"))
    if (!good) { ok = false; if (failureTime == -1) failureTime = t; }
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

  val rnd = if (Module.testerSeedValid) new Random(Module.testerSeed) else new Random()
  var process: Process = null

  def start(): Process = {
    val target = Module.targetDir + "/" + c.name
    val cmd = 
      (if (Module.backend.isInstanceOf[FloBackend]) {
         val dir = Module.backend.asInstanceOf[FloBackend].floDir
         dir + "fix-console :is-debug true :filename " + target + ".hex"
      } else {
         target + (if(Module.backend.isInstanceOf[VerilogBackend]) " -q" else "")
      })
    println("SEED " + Module.testerSeed)
    println("STARTING " + cmd)
    val processBuilder = Process(cmd)
    val pio = new ProcessIO(in => testOut = in, out => testIn = out, err => testErr = err)
    process = processBuilder.run(pio)
    waitForStreams()
    t = 0
    reset(5)
    for (mod <- c.omods.map(x => x.getNode)) mappings(dumpName(mod)) = mod
    process
  }

  def finish(): Boolean = {
    if (process != null) {
      emulatorCmd("quit")

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
    println("RAN " + t + " CYCLES " + (if (ok) "PASSED" else { "FAILED FIRST AT CYCLE " + failureTime }))
    ok
  }
}

class Tester[+T <: Module](c: T, isTrace: Boolean = true) extends ManualTester(c, isTrace) {
  start()
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

/*
class AdvTester[+T <: Module](val dut: T) extends Tester[T](dut) {
  val defaultMaxCycles = 1024
  type HMNN = HashMap[Node, Node]

  val preprocessors = new ArrayBuffer[Processable]()
  val postprocessors = new ArrayBuffer[Processable]()

  def takestep(work: => Unit = {}) = {
    step(1) 
    preprocessors.foreach(_.process())
    work
    postprocessors.foreach(_.process())
  }

  def until(pred: =>Boolean, maxCycles: Int = defaultMaxCycles)(work: =>Unit): Boolean = {
    var timeout_cycles = 0
    while(!pred && (timeout_cycles < maxCycles)) {
      takestep(work)
      timeout_cycles += 1
    }
    assert(timeout_cycles < maxCycles,
      "until timed out after %d cycles".format(timeout_cycles))
    pred
  }
  def eventually(pred: =>Boolean, maxCycles: Int = defaultMaxCycles) = {until(pred, maxCycles){}}
  def doUntil(work: =>Unit)(pred: =>Boolean, maxCycles: Int = defaultMaxCycles): Boolean = {
    takestep(work)
    until(pred, maxCycles){work}
  }

  class DecoupledSink[T <: Data, R]( socket: DecoupledIO[T], cvt: (HMNN,T)=>R ) extends Processable {
    var max_count = -1
    val outputs = new scala.collection.mutable.Queue[R]()
    def isFired = () => peek(socket.valid) == 1 && peek(socket.ready) == 1

    def process() = {
      if(isFired()) {
        outputs.enqueue(cvt(ovars, socket.bits))
      }
      poke(socket.ready, int(max_count < 1 || outputs.length < max_count))
    }

    // Initialize
    poke(socket.ready, true)
    preprocessors += this
  }

  object DecoupledSink {
    def apply[T<:Bits](socket: DecoupledIO[T]) = 
      new DecoupledSink(socket, (db:HMNN, socket_bits: T) => fix_neg(socket_bits, db(socket_bits).litValue()) )
    def apply[T<:Data](socket: DecoupledIO[T], extracts: Array[Bits]) = 
      new DecoupledSink(socket, (db:HMNN, socket_bits: T) => extracts.map(d => fix_neg(d, db(d).litValue()))
    )
  }

  class ValidSink[T <: Data, R]( socket: ValidIO[T], cvt: (HMNN,T)=>R ) extends Processable { 
    val outputs = new scala.collection.mutable.Queue[R]()
    def isValid = ovars(socket.valid).litValue() == 1

    def process() = {
      if(isValid) {
        outputs.enqueue(cvt(ovars, socket.bits))
      }
    }

    // Initialize
    preprocessors += this
  }
  object ValidSink {
    def apply[T<:Bits](socket: ValidIO[T]) = new ValidSink(socket, (db:HMNN, socket_bits: T) => fix_neg(socket_bits, db(socket_bits).litValue()) )
    def apply[T<:Data](socket: ValidIO[T], extracts: Array[Bits]) = new ValidSink(socket,
      (db:HMNN, socket_bits: T) => extracts.map(d => fix_neg(d, db(d).litValue()))
    )
  }

  class DecoupledSource[T <: Data, R]( socket: DecoupledIO[T], post: (HMNN,T,R)=>Unit ) extends Processable
  {
    val inputs = new scala.collection.mutable.Queue[R]()
    var justFired = false // Adjust for the fact that the step function returns ivars/ovars of BEFORE the clock edge

    private def isPresenting = ivars(socket.valid).litValue() == 1
    def isFired = ivars(socket.valid).litValue() == 1 && ovars(socket.ready).litValue() == 1
    def isIdle = !isPresenting && inputs.isEmpty && !justFired

    def process() = {
      justFired = isFired
      if(isFired) {
        ivars(socket.valid) = Bool(false)
      }
      if(!isPresenting && !inputs.isEmpty) {
        ivars(socket.valid) = Bool(true)
        post(ivars, socket.bits, inputs.dequeue())
      }
    }

    // Initialize
    ivars(socket.valid) = Bool(false)
    postprocessors += this
  }
  object DecoupledSource {
    def apply[T<:Bits](socket: DecoupledIO[T]) = new DecoupledSource(socket,
      (db:HMNN, socket_bits: T, in: BigInt) =>
        { db(socket_bits) = Lit.makeLit(Literal(in, width=socket_bits.getWidth, signed=in<0)){socket_bits} }
    )
    def apply[T<:Data](socket: DecoupledIO[T], injects: Array[Bits]) = new DecoupledSource(socket,
      (db:HMNN, socket_bits: T, ins: Array[BigInt]) => {
        injects.zip(ins).foreach(Function.tupled((inj, in) =>
          { db(inj) = Lit.makeLit(Literal(in, width=inj.getWidth, signed=in<0)){inj} }))
      }
    )
  }
  
  class ValidSource[T <: Data, R]( socket: ValidIO[T], post: (HMNN,T,R)=>Unit ) extends Processable
  {
    val inputs = new scala.collection.mutable.Queue[R]()
    var justFired = false // Adjust for the fact that the step function returns ivars/ovars of BEFORE the clock edge
    
    private def isPresenting = ivars(socket.valid).litValue() == 1
    def isIdle = !isPresenting && inputs.isEmpty && !justFired

    def process() = {
      // Always advance the input
      justFired = isPresenting // input is always fired on clock edge!
      ivars(socket.valid) = Bool(false)
      if(!inputs.isEmpty) {
        ivars(socket.valid) = Bool(true)
        post(ivars, socket.bits, inputs.dequeue())
      }
    }

    // Initialize
    ivars(socket.valid) = Bool(false)
    postprocessors += this
  }
  object ValidSource {
    def apply[T<:Bits](socket: ValidIO[T]) = new ValidSource(socket,
      (db:HMNN, socket_bits: T, in: BigInt) =>
        { db(socket_bits) = Lit.makeLit(Literal(in, width=socket_bits.getWidth, signed=in<0)){socket_bits} }
    )
    def apply[T<:Data](socket: ValidIO[T], injects: Array[Bits]) = new ValidSource(socket,
      (db:HMNN, socket_bits: T, ins: Array[BigInt]) => {
        injects.zip(ins).foreach(Function.tupled((inj, in) =>
          { db(inj) = Lit.makeLit(Literal(in, width=inj.getWidth, signed=in<0)){inj} }))
      }
    )
  }

}

trait Processable {
  def process(): Unit
}
 */
