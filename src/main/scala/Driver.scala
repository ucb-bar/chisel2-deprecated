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

import collection.mutable.{ArrayBuffer, HashSet, HashMap, Stack, LinkedHashSet, Queue => ScalaQueue}
import scala.math.min

object Driver extends FileSystemUtilities{
  def apply[T <: Module](args: Array[String], gen: () => T, wrapped:Boolean = true): T = {
    initChisel(args)
    try {
      if(wrapped) execute(gen) else executeUnwrapped(gen)
    } finally {
      ChiselError.report
      if (ChiselError.hasErrors && !getLineNumbers) {
        println("Re-running Chisel in debug mode to obtain erroneous line numbers...")
        apply(args :+ "--lineNumbers", gen, wrapped)
      }
    }
  }

  def apply[T <: Module](args: Array[String], gen: () => T,
                         ftester: T => Tester[T], wrapped:Boolean = true): T = {
    val mod = if(wrapped) apply(args, gen) else apply(args,gen,false)
    if (isTesting) test(mod, ftester)
    mod
  }

  private def executeUnwrapped[T <: Module](gen: () => T): T = {
    if (!chiselConfigMode.isEmpty && !chiselConfigClassName.isEmpty) {
      val name = appendString(chiselProjectName,chiselConfigClassName)
      val config = try {
        Class.forName(name).newInstance.asInstanceOf[ChiselConfig]
      } catch {
        case e: java.lang.ClassNotFoundException =>
          throwException("Could not find the ChiselConfig subclass you asked for (\"" +
                          name + "\"), did you misspell it?", e)
      }
      val world = if(chiselConfigMode.get == "collect") {
        new Collector(config.topDefinitions,config.knobValues)
      } else { new Instance(config.topDefinitions,config.knobValues) }
      val p = Parameters.root(world)
      config.topConstraints.foreach(c => p.constrain(c))
      val c = execute(() => Module(gen())(p))
      if(chiselConfigMode.get == "collect") {
        val v = createOutputFile(chiselConfigClassName.get + ".knb")
        v.write(world.getKnobs)
        v.close
        val w = createOutputFile(chiselConfigClassName.get + ".cst")
        w.write(world.getConstraints)
        w.close
      }
      c
    }
    else {
      execute(() => Module(gen()))
    }
  }

  private def execute[T <: Module](gen: () => T): T = {
    val c = gen()
    /* Params - If dumping design, dump space to pDir*/
    if (chiselConfigMode == None || chiselConfigMode.get == "instance") {
      setTopComponent(c)
      backend.elaborate(c)
      if (isCompiling && isGenHarness) backend.compile(c)
      if(chiselConfigDump && !Dump.dump.isEmpty) {
        val w = createOutputFile(appendString(Some(topComponent.name),chiselConfigClassName) + ".prm")
        w.write(Dump.getDump); w.close
      }
    }
    c
  }

  private def test[T <: Module](mod: T, ftester: T => Tester[T]): Unit = {
    var res = false
    var tester: Tester[T] = null
    try {
      tester = ftester(mod)
    } finally {
      if (tester != null && tester.process != null)
        res = tester.finish()
    }
    println(if (res) "PASSED" else "*** FAILED ***")
    if(!res) throwException("Module under test FAILED at least one test vector.")
  }

  def elapsedTime: Long = System.currentTimeMillis - startTime

  def setTopComponent(mod: Module): Unit = {
    topComponent = mod
    implicitReset.component = topComponent
    implicitClock.component = topComponent
    topComponent.reset = implicitReset
    topComponent.hasExplicitReset = true
    topComponent.clock = implicitClock
    topComponent.hasExplicitClock = true
  }

  def bfs (visit: Node => Unit) = {
    // initialize BFS
    val queue = new ScalaQueue[Node]

    for (c <- components; a <- c.debugs)
      queue enqueue a
    for (b <- blackboxes)
      queue enqueue b.io
    for (c <- components; (n, io) <- c.wires)
      queue enqueue io
    // Ensure any nodes connected to reset are visited.
    for (c <- components) {
      if (!(c.defaultResetPin == null)) {
        queue enqueue c.defaultResetPin
      }
      if (!(c._reset == null) && !(c._reset == implicitReset)) {
        queue enqueue c._reset.getNode
      }
    }

    // Do BFS
    val walked = HashSet[Node]()
    while (!queue.isEmpty) {
      val top = queue.dequeue
      walked += top
      visit(top)
      top match {
        case b: Bundle =>
          for ((n, io) <- b.flatten; if !(io == null) && !(walked contains io)) {
            queue enqueue io
            walked += io
          }
        case v: Vec[_] =>
          for ((n, e) <- v.flatten; if !(e == null) && !(walked contains e)) {
            queue enqueue e
            walked += e
          }
        case _ =>
      }
      for (i <- top.inputs; if !(i == null) && !(walked contains i)) {
        queue enqueue i
        walked += i
      }
    }
  }

  def dfs (visit: Node => Unit) = {
    val stack = new Stack[Node]
    // initialize DFS
    for (c <- components; (n, io) <- c.wires)
      stack push io
    // Ensure any nodes connected to reset are visited.
    for (c <- components) {
      if (!(c.defaultResetPin == null)) {
        stack push c.defaultResetPin
      }
      if (!(c._reset == null) && !(c._reset == implicitReset)) {
        stack push c._reset.getNode
      }
    }
    for (c <- components; a <- c.debugs)
      stack push a
    for (b <- blackboxes)
      stack push b.io

    // Do DFS
    val walked = HashSet[Node]()
    while (!stack.isEmpty) {
      val top = stack.pop
      walked += top
      visit(top)
      top match {
        case b: Bundle =>
          for ((n, io) <- b.flatten; if !(io == null) && !(walked contains io)) {
            stack push io
            walked += io
          }
        case v: Vec[_] => {
          for ((n, e) <- v.flatten; if !(e == null) && !(walked contains e)) {
            stack push e
            walked += e
          }
          for (i <- top.inputs; if !(i == null) && !(walked contains i) && !i.isIo) {
            stack push i
            walked += i
          }
        }
        case _ => {
          for (i <- top.inputs; if !(i == null) && !(walked contains i) && !i.isIo) {
            stack push i
            walked += i
          }
        }
      }
    }
  }

  // A "depth-first" search for width inference.
  def idfs(visit: Node => Unit): Unit = {
    // We "flag" stack items to indicate whether we've dealt with their children or not.
    val res = new Stack[(Node, Boolean)]
    val inputs = new Stack[(Node, Boolean)]
    val stacked = new HashSet[Node]

    // Order the stack so input nodes are handled first.
    def pushInitialNode(n: Node) {
      if (!stacked.contains(n)) {
        stacked += n
        n match {
          case b: Bits if b.isIo && b.dir == INPUT => {
            inputs.push((n, false))
          }
          case _ => {
            res.push((n, false))
          }
        }
      }
    }
    for (c <- components; a <- c.debugs)
      pushInitialNode(a)
    for(b <- blackboxes)
      pushInitialNode(b.io)
    for(c <- components; (n, io) <- c.wires) {
      pushInitialNode(io)
    }
    // Ensure any nodes connected to reset are visited.
    for (c <- components) {
      if (!(c.defaultResetPin == null)) {
        pushInitialNode(c.defaultResetPin)
      }
      if (!(c._reset == null) && !(c._reset == implicitReset)) {
        pushInitialNode(c._reset.getNode)
      }
    }
    val stack = inputs ++ res

    // Callers should ensure the node we're pushing isn't already on the stack.
    def pushBareNode(n: Node) {
      require(!stacked.contains(n), "idfs: pushNode() node on stack <%s>".format(n.toString()))
      stacked += n
      stack.push((n, false))
    }

    // Walk the graph in depth-first order,
    //  visiting nodes as we do so.
    while (!stack.isEmpty) {
      val (top, dealtWithChildren) = stack.pop
      // Have we visited all our children
      if (dealtWithChildren) {
        // Yes. Visit this node.
        visit(top)
      } else {
        // We still have children to visit.
        // Put this node back on the stack, with a flag indicating we've dealt with its children
        stack push ((top, true))
        top match {
          case v: Vec[_] => {
            for ((n, e) <- v.flatten;
            if !(stacked contains e)) {
              pushBareNode(e)
            }
          }
          case b: Bundle => {
            for ((n, e) <- b.flatten;
            if !(stacked contains e)) {
              pushBareNode(e)
            }
          }
          case _ => {}
        }
        // Push any un-visited children
        for (c <- top.inputs;
        if !(stacked contains c)) {
          pushBareNode(c)
        }
      }
    }
  }

  def initChisel(args: Array[String]): Unit = {
    ChiselError.clear()
    warnInputs = false
    warnOutputs = false
    saveConnectionWarnings = false
    saveComponentTrace = false
    dontFindCombLoop = false
    isGenHarness = false
    isDebug = false
    getLineNumbers = false
    isCSE = false
    isIoDebug = true
    isVCD = false
    isVCDMem = false
    isReportDims = false
    targetDir = "."
    components.clear()
    sortedComps.clear()
    compStack.clear()
    stackIndent = 0
    printStackStruct.clear()
    blackboxes.clear()
    chiselOneHotMap.clear()
    chiselOneHotBitMap.clear()
    isCompiling = false
    isCheckingPorts = false
    isTesting = false
    isDebugMem = false
    isSupportW0W = false
    partitionIslands = false
    lineLimitFunctions = 0
    minimumLinesPerFile = 0
    shadowRegisterInObject = false
    allocateOnlyNeededShadowRegisters = false
    compileInitializationUnoptimized = false
    useSimpleQueue = false
    parallelMakeJobs = 0
    isVCDinline = false
    isSupportW0W = false
    hasMem = false
    hasSRAM = false
    sramMaxSize = 0
    topComponent = null
    clocks.clear()
    implicitReset.isIo = true
    implicitReset.setName("reset")
    implicitClock = new Clock()
    implicitClock.setName("clk")
    nodes.clear()
    orderedNodes.clear()
    isInGetWidth = false
    startTime = System.currentTimeMillis
    modStackPushed = false

    readArgs(args)
  }

  private def readArgs(args: Array[String]): Unit = {
    var i = 0
    var backendName = "c"     // Default backend is Cpp.
    while (i < args.length) {
      val arg = args(i)
      arg match {
        case "--Wall" => {
          saveConnectionWarnings = true
          saveComponentTrace = true
          isCheckingPorts = true
        }
        case "--wi" => warnInputs = true
        case "--wo" => warnOutputs = true
        case "--wio" => {warnInputs = true; warnOutputs = true}
        case "--Wconnection" => saveConnectionWarnings = true
        case "--Wcomponent" => saveComponentTrace = true
        case "--W0W" => isSupportW0W = true
        case "--noW0W" => isSupportW0W = false
        case "--noCombLoop" => dontFindCombLoop = true
        case "--genHarness" => isGenHarness = true
        case "--debug" => isDebug = true
        case "--lineNumbers" => getLineNumbers = true
        case "--cse" => isCSE = true
        case "--ioDebug" => isIoDebug = true
        case "--noIoDebug" => isIoDebug = false
        case "--vcd" => isVCD = true
        case "--vcdMem" => isVCDMem = true
        case "--v" => backendName = "v"
        case "--moduleNamePrefix" => Backend.moduleNamePrefix = args(i + 1); i += 1
        case "--inlineMem" => isInlineMem = true
        case "--noInlineMem" => isInlineMem = false
        case "--assert" => isAssert = true
        case "--noAssert" => isAssert = false
        case "--debugMem" => isDebugMem = true
        case "--partitionIslands" => partitionIslands = true
        case "--lineLimitFunctions" => lineLimitFunctions = args(i + 1).toInt; i += 1
        case "--minimumLinesPerFile" => minimumLinesPerFile = args(i + 1).toInt; i += 1
        case "--shadowRegisterInObject" => shadowRegisterInObject = true
        case "--allocateOnlyNeededShadowRegisters" => allocateOnlyNeededShadowRegisters = true
        case "--compileInitializationUnoptimized" => compileInitializationUnoptimized = true
        case "--useSimpleQueue" => useSimpleQueue = true
        case "--parallelMakeJobs" => parallelMakeJobs = args(i + 1).toInt; i += 1
        case "--isVCDinline" => isVCDinline = true
        case "--backend" => backendName = args(i + 1); i += 1
        case "--compile" => isCompiling = true
        case "--test" => isTesting = true
        case "--targetDir" => targetDir = args(i + 1); i += 1
        case "--include" => includeArgs = args(i + 1).split(' ').toList; i += 1
        case "--checkPorts" => isCheckingPorts = true
        case "--reportDims" => isReportDims = true
        //Jackhammer Flags
        case "--configCollect"  => chiselConfigMode = Some("collect"); chiselConfigClassName = Some(getArg(args(i+1),1)); chiselProjectName = Some(getArg(args(i+1),0)); i+=1;  //dump constraints in dse dir
        case "--configInstance" => chiselConfigMode = Some("instance"); chiselConfigClassName = Some(getArg(args(i+1),1)); chiselProjectName = Some(getArg(args(i+1),0)); i+=1;  //use ChiselConfig to supply parameters
        case "--configDump" => chiselConfigDump = true; //when using --configInstance, write Dump parameters to .prm file in targetDir
        case "--dumpTestInput" => dumpTestInput = true
        case "--testerSeed" => {
          testerSeedValid = true
          testerSeed = args(i+1).toLong
          i += 1
        }
        case "--emitTempNodes" => {
            isDebug = true
            emitTempNodes = true
        }
        // Dreamer configuration flags
        case "--numRows" => {
          if (backend.isInstanceOf[FloBackend]) {
            backend.asInstanceOf[FloBackend].DreamerConfiguration.numRows = args(i+1).toInt
          }
          i += 1
        }
        case "--numCols" => {
          if (backend.isInstanceOf[FloBackend]) {
            backend.asInstanceOf[FloBackend].DreamerConfiguration.numCols = args(i+1).toInt
          }
          i += 1
        }
        case any => ChiselError.warning("'" + arg + "' is an unknown argument.")
      }
      i += 1
    }
    // Check for bogus flags
    if (!isVCD) {
      isVCDinline = false
    }
    // Set the backend after we've interpreted all the arguments.
    // (The backend may want to configure itself based on the arguments.)
    backend = backendName match  {
      case "v" => new VerilogBackend
      case "c" => new CppBackend
      case "flo" => new FloBackend
      case "dot" => new DotBackend
      case "fpga" => new FPGABackend
      case "sysc" => new SysCBackend
      case _ => Class.forName(backendName).newInstance.asInstanceOf[Backend]
    }
  }

  var warnInputs = false
  var warnOutputs = false
  var saveConnectionWarnings = false
  var saveComponentTrace = false
  var dontFindCombLoop = false
  var isDebug = false
  var getLineNumbers = false
  var isCSE = false
  var isIoDebug = true
  var isVCD = false
  var isVCDMem = false
  var isInlineMem = true
  var isGenHarness = false
  var isReportDims = false
  var includeArgs: List[String] = Nil
  var targetDir: String = null
  var isCompiling = false
  var isCheckingPorts = false
  var isTesting = false
  var isAssert = true
  var isDebugMem = false
  var partitionIslands = false
  var lineLimitFunctions = 0
  var minimumLinesPerFile = 0
  var shadowRegisterInObject = false
  var allocateOnlyNeededShadowRegisters = false
  var compileInitializationUnoptimized = false
  var useSimpleQueue = false
  var parallelMakeJobs = 0
  var isVCDinline = false
  var isSupportW0W = false
  var hasMem = false
  var hasSRAM = false
  var sramMaxSize = 0
  var backend: Backend = null
  var topComponent: Module = null
  val components = ArrayBuffer[Module]()
  val sortedComps = ArrayBuffer[Module]()
  val nodes = ArrayBuffer[Node]()
  val orderedNodes = ArrayBuffer[Node]()
  val blackboxes = ArrayBuffer[BlackBox]()
  val chiselOneHotMap = HashMap[(UInt, Int), UInt]()
  val chiselOneHotBitMap = HashMap[(Bits, Int), Bool]()
  val compStack = Stack[Module]()
  val parStack = new Stack[Parameters]
  var stackIndent = 0
  val printStackStruct = ArrayBuffer[(Int, Module)]()
  val clocks = ArrayBuffer[Clock]()
  val implicitReset = Bool(INPUT)
  var implicitClock: Clock = null
  var isInGetWidth: Boolean = false
  var modStackPushed: Boolean = false
  var modAdded: Boolean = false
  var startTime = 0L
  /* ChiselConfig flags */
  var chiselConfigClassName: Option[String] = None
  var chiselProjectName: Option[String] = None
  var chiselConfigMode: Option[String] = None
  var chiselConfigDump: Boolean = false

  def appendString(s1:Option[String],s2:Option[String]):String = {
    if(s1.isEmpty && s2.isEmpty) "" else {
      if(!s1.isEmpty) {
        s1.get + (if(!s2.isEmpty) "." + s2.get else "")
      } else {
        if(!s2.isEmpty) s2.get else ""
      }
    }
  }
  def getArg(s:String,i:Int):String = s.split('.')(i)

  // Setting this to TRUE will case the test harness to print its
  // standard input stream to a file.
  var dumpTestInput = false

  // Setting this to TRUE will initialize the tester's RNG with the
  // seed below.
  var testerSeedValid = false
  var testerSeed = System.currentTimeMillis()

  // Setting this to TRUE will result in temporary values (ie, nodes
  // named "T*") to be emited to the VCD file.
  var emitTempNodes = false
}
