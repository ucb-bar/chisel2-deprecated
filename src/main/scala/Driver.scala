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

import collection.mutable.{ArrayBuffer, HashSet, HashMap, LinkedHashMap, Stack, Queue => ScalaQueue}
import scala.collection.immutable.ListSet
import sys.process.{BasicIO,stringSeqToProcess}

object Driver extends FileSystemUtilities{
  def apply[T <: Module](args: Array[String], gen: () => T, wrapped:Boolean): T = {
    initChisel(args)
    try {
      if(wrapped) execute(gen) else executeUnwrapped(gen)
    } finally {
      ChiselError.report
      if (wantLineNumbers) {
        println("Re-running Chisel in debug mode to obtain erroneous line numbers...")
        apply(args :+ "--lineNumbers", gen, wrapped)
      }
    }
  }

  // If we encountered errors, and re-running with --lineNumbers may help
  //  diagnose the problem, indicate we should do so.
  def wantLineNumbers: Boolean = {
    if (ChiselError.hasErrors && !getLineNumbers) {
      // If we have a non-cpp compilation error with no line number information,
      //  re-running may help.
      ChiselError.getErrorList.exists(e => e.line == null && !e.msgFun().startsWith("failed to compile "))
    } else {
      false
    }
  }

  def apply[T <: Module](args: Array[String], gen: () => T, ftester: T => Tester[T], wrapped:Boolean): T = {
    val mod = apply(args, gen, wrapped)
    if (isTesting) ftester(mod).finish
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
        val w = createOutputFile(appendString(Some(c.name),chiselConfigClassName) + ".prm")
        w.write(Dump.getDump); w.close
      }
    }
    c
  }

  def elapsedTime: Long = System.currentTimeMillis - startTime

  def setTopComponent(mod: Module) {
    topComponent = Some(mod)
    implicitReset.compOpt = topComponent
    implicitClock.compOpt = topComponent
    mod._reset = Some(implicitReset)
    mod._clock = Some(implicitClock)
    mod.hasExplicitReset = true
    mod.hasExplicitClock = true
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
      c._reset match {
        case Some(r) if r != implicitReset => queue enqueue r.getNode
        case _ =>
      }
      for (pin <- c.resets.values)
        queue enqueue pin
    }

    // Do BFS
    val _walked = HashSet[Node](queue:_*)
    // Avoid a "java.lang.IllegalArgumentException: Flat hash tables cannot contain null elements" if node is null - unassigned MUX
    def walked(node: Node) = node == null || _walked(node)
    def enqueueNode(node: Node) { if (node != null) { queue enqueue node ; _walked += node }}
    def enqueueInputs(top: Node) { ListSet((top.inputs.filter(_ != null)):_*) filterNot walked foreach enqueueNode }
    def enqueueElems(agg: Data) { agg.flatten.unzip._2 filterNot walked foreach enqueueNode }
    while (!queue.isEmpty) {
      val top = queue.dequeue
      visit(top)
      top match {
        case b: Bundle => enqueueElems(b)
        case v: Vec[_] => enqueueElems(v) ; enqueueInputs(v)
        case _ => enqueueInputs(top)
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
      c._reset match {
        case Some(r) if r != implicitReset => stack push r.getNode
        case _ =>
      }
      for (pin <- c.resets.values)
        stack push pin
    }
    for (c <- components; a <- c.debugs)
      stack push a
    for (b <- blackboxes)
      stack push b.io

    // Do DFS
    val _walked = HashSet[Node](stack:_*)
    // Avoid a "java.lang.IllegalArgumentException: Flat hash tables cannot contain null elements" if node is null - unassigned MUX
    def walked(node: Node) = node == null || _walked(node)
    def pushNode(node: Node) { stack push node ; _walked += node }
    def pushInputs(top: Node) { ListSet(top.inputs:_*) filterNot walked foreach pushNode }
    def pushElems(agg: Data) { agg.flatten.unzip._2 filterNot walked foreach pushNode }
    while (!stack.isEmpty) {
      val top = stack.pop
      visit(top)
      top match {
        case b: Bundle => pushElems(b)
        case v: Vec[_] => pushElems(v) ; pushInputs(v)
        case _ => pushInputs(top)
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
    for(c <- components; (n, io) <- c.wires)
      pushInitialNode(io)
    // Ensure any nodes connected to reset are visited.
    for (c <- components) {
      c._reset match {
        case Some(r) if r != implicitReset => pushInitialNode(r.getNode)
        case _ =>
      }
      for (pin <- c.resets.values)
        pushInitialNode(pin)
    }
    val stack = inputs ++ res

    // Callers should ensure the node we're pushing isn't already on the stack.
    def pushBareNode(n: Node) {
      require(!stacked.contains(n), "idfs: pushNode() node on stack <%s>".format(n.toString()))
      stacked += n
      stack.push((n, false))
    }

    def visited(node: Node) = stacked contains node
    def pushElems(agg: Data) { agg.flatten.unzip._2 filterNot visited foreach pushBareNode }
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
          case v: Vec[_] => pushElems(v)
          case b: Bundle => pushElems(b)
          case _ => {}
        }
        // Push any un-visited children
        for (c <- top.inputs if !(stacked contains c)) { pushBareNode(c) }
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
    isDebug = false
    getLineNumbers = false
    isCSE = false
    isIoDebug = true
    isVCD = false
    isVCDMem = false
    isInlineMem = true
    isGenHarness = false
    isReportDims = false
    includeArgs = Nil
    targetDir = "."
    isCompiling = false
    isCheckingPorts = false
    isTesting = false
    testCommand = None
    isAssert = true
    isAssertWarn = false
    isDebugMem = false
    partitionIslands = false
    lineLimitFunctions = 0
    minimumLinesPerFile = 10000
    shadowRegisterInObject = false
    allocateOnlyNeededShadowRegisters = false
    compileInitializationUnoptimized = false
    useSimpleQueue = false
    parallelMakeJobs = 0
    isVCDinline = false
    isSupportW0W = false
    backend = new CppBackend
    topComponent = None
    moduleNamePrefix = ""
    components.clear()
    sortedComps.clear()
    orderedNodes.clear()
    blackboxes.clear()
    chiselOneHotMap.clear()
    chiselOneHotBitMap.clear()
    compStack.clear()
    parStack.clear()
    stackIndent = 0
    printStackStruct.clear()
    implicitReset.isIo = true
    implicitReset.setName("reset")
    implicitClock.setName("clk")
    clocks.clear()
    clocks += implicitClock
    isInGetWidth = false
    modStackPushed = false
    minimumCompatibility = Version("0.0.0")
    wError = false
    chiselConfigClassName = None
    chiselProjectName = None
    chiselConfigMode = None
    chiselConfigDump = false
    startTime = System.currentTimeMillis
    signalMap.clear
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
        case "--moduleNamePrefix" => moduleNamePrefix = args(i + 1); i += 1
        case "--inlineMem" => isInlineMem = true
        case "--noInlineMem" => isInlineMem = false
        case "--assert" => isAssert = true
        case "--assertWarn" => isAssertWarn = true
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
        case "--testCommand" =>
          var cmd = ""
          while(i + 1 < args.size && args(i + 1).substring(0,2) != "--") {
            cmd += args(i + 1) + " " ; i += 1 }
          testCommand = Some(cmd); i += 1
        case "--targetDir" => targetDir = ensureDir(args(i + 1)); i += 1
        case "--include" => includeArgs = args(i + 1).split(' ').toList; i += 1
        case "--checkPorts" => isCheckingPorts = true
        case "--reportDims" => isReportDims = true
        //Jackhammer Flags
        case "--configName" =>  chiselConfigClassName = Some(args(i + 1)); i += 1
        case "--configCollect"  => chiselConfigMode = Some("collect"); chiselConfigClassName = Some(getArg(args(i + 1),1)); chiselProjectName = Some(getArg(args(i + 1),0)); i += 1;  //dump constraints in dse dir
        case "--configInstance" => chiselConfigMode = Some("instance"); chiselConfigClassName = Some(getArg(args(i + 1),1)); chiselProjectName = Some(getArg(args(i + 1),0)); i += 1;  //use ChiselConfig to supply parameters
        case "--configDump" => chiselConfigDump = true; //when using --configInstance, write Dump parameters to .prm file in targetDir
        case "--dumpTestInput" => dumpTestInput = true
        case "--testerSeed" => {
          testerSeed = args(i + 1).toLong
          i += 1
        }
        case "--emitTempNodes" => {
            isDebug = true
            emitTempNodes = true
        }
        // Dreamer configuration flags
        case "--numRows" => {
          if (backend.isInstanceOf[FloBackend]) {
            backend.asInstanceOf[FloBackend].DreamerConfiguration.numRows = args(i + 1).toInt
          }
          i += 1
        }
        case "--numCols" => {
          if (backend.isInstanceOf[FloBackend]) {
            backend.asInstanceOf[FloBackend].DreamerConfiguration.numCols = args(i + 1).toInt
          }
          i += 1
        }
        case "--minimumCompatibility" => minimumCompatibility = Version(args(i + 1)); i += 1
        case "--wError" => wError = true
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
      case "null" => new Backend
      case "c" => new CppBackend
      case "dot" => new DotBackend
      case "flo" => new FloBackend
      case "fpga" => new FPGABackend
      case "sysc" => new SysCBackend
      case "v" => new VerilogBackend
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
  var targetDir: String = "."
  var isCompiling = false
  var isCheckingPorts = false
  var isTesting = false
  var testCommand: Option[String] = None
  var isAssert = true
  var isAssertWarn = false
  var isDebugMem = false
  var partitionIslands = false
  var lineLimitFunctions = 0
  var minimumLinesPerFile = 10000
  var shadowRegisterInObject = false
  var allocateOnlyNeededShadowRegisters = false
  var compileInitializationUnoptimized = false
  var useSimpleQueue = false
  var parallelMakeJobs = 0
  var isVCDinline = false
  var isSupportW0W = false
  var backend: Backend = new CppBackend
  var topComponent: Option[Module] = None
  var moduleNamePrefix = ""
  val components = ArrayBuffer[Module]()
  val sortedComps = ArrayBuffer[Module]()
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
  val implicitClock = Clock()
  var isInGetWidth: Boolean = false
  var modStackPushed: Boolean = false
  var modAdded: Boolean = false
  var minimumCompatibility = Version("0.0.0")
  var wError = false
  /* ChiselConfig flags */
  var chiselConfigClassName: Option[String] = None
  var chiselProjectName: Option[String] = None
  var chiselConfigMode: Option[String] = None
  var chiselConfigDump: Boolean = false
  var startTime = 0L
  /* For tester */
  val signalMap = LinkedHashMap[Node, Int]()
  var nodeId = 0
  def getNodeId = {
    val id = nodeId
    nodeId +=1
    id
  }

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

  // This value may be overridden with a command line option --testerSeed
  var testerSeed = System.currentTimeMillis()

  // Setting this to TRUE will result in temporary values (ie, nodes
  // named "T*") to be emited to the VCD file.
  var emitTempNodes = false

  // Indicate if an external command is available.
  def isCommandAvailable(cmd: String): Boolean = {
    // Eat any output.
    val sb = new StringBuffer
    val ioToDevNull = BasicIO(false, sb, None)

    Seq("bash", "-c", "which %s".format(cmd)).run(ioToDevNull).exitValue == 0
  }

  lazy val isVCSAvailable = isCommandAvailable("vcs")
}
