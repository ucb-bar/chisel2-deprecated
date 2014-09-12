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

import collection.mutable.{ArrayBuffer, HashSet, HashMap, Stack, LinkedHashSet}

object Driver extends FileSystemUtilities{
  def apply[T <: Module](args: Array[String], gen: () => T, wrapped:Boolean = true): T = {
    Driver.initChisel(args)
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
    if (Driver.isTesting) test(mod, ftester)
    mod
  }

  private def executeUnwrapped[T <: Module](gen: () => T): T = {
    if (!Driver.chiselConfigMode.isEmpty && !Driver.chiselConfigClassName.isEmpty) { 
      val config = Class.forName(chiselConfigClassName.get).newInstance.asInstanceOf[ChiselConfig]
      val world = if(Driver.chiselConfigMode.get == "collect") new Collector(config.topDefinitions,config.knobValues) else new Instance(config.topDefinitions,config.knobValues)
      val p = Parameters.root(world)
      config.topConstraints.foreach(c => p.constrain(c))
      val c = execute(() => Module(gen())(Some(p)))
      if(Driver.chiselConfigMode.get == "collect") {
        val v = createOutputFile(Driver.chiselConfigClassName.get + ".knb")
        v.write(world.getKnobs)
        v.close
        val w = createOutputFile(Driver.chiselConfigClassName.get + ".cst")
        w.write(world.getConstraints)
        w.close
      }
      c
    } 
    else {
      execute(() => Module(gen())(None))
    }
  }

  private def execute[T <: Module](gen: () => T): T = {
    val c = gen()
    Driver.backend.initBackannotation
    /* Params - If dumping design, dump space to pDir*/
    if (Driver.chiselConfigMode == None || Driver.chiselConfigMode.get == "instance") { 
      if(Driver.chiselConfigDump && !Dump.dump.isEmpty) {
        val w = createOutputFile(Driver.chiselConfigClassName.get + ".prm")
        w.write(Dump.getDump); w.close
      }
      Driver.backend.elaborate(c)
      if (Driver.isCheckingPorts) Driver.backend.checkPorts(c)
      if (Driver.isCompiling && Driver.isGenHarness) Driver.backend.compile(c)
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
    implicitReset.component = Driver.topComponent
    implicitClock.component = Driver.topComponent
    topComponent.reset = Driver.implicitReset
    topComponent.hasExplicitReset = true
    topComponent.clock = Driver.implicitClock
    topComponent.hasExplicitClock = true    
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
    backend = new CppBackend
    topComponent = null
    randInitIOs.clear()
    clocks.clear()
    implicitReset = Bool(INPUT)
    implicitReset.isIo = true
    implicitReset.setName("reset")
    implicitClock = new Clock()
    implicitClock.setName("clk")
    nodes.clear()
    isInGetWidth = false
    startTime = System.currentTimeMillis

    // Backannotation
    isBackannotating = false
    model = ""
    signals.clear
    pseudoMuxes.clear
    modStackPushed = false

    readArgs(args)
  }

  private def readArgs(args: Array[String]): Unit = {
    var i = 0
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
        case "--wio" => {warnInputs = true; Driver.warnOutputs = true}
        case "--Wconnection" => saveConnectionWarnings = true
        case "--Wcomponent" => saveComponentTrace = true
        case "--noCombLoop" => dontFindCombLoop = true
        case "--genHarness" => isGenHarness = true
        case "--debug" => isDebug = true
        case "--lineNumbers" => getLineNumbers = true
        case "--cse" => isCSE = true
        case "--ioDebug" => isIoDebug = true
        case "--noIoDebug" => isIoDebug = false
        case "--vcd" => isVCD = true
        case "--vcdMem" => isVCDMem = true
        case "--v" => backend = new VerilogBackend
        case "--moduleNamePrefix" => Backend.moduleNamePrefix = args(i + 1); i += 1
        case "--inlineMem" => isInlineMem = true
        case "--noInlineMem" => isInlineMem = false
        case "--debugMem" => isDebugMem = true
        case "--backend" => {
          if (args(i + 1) == "v") {
            backend = new VerilogBackend
          } else if (args(i + 1) == "c") {
            backend = new CppBackend
          } else if (args(i + 1) == "flo") {
            backend = new FloBackend
          } else if (args(i + 1) == "dot") {
            backend = new DotBackend
          } else if (args(i + 1) == "fpga") {
            backend = new FPGABackend
          } else if (args(i + 1) == "counterc") {
            backend = new CounterCppBackend
          } else if (args(i + 1) == "counterv") {
            backend = new CounterVBackend
          } else if (args(i + 1) == "counterfpga") {
            backend = new CounterFPGABackend
          } else if (args(i + 1) == "counterw") {
            backend = new CounterWBackend
          } else {
            backend = Class.forName(args(i + 1)).newInstance.asInstanceOf[Backend]
          }
          i += 1
        }
        case "--compile" => isCompiling = true
        case "--test" => isTesting = true
        case "--targetDir" => targetDir = args(i + 1); i += 1
        case "--include" => includeArgs = args(i + 1).split(' ').toList; i += 1
        case "--checkPorts" => isCheckingPorts = true
        case "--reportDims" => isReportDims = true
        // Counter backend flags
        case "--backannotation" => isBackannotating = true
        case "--model" => model = args(i + 1) ; i += 1
        //Jackhammer Flags
        case "--configCollect"  => chiselConfigMode = Some("collect"); chiselConfigClassName = Some(args(i+1)); i+=1;  //dump constraints in dse dir
        case "--configInstance" => chiselConfigMode = Some("instance"); chiselConfigClassName = Some(args(i+1)); i+=1;  //use ChiselConfig to supply parameters
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
  var isDebugMem = false
  var backend: Backend = null
  var topComponent: Module = null
  val components = ArrayBuffer[Module]()
  var sortedComps: ArrayBuffer[Module] = null
  val nodes = ArrayBuffer[Node]()
  val blackboxes = ArrayBuffer[BlackBox]()
  val chiselOneHotMap = HashMap[(UInt, Int), UInt]()
  val chiselOneHotBitMap = HashMap[(Bits, Int), Bool]()
  val compStack = Stack[Module]()
  val parStack = new Stack[Parameters]
  var stackIndent = 0
  val printStackStruct = ArrayBuffer[(Int, Module)]()
  val randInitIOs = ArrayBuffer[Node]()
  val clocks = ArrayBuffer[Clock]()
  var implicitReset: Bool = null
  var implicitClock: Clock = null
  var isInGetWidth: Boolean = false
  /* Backannotation flags */
  var isBackannotating = false
  var model = ""
  val signals = LinkedHashSet[Node]()
  val pseudoMuxes = HashMap[Node, Node]()
  var modStackPushed: Boolean = false
  var startTime = 0L
  /* ChiselConfig flags */
  var chiselConfigClassName: Option[String] = None
  var chiselConfigMode: Option[String] = None
  var chiselConfigDump: Boolean = false

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
