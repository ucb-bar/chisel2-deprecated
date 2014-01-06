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
import scala.math._
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Queue=>ScalaQueue}
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.BitSet
import java.lang.reflect.Modifier._
import scala.sys.process._
import scala.math.max;
import Node._
import Literal._
import Bundle._
import ChiselError._
import Module._

import Direction._

object Module {
  /* We have to keep a list of public methods which happen to be public,
   have no arguments yet should not be used to generate C++ or Verilog code. */
  val keywords = HashSet[String]("test")

  var warnInputs = false
  var warnOutputs = false
  var saveWidthWarnings = false
  var saveConnectionWarnings = false
  var saveComponentTrace = false
  var dontFindCombLoop = false
  var isDebug = false;
  var isIoDebug = true;
  var isClockGatingUpdates = false;
  var isClockGatingUpdatesInline = false;
  var isVCD = false;
  var isInlineMem = true;
  var isFolding = true;
  var isGenHarness = false;
  var isReportDims = false;
  var isPruning = false;
  var scanFormat = "";
  var scanArgs: ArrayBuffer[Node] = null;
  var printFormat = "";
  var printArgs: ArrayBuffer[Node] = null;
  var tester: Tester[Module] = null;
  var includeArgs: List[String] = Nil;
  var targetDir: String = null;
  var isEmittingComponents = false;
  var isCompiling = false;
  var isCheckingPorts = false
  var isTesting = false;
  var backend: Backend = null;
  var topComponent: Module = null;
  val components = ArrayBuffer[Module]();
  var sortedComps: ArrayBuffer[Module] = null
  val procs = ArrayBuffer[proc]();
  val resetList = ArrayBuffer[Node]();
  val muxes = ArrayBuffer[Node]();
  val nodes = ArrayBuffer[Node]()
  val blackboxes = ArrayBuffer[BlackBox]()
  var ioMap = new HashMap[Node, Int];
  var chiselOneHotMap = new HashMap[(UInt, Int), UInt]
  var chiselOneHotBitMap = new HashMap[(Bits, Int), Bool]
  var chiselAndMap = new HashMap[(Node, Node), Bool]
  var searchAndMap = true
  var ioCount = 0;
  val compStack = new Stack[Module]();
  var stackIndent = 0;
  var printStackStruct = ArrayBuffer[(Int, Module)]();
  val printfs = ArrayBuffer[Printf]()
  val randInitIOs = new ArrayBuffer[Node]()
  val clocks = new ArrayBuffer[Clock]()
  var implicitReset: Bool = null
  var implicitClock: Clock = null
  var crossFilename: String = ""                                      // by Donggyu
  val crosses = new ArrayBuffer[(Double, Array[Node], Array[Node])]() // by Donggyu
  var criticalPath = ""                                               // by Donggyu
  var criticalPathDelay = 0.0                                         // by Donggyu

  /* Any call to a *Module* constructor without a proper wrapping
   into a Module.apply() call will be detected when trigger is false. */
  var trigger: Boolean = false

  def apply[T <: Module](c: => T): T = {
    trigger = true
    /* *push* is done in the Module constructor because we don't have
     a *this* pointer before then, yet we need to store it before the subclass
     constructors are built. */
    val res = c
    pop()
    for ((n, io) <- res.wires) {
      io.isIo = true
    }
    res
  }

  def splitArg (s: String) : List[String] = s.split(' ').toList;

  def initChisel () {
    ChiselError.clear();
    warnInputs = false
    warnOutputs = false
    saveWidthWarnings = false
    saveConnectionWarnings = false
    saveComponentTrace = false
    dontFindCombLoop = false
    isGenHarness = false;
    isDebug = false;
    isIoDebug = true;
    isClockGatingUpdates = false;
    isClockGatingUpdatesInline = false;
    isVCD = false;
    isFolding = true;
    isReportDims = false;
    scanFormat = "";
    scanArgs = new ArrayBuffer[Node]();
    printFormat = "";
    printArgs = new ArrayBuffer[Node]();
    tester = null;
    targetDir = "."
    components.clear();
    compStack.clear();
    stackIndent = 0;
    printfs.clear();
    printStackStruct.clear();
    procs.clear();
    resetList.clear()
    muxes.clear();
    blackboxes.clear();
    ioMap.clear()
    chiselOneHotMap.clear()
    chiselOneHotBitMap.clear()
    chiselAndMap.clear()
    searchAndMap = false
    ioCount = 0;
    isEmittingComponents = false;
    isCompiling = false;
    isCheckingPorts = false
    isTesting = false;
    backend = new CppBackend
    topComponent = null;
    randInitIOs.clear()
    clocks.clear()
    implicitReset = Bool(INPUT)
    implicitReset.isIo = true
    implicitReset.setName("reset")
    implicitClock = new Clock()
    implicitClock.setName("clk")
    crossFilename = ""  // by Donggyu
    crosses.clear()     // by Donggyu
    criticalPathDelay = 0.0 // by Donggyu

    /* Re-initialize global variables defined in object Node {} */
    nodes.clear()
    clk = UInt(INPUT, 1)
    clk.setName("clk")

    isCoercingArgs = true
    isInGetWidth = false
    conds.clear()
    conds.push(Bool(true))
    keys.clear()
  }

  //component stack handling stuff

  def isSubclassOfModule(x: java.lang.Class[_]): Boolean = {
    val classString = x.toString;
    if(classString == "class java.lang.Object") {
      false
    } else if(classString == "class Chisel.Module") {
      true
    } else {
      isSubclassOfModule(x.getSuperclass)
    }
  }

  private def push(c: Module) {
    if( !Module.trigger ) {
      ChiselError.error(
        c.getClass.getName + " was not properly wrapped into a module() call.")
    }
    Module.trigger = false
    compStack.push(c);
    printStackStruct += ((stackIndent, c));
    stackIndent += 1;
  }

  def pop(){
    val c = compStack.pop;
    if( !compStack.isEmpty ) {
      val dad = compStack.top;
      c.parent = dad;
      dad.children += c;
    }
    stackIndent -= 1;
    c.level = 0;
    for(child <- c.children) {
      c.level = math.max(c.level, child.level + 1);
    }
  }

  def getComponent(): Module = if(compStack.length != 0) compStack.top else null

  def setAsTopComponent(mod: Module) {
    topComponent = mod;
    implicitReset.component = topComponent
    implicitClock.component = topComponent
    topComponent.reset = Module.implicitReset
    topComponent.hasExplicitReset = true
    topComponent.clock = Module.implicitClock
    topComponent.hasExplicitClock = true    
  }
  
  // automatic pipeline stuff
  var autoPipe = false
  var pipeline = new HashMap[Int, ArrayBuffer[(Node, Bits)]]() //need to be deprecated
  var pipelineComponent: Module = null
  var pipelineReg = new HashMap[Int, ArrayBuffer[Reg]]()
  
  val forwardedRegs = new HashSet[Reg]
  val forwardedMemReadPorts = new HashSet[(TransactionMem[_], FunRdIO[_])]
  val memNonForwardedWritePoints = new HashSet[FunWrIO[_]]
  
  val speculation = new ArrayBuffer[(Bits, Bits)]//need to deprecate
  val speculatedRegs = new ArrayBuffer[(Reg, Data)]
  
  var annotatedStages = new HashMap[Node, Int]()
  
  var nodeToStageMap: HashMap[Node, Int] = null
  
  var ArchitecturalRegs: ArrayBuffer[Reg] = null
  var TransactionMems: ArrayBuffer[TransactionMem[ Data ]] = null
  var VariableLatencyComponents: ArrayBuffer[TransactionalComponent] = null
  
  val valids = new ArrayBuffer[Bool]//need to deprecate
  val stalls = new HashMap[Int, ArrayBuffer[Bool]]
  val kills = new HashMap[Int, ArrayBuffer[Bool]]
  val speckills = new HashMap[Int, ArrayBuffer[Bool]]
  var globalStall: Bool = null
  val hazards = new ArrayBuffer[(Bool, Delay, Int, Int, Bool, FunRdIO[Data])]//need to deprecate
  
  var pipelineLength = 0
  val stageValids = new ArrayBuffer[Bool]
  val stageStalls = new ArrayBuffer[Bool]
  val stageKills = new ArrayBuffer[Bool]
  val fillerNodes = new HashSet[Node]
  val requireStageSet = new HashSet[Node]

  val regWritePoints = new HashSet[Node] //list of all register write inputs and transactionMem write port nodes(including write addr, write data, write en)
  val regReadPoints = new HashSet[Node] //list of all register read outputs and transactionMem read port
  val tMemWritePoints = new HashSet[Node] //list of all transactionMem write port nodes(including write addr, write data, write en)
  val tMemReadDatas = new HashSet[Node] //list of all transactionMem read port nodes(including read addr, read data, read en)
  val tMemReadAddrs = new HashSet[Node]
  val inputNodes = new HashSet[Node] //list of all module Input Nodes
  val outputNodes = new HashSet[Node] //list of all module Output Nodes
  val variableLatencyUnitInputs = new HashSet[Node]// list of all variable latency unit input nodes
  val variableLatencyUnitOutputs = new HashSet[Node]// list of all variable latency unit output nodes

  val regRAWHazards = new HashMap[(Reg, Int, Int), Bool] //map of (register, updatelist num, write stage) -> RAW signal
  val tMemRAWHazards = new HashMap[(FunRdIO[Data], Int, Int), Bool] //map of (tmem readport, writeport number, write stage) -> RAW signal 
  
  def isSource(node: Node) =  {
    regReadPoints.contains(node) || tMemReadDatas.contains(node) || inputNodes.contains(node) || variableLatencyUnitOutputs.contains(node)
  }

  def isSink(node: Node) = {
    regWritePoints.contains(node) || tMemReadAddrs.contains(node) || tMemWritePoints.contains(node) || outputNodes.contains(node) || variableLatencyUnitInputs.contains(node)
  }

  def sourceNodes(): HashSet[Node] = {
    regReadPoints | tMemReadDatas | inputNodes |  variableLatencyUnitOutputs
  }
  def sinkNodes(): HashSet[Node] = {
    regWritePoints | tMemReadAddrs | tMemWritePoints | outputNodes | variableLatencyUnitInputs
  }
  def setPipelineLength(length: Int) = {
    pipelineLength = length
    for (i <- 0 until pipelineLength - 1) {
      pipelineReg += (i -> new ArrayBuffer[Reg]())
    }
    for (i <- 0 until pipelineLength) {
      val valid = Bool()
      stageValids += valid
      valid.nameIt("PipeStage_Valid_" + i)
    }
    for (i <- 0 until pipelineLength) {
      val stall = Bool()
      stageStalls += stall
      stall.nameIt("PipeStage_Stall_" + i)
    }
    for (i <- 0 until pipelineLength) {
      val kill = Bool()
      stageKills += kill
      kill.nameIt("PipeStage_Kill_" + i)
    }
  }
  
  def setNumStages(x: Int) = {
    for (i <- 0 until x-1) {
      pipeline += (i -> new ArrayBuffer[(Node, Bits)]())
      pipelineReg += (i -> new ArrayBuffer[Reg]())
    }
    for (i <- 0 until x) {
      stalls += (i -> new ArrayBuffer[Bool])
      kills += (i -> new ArrayBuffer[Bool])
      speckills += (i -> new ArrayBuffer[Bool])
    }
  }
  
  def addPipeReg(stage: Int, n: Node, rst: Bits) = {//insert pipeline registers manually
    pipeline(stage) += (n -> rst)
  }
  def setStage(n: Node, s:Int) = {//insert pipeline registers by annotating nodes with stages
    Predef.assert(!annotatedStages.contains(n), n.name + " already annotated as stage " + annotatedStages(n))
    if(n.isInstanceOf[Data] && n.asInstanceOf[Data].comp != null){
      annotatedStages(n.asInstanceOf[Data].comp) = s
    } else {
      annotatedStages(n) = s
    }
  }
  def setRegReadStage(node: Node, stage: Int) = {
    val reg = node.asInstanceOf[Data].comp.asInstanceOf[Reg]
    Predef.assert(!(annotatedStages.contains(reg) && annotatedStages(reg) != stage), reg.name + " already annotated as stage " + annotatedStages(reg))
    annotatedStages(reg) = stage
  }
  def setRegWriteStage(node: Node, stage: Int) = {
    val reg = node.asInstanceOf[Data].comp.asInstanceOf[Reg]
    for(producer <- reg.getProducers()){
      Predef.assert(!(annotatedStages.contains(producer) && annotatedStages(producer) != stage), producer.name + " already annotated as stage " + annotatedStages(producer))
      annotatedStages(producer) = stage
    }
  }
  def setTmemReadStage(tmem: TransactionMem[_], stage: Int) = {
    for(i <- 0 until tmem.io.reads.length){
      val readPoint = tmem.io.reads(i)
      val readAddr = readPoint.adr.asInstanceOf[Node]
      val readData = readPoint.dat.asInstanceOf[Node]
      Predef.assert(!(annotatedStages.contains(readAddr) && annotatedStages(readAddr) != stage), readAddr.name + " already annotated as stage " + annotatedStages(readAddr))
      annotatedStages(readAddr) = stage
      Predef.assert(!(annotatedStages.contains(readData) && annotatedStages(readData) != stage), readData.name + " already annotated as stage " + annotatedStages(readData))
      annotatedStages(readData) = stage
    }
  }
  def setTmemWriteStage(tmem: TransactionMem[_], stage: Int) = {
    for(i <- 0 until tmem.io.writes.length){
      val writePoint = tmem.io.writes(i)
      val writeAddr = writePoint.actualWaddr
      val writeEn = writePoint.actualWen
      val writeData = writePoint.actualWdata
      Predef.assert(!(annotatedStages.contains(writeAddr) && annotatedStages(writeAddr) != stage), writeAddr.name + " already annotated as stage " + annotatedStages(writeAddr))
      annotatedStages(writeAddr) = stage
      Predef.assert(!(annotatedStages.contains(writeData) && annotatedStages(writeData) != stage), writeData.name + " already annotated as stage " + annotatedStages(writeData))
      annotatedStages(writeData) = stage
      Predef.assert(!(annotatedStages.contains(writeEn) && annotatedStages(writeEn) != stage), writeEn.name + " already annotated as stage " + annotatedStages(writeEn))
      annotatedStages(writeEn) = stage
    }
  }
  def setInputStage(stage: Int) = {
    val inputs = pipelineComponent.io.flatten.filter(_._2.dir == INPUT).map(_._2)
    for(input <- inputs){
      Predef.assert(!annotatedStages.contains(input), input.name + " already annotated as stage " + annotatedStages(input))
      annotatedStages(input) = stage
    }
  }
  def setOutputStage(stage: Int) = {
    val outputs = pipelineComponent.io.flatten.filter(_._2.dir == OUTPUT).map(_._2)
    for(output <- outputs){
      Predef.assert(!annotatedStages.contains(output), output.name + " already annotated as stage " + annotatedStages(output))
      annotatedStages(output) = stage
    }
  }
  def setVariableLatencyUnitStage(varComp: TransactionalComponent, stage: Int) = {
    annotatedStages(varComp.io.req.valid) = stage
    annotatedStages(varComp.io.req.bits) = stage
    annotatedStages(varComp.resp_ready) = stage
    annotatedStages(varComp.io.resp) = stage
    annotatedStages(varComp.resp_valid) = stage
    annotatedStages(varComp.req_ready) = stage
  }

  def addForwardedReg(d: Reg) = {
    forwardedRegs += d
  }
  def addForwardedMemReadPort(m: TransactionMem[_], r: FunRdIO[_]) = {
    forwardedMemReadPorts += ((m.asInstanceOf[TransactionMem[Data]], r.asInstanceOf[FunRdIO[Data]]))
  }
  def addNonForwardedMemWritePoint[T <: Data] (writePoint: FunWrIO[T]) = {
    memNonForwardedWritePoints += writePoint
  }

  def speculate(s: Bits, v: Bits) = {//need to deprecate
    speculation += ((s, v))
  }
  
  def speculate(reg: Reg, specValue: Data) = {
    speculatedRegs += ((reg, specValue))
  }
  
  //we should not propagate stage numbers to literals, reset signals, and clock signals

  def requireStage(node: Node): Boolean = {
    return requireStageSet.contains(node)
  }
  
  def getStage(n: Node): Int = {
    if (nodeToStageMap.contains(n))
      return nodeToStageMap(n)
    else if(!requireStage(n))
      return -1
    else
      Predef.assert(false, "node does not have astage number: " + n)
      return -2
  }
  //end automatic pipeline stuff
}


/* ----- RULES FOR CLOCKS AND RESETS -----
   ( + ) clock parameter
         ( + ) by default, use parent's clock
         ( + ) sets the default clock domain for all Delay nodes within scope
         ( + ) overriden if Delay specifies its own clock
   ( + ) reset parameter
         ( + ) sets the default reset signal
         ( + ) overriden if Delay specifies its own clock w/ reset != implicitReset
*/
abstract class Module(var clock: Clock = null, private var _reset: Bool = null) {
  /** A backend(Backend.scala) might generate multiple module source code
    from one Module, based on the parameters to instanciate the component
    instance. Since we do not want to blindly generate one module per instance
    the backend will keep a cache of each module's implementation source code
    and discard textual duplicates. By walking the nodes from level zero
    (leafs) to level N (root), we are guarenteed to generate all
    Module/modules source text before their first instantiation. */
  var level = 0;
  var traversal = 0;
  var ioVal: Data = null;
  /** Name of the instance. */
  var name: String = "";
  /** Name of the module this component generates (defaults to class name). */
  var moduleName: String = "";
  var named = false;
  val bindings = new ArrayBuffer[Binding];
  var wiresCache: Array[(String, Bits)] = null;
  var parent: Module = null;
  var containsReg = false;
  val children = new ArrayBuffer[Module];
  val debugs = HashSet[Node]();

  val nodes = new ArrayBuffer[Node]
  val mods  = new ArrayBuffer[Node];
  val omods = new ArrayBuffer[Node];

  val regs  = new ArrayBuffer[Reg];
  val nexts = new ScalaQueue[Node];
  var nindex = -1;
  var defaultWidth = 32;
  var pathParent: Module = null;
  var verilog_parameters = "";
  val clocks = new ArrayBuffer[Clock]
  val resets = new HashMap[Bool, Bool]

  def hasReset = !(reset == null)
  def hasClock = !(clock == null)

  components += this;
  push(this);

  var hasExplicitClock = !(clock == null)
  var hasExplicitReset = !(_reset == null)

  var defaultResetPin: Bool = null
  def reset = {
    if (defaultResetPin == null) {
      defaultResetPin = Bool(INPUT)
      defaultResetPin.isIo = true
      defaultResetPin.component = this
      defaultResetPin.setName("reset")
    }
    defaultResetPin
  }
  def reset_=(r: Bool) {
    _reset = r
  }
  def reset_=() {
    _reset = parent._reset
  }

  override def toString = this.getClass.toString

  //true if this is a subclass of x
  def isSubclassOf(x: java.lang.Class[_]): Boolean = {
    var className: java.lang.Class[_] = this.getClass;
    while(className.toString != x.toString){
      if(className.toString == "class Chisel.Module") return false;
      className = className.getSuperclass;
    }
    return true;
  }

  def depthString(depth: Int): String = {
    var res = "";
    for (i <- 0 until depth)
      res += "  ";
    res
  }

  // This function sets the IO's component.
  def ownIo() {
    val wires = io.flatten;
    for ((n, w) <- wires) {
      // This assert is a sanity check to make sure static resolution
      // of IOs didn't fail
      scala.Predef.assert(this == w.component,
        ChiselError.error("Statically resolved component differs from dynamically resolved component of IO: " + w + " crashing compiler"))
    }
  }

  def findBinding(m: Node): Binding = {
    for (b <- bindings) {
      if (b.inputs(0) == m) {
        return b
      }
    }
    null
  }

  def io: Data
  def nextIndex : Int = { nindex = nindex + 1; nindex }

  var isWalking = new HashSet[Node];
  var isWalked = new HashSet[Node];
  // override def toString: String = name this one isn't really working...
  def wires: Array[(String, Bits)] = {
    // if (wiresCache == null) {
    //   wiresCache = io.flatten;
    // }
    // wiresCache
    io.flatten
  }

  /** Add an assertion in the code generated by a backend. */
  def assert(cond: Bool, message: String): Unit = {
    debug(new Assert(cond || this.reset, message))
  }

  /** Insures a backend does not remove a signal because it is unreachable
    from the outputs. */
  def debug(x: Node): Unit = {
    // XXX Because We cannot guarentee x is flatten later on in collectComp.
    x.getNode.component = this
    debugs += x.getNode
  }

  def printf(message: String, args: Node*): Unit = {
    val p = new Printf(conds.top && !this.reset, message, args)
    printfs += p
    debug(p)
    p.inputs.foreach(debug _)
  }

  def <>(src: Module) {
    io <> src.io
  }

  def apply(name: String): Data = io(name);
  // COMPILATION OF REFERENCE
  def emitDec(b: Backend): String = {
    var res = "";
    val wires = io.flatten;
    for ((n, w) <- wires)
      res += b.emitDec(w);
    res
  }

  def addResetPin(reset: Bool) {
    if (!this.resets.contains(reset)) {
      val pin = 
        if (reset == _reset) {
          this.reset
        } else {
          val res = Bool(INPUT)
          res.isIo = true
          res.component = this
          res
        }
      this.resets += (reset -> pin)
    }
  }

  def addClock(clock: Clock) {
    if (!this.clocks.contains(clock))
      this.clocks += clock
  }

  // returns the pin connected to the reset signal, creates a new one if 
  // no such pin exists
  def getResetPin(reset: Bool): Bool = {
    addResetPin(reset)
    resets(reset)
  }

  // COMPILATION OF BODY
  def isInferenceTerminal(m: Node): Boolean = {
    m.isFixedWidth || (
      m match {
        case io: Bits => io.dir != null;
        case b: Binding => true;
        case _ => false }
    )
    /*
    var isAllKnown = true;
    for (i <- m.inputs) {
      if (i.width == -1)
        isAllKnown = false;
    }
    isAllKnown
    */
  }

  def initializeBFS: ScalaQueue[Node] = {
    val res = new ScalaQueue[Node]

    for (c <- Module.components; a <- c.debugs)
      res.enqueue(a)
    for(b <- Module.blackboxes)
      res.enqueue(b.io)
    for(c <- Module.components) {
      for((n, io) <- c.io.flatten)
        res.enqueue(io)
      // edited by Donggyu
      // TODO: side effects?
      for(reset <- resets.values)
        res.enqueue(reset)
    }

    res
  }

  def initializeDFS: Stack[Node] = {
    val res = new Stack[Node]

    /* XXX Make sure roots are consistent between initializeBFS, initializeDFS
     and findRoots.
     */
    for( a <- this.debugs ) {
      res.push(a)
    }
    for((n, flat) <- this.io.flatten) {
      res.push(flat)
    }
    res
  }

  def bfs(visit: Node => Unit): Unit = {
    val walked = new HashSet[Node]
    val bfsQueue = initializeBFS

    // conduct bfs to find all reachable nodes
    while(!bfsQueue.isEmpty){
      val top = bfsQueue.dequeue
      walked += top
      visit(top)
      for(i <- top.inputs) {
        if(!(i == null)) {
          if(!walked.contains(i)) {
            bfsQueue.enqueue(i)
            walked += i
          }
        }
      }
    }
  }

  def inferAll(): Int = {
    val nodesList = ArrayBuffer[Node]()
    bfs { nodesList += _ }

    def verify {
      var hasError = false
      for (elm <- nodesList) {
        if (elm.infer || elm.width == -1) {
          ChiselError.error("Could not infer the width on: " + elm)
          hasError = true
        }
      }
      if (hasError) throw new Exception("Could not elaborate code due to uninferred width(s)")
    }

    var count = 0
    // Infer all node widths by propagating known widths
    // in a bellman-ford fashion.
    for(i <- 0 until nodesList.length) {
      var nbUpdates = 0
      var done = true;
      for(elm <- nodesList){
        val updated = elm.infer
        if( updated ) { nbUpdates = nbUpdates + 1  }
        done = done && !updated
      }

      count += 1

      if(done){
        verify
        return count;
      }
    }
    verify
    count
  }

  /** All classes inherited from Data are used to add type information
   and do not represent logic itself. */
  def removeTypeNodes(): Int = {
    var count = 0
    bfs {x =>
      scala.Predef.assert(!x.isTypeNode)
      count += 1
      for (i <- 0 until x.inputs.length)
        if (x.inputs(i) != null && x.inputs(i).isTypeNode) {
          x.inputs(i) = x.inputs(i).getNode
        }
    }
    count
  }

  def forceMatchingWidths {
    bfs(_.forceMatchingWidths)
  }

  def addDefaultReset {
    if (!(defaultResetPin == null)) {
      addResetPin(_reset)
      if (this != topComponent && hasExplicitReset)
        defaultResetPin.inputs += _reset
    }
  }

  // for every reachable delay element
  // assign it a clock and reset where
  // clock is chosen to be the component's clock if delay does not specify a clock
  // reset is chosen to be 
  //          component's explicit reset
  //          delay's explicit clock's reset
  //          component's clock's reset
  def addClockAndReset {
    bfs {x => 
      {
        if (x.isInstanceOf[Delay]) {
          val clock = if (x.clock == null) x.component.clock else x.clock
          if (x.isInstanceOf[Reg] && x.asInstanceOf[Reg].isReset ||
              x.isInstanceOf[Mem[ _ ]] && !Module.isInlineMem) { // assign resets to regs
            val reset = 
              if (x.component.hasExplicitReset)
                x.component._reset
              else if (x.clock != null)
                x.clock.getReset
              else if (x.component.hasExplicitClock)
                x.component.clock.getReset
              else
                x.component._reset
            x.inputs += x.component.getResetPin(reset)
          }
          x.clock = clock
          if (x.isInstanceOf[Mem[ _ ]])
            for (i <- x.inputs)
              if (i.isInstanceOf[MemWrite]) i.clock = clock
          x.component.addClock(clock)
        }
      }
    }
  }

  def findConsumers() {
    for (m <- mods) {
      m.addConsumers;
    }
  }

  /** Since we are relying on the out-degree of nodes (i.e. consumers.length),
    this method should only be called after the forward edges have been
    constructed. */
  def findRoots(): ArrayBuffer[Node] = {
    val roots = new ArrayBuffer[Node];
    for (c <- Module.components) {
      roots ++= c.debugs
    }
    for (b <- Module.blackboxes)
      roots += b.io;
    for (m <- mods) {
      m match {
        case io: Bits => {
          if (io.dir == OUTPUT) {
            if (io.consumers.length == 0) roots += m;
          }
        }
        case d: Delay => roots += m;
        case any      =>
      }
    }
    roots
  }

  def visitNodes(roots: Array[Node]) {
    val stack = new Stack[(Int, Node)]();
    for (root <- roots) {
      stack.push((0, root));
    }
    isWalked.clear();
    while (stack.length > 0) {
      val (newDepth, node) = stack.pop();
      val comp = node.componentOf;
      if (newDepth == -1) {
        comp.omods += node;
      } else {
        node.depth = max(node.depth, newDepth);
        if (!comp.isWalked.contains(node)) {
          comp.isWalked += node;
          node.walked = true;
          stack.push((-1, node));
          for (i <- node.inputs) {
            if (i != null) {
              i match {
                case d: Delay       => ;
                case o              => stack.push((newDepth + 1, o));
              }
            }
          }
        }
      }
    }
  }

  def findOrdering(): Unit = visitNodes(findRoots().toArray);

  def findGraphDims(): (Int, Int, Int) = {
    var maxDepth = 0;
    val imods = new ArrayBuffer[Node]();
    for (m <- mods) {
      m match {
        case l: Literal =>
        case i      => imods += m;
      }
    }
    val whist = new HashMap[Int, Int]();
    for (m <- imods) {
      val w = m.width;
      if (whist.contains(w)) {
        whist(w) = whist(w) + 1;
      } else {
        whist(w) = 1;
      }
    }
    val hist = new HashMap[String, Int]();
    for (m <- imods) {
      var name = m.getClass().getName();
      m match {
        case m: Mux => name = "Mux";
        case op: Op => name = op.op;
        case o      => name = name.substring(name.indexOf('.') + 1);
      }
      if (hist.contains(name)) {
        hist(name) = hist(name) + 1;
      } else {
        hist(name) = 1;
      }
    }
    for (m <- imods)
      maxDepth = max(m.depth, maxDepth);
    // for ((n, c) <- hist)
    ChiselError.info("%6s: %s".format("name", "count"));
    for (n <- hist.keys.toList.sortWith((a, b) => a < b))
      ChiselError.info("%6s: %4d".format(n, hist(n)));
    ChiselError.info("%6s: %s".format("width", "count"));
    for (w <- whist.keys.toList.sortWith((a, b) => a < b))
      ChiselError.info("%3d: %4d".format(w, whist(w)));
    var widths = new Array[Int](maxDepth + 1);
    for (i <- 0 until maxDepth + 1)
      widths(i) = 0;
    for (m <- imods)
      widths(m.depth) = widths(m.depth) + 1;
    var numNodes = 0;
    for (m <- imods)
      numNodes += 1;
    var maxWidth = 0;
    for (i <- 0 until maxDepth + 1)
      maxWidth = max(maxWidth, widths(i));
    (numNodes, maxWidth, maxDepth)
  }

  def collectNodes(c: Module) {
    for (m <- c.mods) {
      m match {
/* XXX deprecated?
        case io: Bits  =>
          if (io.dir == INPUT) {
            inputs += m;
          } else if (io.dir == OUTPUT) {
            outputs += m;
          }
 */
        case r: Reg    => regs += r;
        case other     =>
      }
    }
  }

  def traceableNodes: Array[Node] = io.traceableNodes;

  /** Returns true if this module or any of its children contains
    at least one register. */
  def containsRegInTree: Boolean = {
    if( containsReg ) {
      true
    } else {
      for(child <- children){
        if( child.containsRegInTree ) return true
      }
      false
    }
  }

  // 1) name the component
  // 2) name the IO
  // 3) name and set the component of all statically declared nodes through introspection
  /* XXX deprecated. make sure containsReg and isClk are set properly. */
  def markComponent() {
    ownIo();
    /* We are going through all declarations, which can return Nodes,
     ArrayBuffer[Node], Cell, BlackBox and Modules.
     Since we call invoke() to get a proper instance of the correct type,
     we have to insure the method is accessible, thus all fields
     that will generate C++ or Verilog code must be made public. */
     for (m <- getClass().getDeclaredMethods) {
       val name = m.getName();
       val types = m.getParameterTypes();
       if (types.length == 0
        && isPublic(m.getModifiers()) && !(Module.keywords contains name)) {
         val o = m.invoke(this);
         o match {
         case node: Node => {
           if (node.isReg || node.isClkInput) containsReg = true;
         }
         case buf: ArrayBuffer[_] => {
           /* We would prefer to match for ArrayBuffer[Node] but that's
            impossible because of JVM constraints which lead to type erasure.
            XXX Using Seq instead of ArrayBuffer will pick up members defined
            in Module that are solely there for implementation purposes. */
           if(!buf.isEmpty && buf.head.isInstanceOf[Node]){
             val nodebuf = buf.asInstanceOf[Seq[Node]];
             for(elm <- nodebuf){
               if (elm.isReg || elm.isClkInput) {
                 containsReg = true;
               }
             }
           }
         }
         case cell: Cell => {
           if(cell.isReg) containsReg = true;
         }
         case bb: BlackBox => {
           bb.pathParent = this;
           for((n, elm) <- io.flatten) {
             if (elm.isClkInput) containsReg = true
           }
         }
         case comp: Module => {
           comp.pathParent = this;
         }
         case any =>
       }
     }
     }
  }


  def genAllMuxes {
    for (p <- procs) {
      p match {
        case b: Bits  => if(b.updates.length > 0) b.genMuxes(b.default);
        case r: Reg  => r.genMuxes(r);
        case mw: MemWrite =>
        case mw: PutativeMemWrite =>
        case e: Extract =>
        case v: VecProc =>
      }
    }
  }

  def verifyAllMuxes {
    for(m <- Module.muxes) {
      if(m.inputs(0).width != 1 && m.component != null && (!Module.isEmittingComponents || !m.component.isInstanceOf[BlackBox])) {
        ChiselError.error({"Mux " + m.name + " has " + m.inputs(0).width + "-bit selector " + m.inputs(0).name}, m.line);
      }
    }
  }
  /* XXX Not sure what the two following do.
   They never get overridden yet it is called
   for each component (See Backend implementations). */
  def elaborate(fake: Int = 0) {}
  def postMarkNet(fake: Int = 0) {}
  def stripComponent(s: String): String = s.split("__").last

    /** Returns the absolute path to a component instance from toplevel. */
  def getPathName: String = {
    if ( parent == null ) name else parent.getPathName + "_" + name;
  }

  def traceNodes() {
    val queue = Stack[() => Any]();

    /* XXX Why do we do something different here? */
    if (!Module.backend.isInstanceOf[VerilogBackend]) {
      queue.push(() => io.traceNode(this, queue));
    } else {
      for (c <- Module.components) {
        queue.push(() => c.io.traceNode(c, queue))
      }
    }
    for (c <- Module.components) {
        if (!(c.defaultResetPin == null)) { // must manually add reset pin cuz it isn't part of io
          queue.push(() => c.defaultResetPin.traceNode(c, queue))
        }
    }
    for (c <- Module.components; d <- c.debugs)
      queue.push(() => d.traceNode(c, queue))
    for (b <- Module.blackboxes)
      queue.push(() => b.io.traceNode(this, queue));
    while (queue.length > 0) {
      val work = queue.pop();
      work();
    }
  }

  def findCombLoop() {
    // Tarjan's strongly connected components algorithm to find loops
    var sccIndex = 0
    val stack = new Stack[Node]
    val sccList = new ArrayBuffer[ArrayBuffer[Node]]

    def tarjanSCC(n: Node): Unit = {
      if(n.isInstanceOf[Delay]) throw new Exception("trying to DFS on a register")

      n.sccIndex = sccIndex
      n.sccLowlink = sccIndex
      sccIndex += 1
      stack.push(n)

      for(i <- n.inputs) {
        if(!(i == null) && !i.isInstanceOf[Delay] && !i.isReg) {
          if(i.sccIndex == -1) {
            tarjanSCC(i)
            n.sccLowlink = min(n.sccLowlink, i.sccLowlink)
          } else if(stack.contains(i)) {
            n.sccLowlink = min(n.sccLowlink, i.sccIndex)
          }
        }
      }

      if(n.sccLowlink == n.sccIndex) {
        val scc = new ArrayBuffer[Node]

        var top: Node = null
        do {
          top = stack.pop()
          scc += top
        } while (!(n == top))
        sccList += scc
      }
    }

    bfs { node =>
      if(node.sccIndex == -1 && !node.isInstanceOf[Delay] && !(node.isReg)) {
        tarjanSCC(node)
      }
    }

    // check for combinational loops
    var containsCombPath = false
    for (nodelist <- sccList) {
      if(nodelist.length > 1) {
        containsCombPath = true
        ChiselError.error("FOUND COMBINATIONAL PATH!")
        for((node, ind) <- nodelist zip nodelist.indices) {
          val ste = node.line
          ChiselError.error("  (" + ind +  ") on line " + ste.getLineNumber +
                                  " in class " + ste.getClassName +
                                  " in file " + ste.getFileName +
                                  ", " + node.name)
        }
      }
    }
  }

  def isInput(node: Node): Boolean =
    node match { case b:Bits => b.dir == INPUT; case o => false }
  def keepInputs(nodes: Seq[Node]): Seq[Node] =
    nodes.filter(isInput)
  def removeInputs(nodes: Seq[Node]): Seq[Node] =
    nodes.filter(n => !isInput(n))
    
  //automatic pipelining stuff

  def getConsumers() = {
    val map = new HashMap[Node, ArrayBuffer[Node]]

    def getConsumer(node: Node) = {
      for (i <- node.inputs) {
        if (!map.contains(i)) {
          map += (i -> new ArrayBuffer[Node])
        }
        //if(!map(i).contains(node))
        map(i) += node
      }
    }

    bfs(getConsumer(_))
    map
  }
  
  def gatherSpecialComponents() = {
    println("gathering special pipelining components")
    ArchitecturalRegs = new ArrayBuffer[Reg]
    TransactionMems = new ArrayBuffer[TransactionMem[Data]]
    VariableLatencyComponents = new ArrayBuffer[TransactionalComponent]()
    //mark architectural registers
    for(node <- pipelineComponent.nodes){
      if (node.isInstanceOf[Reg] && !isPipeLineReg(node)) ArchitecturalRegs += node.asInstanceOf[Reg]
    }
    //mark TransactionMems
    for(component <- pipelineComponent.children){
      if(component.isInstanceOf[TransactionMem[Data]]){
        TransactionMems += component.asInstanceOf[TransactionMem[Data]]
      }
    }
    //mark variable latency modules
    for(component <- pipelineComponent.children){
      if(component.isInstanceOf[TransactionalComponent]){
        VariableLatencyComponents += component.asInstanceOf[TransactionalComponent]
	variableLatencyUnitInputs += component.asInstanceOf[TransactionalComponent].io.req.valid
	variableLatencyUnitInputs += component.asInstanceOf[TransactionalComponent].io.req.bits
	variableLatencyUnitInputs += component.asInstanceOf[TransactionalComponent].resp_ready
	variableLatencyUnitOutputs += component.asInstanceOf[TransactionalComponent].io.resp
	variableLatencyUnitOutputs += component.asInstanceOf[TransactionalComponent].resp_valid
	variableLatencyUnitOutputs += component.asInstanceOf[TransactionalComponent].req_ready
      }
    }

    //mark read and write points for regs
    for (reg <- ArchitecturalRegs) {
      regReadPoints += reg
      for(i <- reg.getProducers){
        regWritePoints += i
      }
    }
    //mark read and write ports for transactionMems
    for(tmem <- TransactionMems){
      for(i <- 0 until tmem.readPortNum){
        tMemReadAddrs += tmem.io.reads(i).adr
        tMemReadDatas += tmem.io.reads(i).dat
      }
      for(i <- 0 until tmem.virtWritePortNum){
        tMemWritePoints += tmem.io.writes(i).adr
        tMemWritePoints += tmem.io.writes(i).dat
        tMemWritePoints += tmem.io.writes(i).is
      }
    }
    //find module IO nodes
    val inputs = pipelineComponent.io.flatten.filter(_._2.dir == INPUT)
    val outputs = pipelineComponent.io.flatten.filter(_._2.dir == OUTPUT)
    for(input <- inputs){
      inputNodes += input._2
    }
    for(output <- outputs){
      outputNodes += output._2
    }
  }
  
  def findNodesRequireStage() = {
    //mark nodes reachable from sourceNodes through the consumer pointer as requiring a stage number
    val bfsQueue = new ScalaQueue[Node]
    val visited = new HashSet[Node]
    for(node <- sourceNodes()){
      bfsQueue.enqueue(node)
    }
    while(!bfsQueue.isEmpty){
      val currentNode = bfsQueue.dequeue
      if(!isSink(currentNode)){
        for(child <- currentNode.consumers){
          if(!visited.contains(child) && !isSink(child)){
            bfsQueue.enqueue(child)
          }
        }
        requireStageSet += currentNode
      }
      visited +=currentNode
    }

    //mark all source and sink nodes as requiring a stage number(need to do this separately because not all of them are reachable by following consumers pointers
    for(node <- sourceNodes()){
      requireStageSet += node
    }
    for(node <- sinkNodes()){
      requireStageSet += node
    }
  }

  def insertPipelineRegisters2() = { 
    setPipelineLength(annotatedStages.map(_._2).max + 1)
    
    //insert filler wires for easier stage propagation. ie make sure that there is a non-shared wire node between a combinational logic node and each of its inputs and consumers. Each filler node is gaurenteed to have only 1 producer and 1 consumer. This way we make sure that the pipeline boundary can always lie on these wire nodes, ensuring that we get a legal pipelining
    insertFillerWires()
    
    findNodesRequireStage()
    
    println("finding valid pipeline register placement")
    val coloredNodes = propagateStages()
    verifyLegalStageColoring(coloredNodes)
    
    println("optimizing pipeline register placement")
    optimizeRegisterPlacement(coloredNodes)
    verifyLegalStageColoring(coloredNodes)
    
    println("inserting pipeline registers")
    
    /*
    var maxStage = 0
    for((node, stages) <- coloredNodes){
      if(stages.length > 0 && stages.max > maxStage){
        maxStage = stages.max
      }
    }
    setPipelineLength(maxStage + 1)
    */

    var counter = 0//hack to get unique register names with out anything nodes being named already
    nodeToStageMap = new HashMap[Node, Int]()
    for((node,stgs) <- coloredNodes){
      Predef.assert(stgs.length <= 2, stgs)
      if(stgs.length > 1){
        val stageDifference = Math.abs(stgs(1) - stgs(0))
        Predef.assert(stageDifference > 0, stageDifference)
        Predef.assert(!node.isInstanceOf[Op])
        var currentNodeOut = node
        nodeToStageMap(currentNodeOut) = Math.min(stgs(0),stgs(1))
        for(i <- 0 until stageDifference){
          println("inserted register on " + currentNodeOut.name)
          currentNodeOut = insertRegister(currentNodeOut.asInstanceOf[Bits], Bits(0), "Stage_" + (Math.min(stgs(0),stgs(1)) + i) + "_" + "PipeReg_"+ currentNodeOut.name + counter)
          nodeToStageMap(currentNodeOut) = Math.min(stgs(0),stgs(1)) + i + 1
          nodeToStageMap(currentNodeOut.asInstanceOf[Bits].comp) = Math.min(stgs(0),stgs(1)) + i + 1
          counter = counter + 1
          pipelineReg(Math.min(stgs(0),stgs(1)) + i) += currentNodeOut.asInstanceOf[Bits].comp.asInstanceOf[Reg]
        }
      } else if(stgs.length == 1){
        nodeToStageMap(node) = stgs(0)
      }
    }
  }
  
  
  //place extra wire nodes between all nodes
  def insertFillerWires() = {
    val bfsQueue = new ScalaQueue[Node]
    val visited = new HashSet[Node]
    for(node <- sourceNodes()){
      bfsQueue.enqueue(node)
    }
    while(!bfsQueue.isEmpty){
      val currentNode = bfsQueue.dequeue
      if(!visited.contains(currentNode)){
        for(child <- currentNode.consumers){
          if(!visited.contains(child) && !isSink(child)){
            bfsQueue.enqueue(child)
          }
        }

        visited += currentNode
        fillerNodes ++= insertBitsOnOutputs(currentNode)
      }
    }
  }
  
  def propagateStages() = {
    val bfsQueue = new ScalaQueue[(Node, Boolean)] //the 2nd item of the tuple indicateds which direction the node should propagate its stage number to. True means propagate to outputs, false means propagate to inputs
    val retryNodes = new ArrayBuffer[(Node, Boolean)] //list of nodes that need to retry their stage propagation because their children weren't ready
    
    val coloredNodes = new HashMap[Node, ArrayBuffer[Int]] //map of node to stage numbers to be returned
    var oldColoredNodes = new HashMap[Node, ArrayBuffer[Int]] //remember old colored nodes so that we can see if the algorithm is making progress
    
    //initialize empty array buffer for all nodes in coloredNodes
    def initializeColoredNodes() ={
      for(node <- requireStageSet){
        coloredNodes(node) = new ArrayBuffer[Int]
      }
    }
    
  
    def propagatedToChild(parentStage: Int, child: Node) : Boolean = {
      if(requireStage(child)){
        if(!coloredNodes.contains(child)){
          return false
        } else {
          return coloredNodes(child).contains(parentStage)
        }
      }
      return true
    }
    
    def propagatedToAllChildren(node: Node, stage: Int, direction: Boolean) : Boolean = {
      var children = node.getProducers
      if(direction){
        children = node.consumers
      }
      var result = true
      for(child <- children){
        result = result && propagatedToChild(stage, child)
      }
      result
    }
    
    def propagateToChildren(node: Node, direction: Boolean) = {
      val propagatedChildren = new ArrayBuffer[Node] 
      
      //determine if we need/can propagate to child and return the stage number that should be propagated to child; direction = false means child is producer of node, direction = true means child is consumer of node
      def childEligibleForPropagation(child: Node, direction: Boolean): Boolean ={
        val childWasPropagated = propagatedToChild(coloredNodes(node)(0), child)
        val fillerToReal = fillerNodes.contains(node) && !fillerNodes.contains(child) && coloredNodes(child).length > 0
        var allParentsResolved = true
        var childParents:Seq[Node] = null
        if(direction){
          childParents = child.getProducers() 
        } else {
          childParents = child.consumers
        }
        var edgeParentStage:Int = 0//this is the minimum stage of child's consumers when direction == true, this is the maximum stage of child's producers when direction == false
        if(direction){
          edgeParentStage = 0
        } else {
          edgeParentStage = Int.MaxValue
        }

        for(parent <- childParents.filter(requireStage(_))){
          if(coloredNodes(parent).length == 0){
            allParentsResolved = false
          } else {
            if(direction){
              if(coloredNodes(parent).max > edgeParentStage){
                edgeParentStage = coloredNodes(parent).max
              }
            } else {
              if(coloredNodes(parent).min < edgeParentStage){
                edgeParentStage = coloredNodes(parent).min
              }
            }
          }
        }
        val childEligible = !childWasPropagated && !fillerToReal && allParentsResolved
        return childEligible
      }
      //propagate node's stage number to child and record all new nodes that have been propagated to; direction = false means child is producer of node, direction = true means child is consumer of node
      def doPropagation(child: Node, direction: Boolean) = {
        var childParents:Seq[Node] = null
        if(direction){
          childParents = child.getProducers()
        } else {
          childParents = child.consumers
        }
        var edgeParentStage:Int = 0//this is the minimum stage of child's consumers when direction == true, this is the maximum stage of child's producers when direction == false
        if(direction){
          edgeParentStage = 0
        } else {
          edgeParentStage = Int.MaxValue
        }

        for(parent <- childParents.filter(requireStage(_))){
          if(direction){
            if(coloredNodes(parent).max > edgeParentStage){
              edgeParentStage = coloredNodes(parent).max
            }
          } else {
            if(coloredNodes(parent).min < edgeParentStage){
              edgeParentStage = coloredNodes(parent).min
            }
          }
        }
        //propagate stage to child
        if(!coloredNodes(child).contains(edgeParentStage)){
          coloredNodes(child) += edgeParentStage
        }
        //propagate child stage back to its parents
        for(parent <- childParents.filter(requireStage(_))){
          if(!coloredNodes(parent).contains(edgeParentStage)){
            coloredNodes(parent) += edgeParentStage
          }
        }
        propagatedChildren += child
      }

      var children:Seq[Node] = null
      if(direction){//propagate to consumers
        children = node.consumers
      } else {//propagate to producers
        children = node.getProducers()
      }
      
      for(child <- children) {
        //check if we need/can propagate to child
        if(childEligibleForPropagation(child, direction)){
          //propagate stage to child
          doPropagation(child, direction)
        }
      }
      propagatedChildren
    }
    
    //initializeColoredNodes with empty ArrayBuffers for all nodes
    initializeColoredNodes()

    //initialize bfs queue and coloredNodes with user annotated nodes
    for((node, stage) <- annotatedStages){
      Predef.assert(!(regReadPoints.contains(node) && regWritePoints.contains(node)), "same node marked as both read point and write point")
      if(!isSink(node)){
        retryNodes += ((node, true))
      }
      if(!isSource(node)){
        retryNodes += ((node, false))
      }
      coloredNodes(node) = new ArrayBuffer[Int]()
      coloredNodes(node) += stage
    }
    
    //do stage propagation
    while(!retryNodes.isEmpty && !oldColoredNodes.equals(coloredNodes)){
      for((node, direction) <- retryNodes){
        bfsQueue.enqueue(((node, direction)))
      }
      oldColoredNodes = coloredNodes.clone()
      retryNodes.clear
      while(!bfsQueue.isEmpty){
        val current = bfsQueue.dequeue
        val currentNode = current._1
        val currentDirection = current._2
        if(coloredNodes(currentNode).length < 2){
          var childrenPropagatedTo:ArrayBuffer[Node] = null
          childrenPropagatedTo = propagateToChildren(currentNode, currentDirection)
          for(child <- childrenPropagatedTo){
            if((coloredNodes(child).length < 2) && !isSource(child) && !isSink(child)){
              bfsQueue.enqueue(((child, currentDirection)))
            }
          }
          if(!propagatedToAllChildren(currentNode, coloredNodes(currentNode)(0), currentDirection)){
            retryNodes += ((currentNode, currentDirection))
          }
        }
      }
    }
    //visualizeGraph(coloredNodes, "stages.gv")
    coloredNodes
  }
    
  
  //inserts register between *input* and its consumers
  def insertRegister(input: Bits, init_value: Bits, name: String) : Bits = {
    val new_reg = Reg(Bits())
    new_reg := input
    new_reg.comp.asInstanceOf[Reg].clock = pipelineComponent.clock
    when(pipelineComponent._reset){
      new_reg := init_value
    }
    input.pipelinedVersion = new_reg
    new_reg.unPipelinedVersion = input
    new_reg.comp.setName(name)
    for(i <- 0 until input.consumers.size){
      val consumer = input.consumers(i)
      val producer_index = consumer.inputs.indexOf(input)
      if(producer_index > -1) consumer.inputs(producer_index) = new_reg
      new_reg.consumers += consumer
      input.consumers(i) = new_reg
    }
    new_reg
  }
  
  //inserts additional bit nodes between *input* and its consumers(useful for automatic pipelining)
  def insertBitsOnOutputs(input: Node) : ArrayBuffer[Node] = {
    val newNodes = new ArrayBuffer[Node]()
    for(i <- 0 until input.consumers.size){
      val consumer = input.consumers(i)
      for(j <- 0 until consumer.inputs.size){
        if(consumer.inputs(j) == input){
          val new_bits = Bits()
          newNodes += new_bits
          consumer.inputs(j) = new_bits
          new_bits.consumers += consumer
          new_bits.inputs += input
          pipelineComponent.nodes += new_bits
        } 
      }
    }
    input.consumers.clear()
    for(newNode <- newNodes){
      input.consumers += newNode
    }
    newNodes
  }
  
  //inserts additional bit nodes between *output* and its inputs(useful for automatic pipelining)
  def insertBitsOnInputs(output: Node) : ArrayBuffer[Node] = {
    val newNodes = new ArrayBuffer[Node]()
    for(i <- 0 until output.inputs.size){
      val input = output.inputs(i)
      for(j <- 0 until input.consumers.size){
        if(input.consumers(j) == output){
          val new_bits = Bits()
          newNodes += new_bits
          input.consumers(j) = new_bits
	  output.inputs(i) = new_bits
          new_bits.inputs += input
          new_bits.consumers += output
        } 
      }
    }
    newNodes
  }

  //inserts additional (SINGULAR) bit node between *input* and its consumers(useful for automatic pipelining)
  def insertBitOnOutputs(input: Node) : Node = {
    val new_bits = Bits()
    new_bits.inputs += input
    for(i <- 0 until input.consumers.size){
      val consumer = input.consumers(i)
      for(j <- 0 until consumer.inputs.size){
        if(consumer.inputs(j) == input){
          consumer.inputs(j) = new_bits
          new_bits.consumers += consumer
        } 
      }
    }
    input.consumers.clear()
    input.consumers += new_bits
    new_bits
  }
  
  def optimizeRegisterPlacement(coloredNodes: HashMap[Node, ArrayBuffer[Int]]) = { 
    val nodeArrivalTimes = new HashMap[Node, Double]
    val forwardArrivalTimes = new HashMap[Node, Double]
    val backwardArrivalTimes = new HashMap[Node, Double]

    val stageDelays = ArrayBuffer.fill(pipelineLength)(0.0)
    
    def calculateArrivalTimes() = {
      def findArrivalTime(node: Node, dir: Direction): Double = {
        var arrivalTimes = forwardArrivalTimes
        if(dir == FORWARD){
          arrivalTimes = forwardArrivalTimes
        } else {
          arrivalTimes = backwardArrivalTimes
        }

        if(arrivalTimes.contains(node)){
          return arrivalTimes(node)
        } else if(node.isInstanceOf[Mem[_]]){
          arrivalTimes(node) = 0.0
          return 0.0
        } else if(!requireStage(node)){
          arrivalTimes(node) = 0.0
          return 0.0
        } else if(isSource(node) && dir == FORWARD || isSink(node) && dir == BACKWARD){
          arrivalTimes(node) = 0.0
          return 0.0
        } else if(coloredNodes.contains(node) && coloredNodes(node).length > 1 && (coloredNodes(node)(0) != coloredNodes(node)(1))) {
          arrivalTimes(node) = 0.0
          return 0.0
        } else {
          var arrivalTime :Double= 0.0
          var parents = node.inputs
          if(dir == FORWARD){
            parents = node.inputs
          } else {
            parents = node.consumers
          }

          for(parent <- parents){
            arrivalTime = Math.max(arrivalTime, findArrivalTime(parent, dir))
          }
          arrivalTimes(node) = arrivalTime + node.delay
          return arrivalTime + node.delay
        }
      }
      //find forward delay times
      forwardArrivalTimes.clear()
      for(node <- pipelineComponent.nodes){
        if(coloredNodes.contains(node) && coloredNodes(node).length > 1 && (coloredNodes(node)(0) != coloredNodes(node)(1))){
          for(input <- node.inputs){
            findArrivalTime(input, FORWARD)
          }
        }
      }
      for(node <- sinkNodes()){
        findArrivalTime(node, FORWARD)
      }
      //find backward delay times
      backwardArrivalTimes.clear()
      for(node <- pipelineComponent.nodes){
        if(coloredNodes.contains(node) && coloredNodes(node).length > 1 && (coloredNodes(node)(0) != coloredNodes(node)(1))){
          for(consumer <- node.consumers){
            findArrivalTime(consumer, BACKWARD)
          }
        }
      }
      for(node <- sourceNodes()){
        findArrivalTime(node, BACKWARD)
      }
    }
    
    def findUnpipelinedPathLength(coloredNodes: HashMap[Node, ArrayBuffer[Int]]): Double = {
      val nodeArrivalTimes = new HashMap[Node, Double]
      def calculateArrivalTimes() = {
        def findArrivalTime(node: Node) : Double = {
          if(nodeArrivalTimes.contains(node)){
            return nodeArrivalTimes(node)
          } else if(isSource(node)){
            nodeArrivalTimes(node) = 0.0
            return 0.0
          } else if(node.isInstanceOf[Mem[_]]){
            nodeArrivalTimes(node) = 0.0
            return 0.0
          } else if(!requireStage(node)){
            nodeArrivalTimes(node) = 0.0
            return 0.0
          } else {
            var arrivalTime :Double= 0.0
            for(input <- node.inputs){
              arrivalTime = Math.max(arrivalTime, findArrivalTime(input))
            }
            nodeArrivalTimes(node) = arrivalTime + node.delay
            return arrivalTime + node.delay
          }
        }
        nodeArrivalTimes.clear()
        for(node <- sinkNodes()){
          findArrivalTime(node)
        }
      }
    
      calculateArrivalTimes()
      var maxLength = 0.0
      for(node <- nodeArrivalTimes.keys){
        maxLength = Math.max(maxLength, nodeArrivalTimes(node))
      }
      return maxLength
    }
    
    def findStageDelays() = {
      for(i <- 0 until pipelineLength){
        stageDelays(i) = 0.0
      }
      for((node, stages) <- coloredNodes.filter(_._2.length == 1)){
        /*println("DEBUG0")
        println(node)
        println(stages)
        println(requireStage(node))
        println(isSink(node))
        println(isSource(node))
        visualizeSubGraph(node, "debug.gv", 5)*/ 
        if(forwardArrivalTimes.contains(node)){
          if(forwardArrivalTimes(node) > stageDelays(stages(0))){
            stageDelays(stages(0)) = forwardArrivalTimes(node)
          }
        }
      }
    }

    def movePipelineBoundary(boundaryNum: Int, direction: Direction) = {
      val boundaryNodes = new ArrayBuffer[Node]
      val possibleMoveNodes = new ArrayBuffer[Node]
      val eligibleMoveNodes = new ArrayBuffer[Node]
      
      //find nodes from possibleMoveNodes that can have the pipeline boundary be legally moved in "direction" accross the node and store them in eligibleMoveNodes
      def findEligibleNodes(direction: Direction) = {
        for(node <- possibleMoveNodes){
          if(requireStage(node) && !annotatedStages.contains(node) && !fillerNodes.contains(node)){
            val nodeStage = coloredNodes(node)(0)
            Predef.assert(coloredNodes(node).length == 1, "" + node + " " + coloredNodes(node))
            var parentsEligible = true
            var parents: Seq[Node] = null
            
            if(direction == FORWARD){
              parents = node.inputs
            } else if(direction == BACKWARD){
              parents = node.consumers
            }

            for(parent <- parents){
              if(requireStage(parent)){
                Predef.assert(coloredNodes.contains(parent))
                Predef.assert(coloredNodes(parent).length > 0)
                Predef.assert(coloredNodes(parent).length <= 2)
                if(coloredNodes(parent).length == 1){
                  parentsEligible = false
                } else {
                  Predef.assert(coloredNodes(parent).contains(nodeStage))
                }
              }
            }

            eligibleMoveNodes += node
          }
        }
      }
      //move pipeline boundary in "direction" across "node"
      def moveNode(movedNode: Node, direction: Direction) = {
        val nodeStage = coloredNodes(movedNode)(0)
        var stageDelta = 0
        if(direction == FORWARD){
          stageDelta = -1
        } else {
          stageDelta = 1
        }

        coloredNodes(movedNode)(0) = coloredNodes(movedNode)(0) + stageDelta

        val parents = new ArrayBuffer[Node]
        for(input <- movedNode.inputs){
          parents += input
        }
        for(consumer <- movedNode.consumers){
          parents += consumer
        }
        for(parent <- parents){
          if(requireStage(parent)){
            if(coloredNodes(parent).length == 2){
              if(coloredNodes(parent)(0) == nodeStage){
                coloredNodes(parent)(0) = coloredNodes(parent)(0) + stageDelta
              } else {
                coloredNodes(parent)(1) = coloredNodes(parent)(1) + stageDelta
              }
              if(coloredNodes(parent)(0) == coloredNodes(parent)(1)){
                coloredNodes(parent) -= coloredNodes(parent)(1)
              }
            } else {
              coloredNodes(parent) += coloredNodes(parent)(0) + stageDelta
            }
          }
        }
      }
      //populate boundaryNodes
	    for((node, stages) <- coloredNodes.filter(_._2.length == 2)){
        if(stages.contains(boundaryNum) && stages.contains(boundaryNum + 1)){
          boundaryNodes += node
        } else {
          if(direction == FORWARD){
            if(stages.contains(boundaryNum + 1) && stages.filter(_ != boundaryNum + 1)(0) < boundaryNum + 1){
              boundaryNodes += node
            }
          } else if(direction == BACKWARD) {
            if(stages.contains(boundaryNum) && stages.filter(_ != boundaryNum)(0)  > boundaryNum){
              boundaryNodes += node
            }
          }
        }
      }
      //find nodes that are producers/consumers of the boundary nodes
      if(direction == FORWARD){
        for(node <- boundaryNodes){
          for(consumer <- node.consumers){
            possibleMoveNodes += consumer
          }
        }
      } else if(direction == BACKWARD){
        for(node <- boundaryNodes){
          for(input <- node.inputs){
            possibleMoveNodes += input
          }
        }
      }

      //find nodes that are eligible to be moved accross the pipeline boundary
      findEligibleNodes(direction)
      //find eligible node with the highest dealy
      var criticalNode: Node = null
      var highestDelay = 0.0
      var arrivalTimes = forwardArrivalTimes
      if(direction == FORWARD){
        arrivalTimes = backwardArrivalTimes
      } else {
        arrivalTimes = forwardArrivalTimes
      }
      for(node <- eligibleMoveNodes){
        val nodeDelay = arrivalTimes(node)
        if(nodeDelay >= highestDelay){
          criticalNode = node
          highestDelay = nodeDelay
        }
      }
      if(eligibleMoveNodes.length > 0){
        moveNode(criticalNode, direction)
      } else {
        println("no eligible nodes")
        println(boundaryNum)
        println(direction)
        println(possibleMoveNodes)
        visualizeGraph(coloredNodes, "debug.gv")
        Predef.assert(false)
      }
    }

    var iterCount = 1
    calculateArrivalTimes()
    findStageDelays()
    var oldMaxDelay = findUnpipelinedPathLength(coloredNodes)
    println("max unpipelined path: " + findUnpipelinedPathLength(coloredNodes))
    println("max delay before optimizeation: " + stageDelays.max)
    while(!(iterCount % 100 == 0 && oldMaxDelay == stageDelays.max) && iterCount < 10000){
      //find pipeline stage with longest delay
      val criticalStageNum = stageDelays.indexOf(stageDelays.max)
      //determine which pipeline boundary to move
      for(pipeBoundaryNum <- 0 until pipelineLength - 1){
        calculateArrivalTimes()
        findStageDelays()
        if(stageDelays(pipeBoundaryNum) < stageDelays(pipeBoundaryNum + 1)){
          movePipelineBoundary(pipeBoundaryNum, FORWARD)
        } else if(stageDelays(pipeBoundaryNum) > stageDelays(pipeBoundaryNum + 1)){
          movePipelineBoundary(pipeBoundaryNum, BACKWARD)
        }
      }
      iterCount = iterCount + 1
      if(iterCount % 100 == 0){
        oldMaxDelay = stageDelays.max
      }
    }
    calculateArrivalTimes()
    findStageDelays()
    println(stageDelays)
    println("max delay after optimizeation: " + stageDelays.max)
    println("iteration count: " + iterCount)
    visualizeGraph(coloredNodes, "stages.gv")
    visualizeGraph(forwardArrivalTimes, "fdelays.gv")
    visualizeGraph(backwardArrivalTimes, "bdelays.gv")
  }

  /*
  def optimizeRegisterPlacement(coloredNodes: HashMap[Node, ArrayBuffer[Int]]) = { 
    val lastMovedNodes = new ArrayBuffer[(Node, ArrayBuffer[Int])]//keep track of nodes we just moved so that we can undo the move later
    val nodeArrivalTimes = new HashMap[Node, Double]
    
    var temp = 10000.0
    val coolRate = 0.002
    var iterCount = 0
    
    def acceptProbability(currentEnergy: Double, newEnergy: Double, temp: Double): Double = {
      if(newEnergy <= currentEnergy){
        return 1
      } else {
        return Math.exp((currentEnergy-newEnergy)/temp)
      }
    }

    def calculateArrivalTimes() = {
      def findArrivalTime(node: Node) : Double = {
        if(nodeArrivalTimes.contains(node)){
          return nodeArrivalTimes(node)
        } else if(isSource(node)){
          nodeArrivalTimes(node) = 0.0
          return 0.0
        } else if(node.isInstanceOf[Mem[_]]){
          nodeArrivalTimes(node) = 0.0
          return 0.0
        } else if(!requireStage(node)){
          nodeArrivalTimes(node) = 0.0
          return 0.0
        } else if(coloredNodes.contains(node) && coloredNodes(node).length > 1 && (coloredNodes(node)(0) != coloredNodes(node)(1))) {
          nodeArrivalTimes(node) = 0.0
          return 0.0
        } else {
          var arrivalTime :Double= 0.0
          for(input <- node.inputs){
            arrivalTime = Math.max(arrivalTime, findArrivalTime(input))
          }
          nodeArrivalTimes(node) = arrivalTime + node.delay
          return arrivalTime + node.delay
        }
      }
      nodeArrivalTimes.clear()
      for(node <- pipelineComponent.nodes){
        if(coloredNodes.contains(node) && coloredNodes(node).length > 1 && (coloredNodes(node)(0) != coloredNodes(node)(1))){
          for(input <- node.inputs){
            findArrivalTime(input)
          }
        }
      }
      for(node <- sinkNodes()){
        findArrivalTime(node)
      }
    }
    var maxPath:Node = null
    def findEnergy(coloredNodes: HashMap[Node, ArrayBuffer[Int]]): Double = {
      calculateArrivalTimes()
      var maxLength = 0.0
      for(node <- nodeArrivalTimes.keys){
        maxLength = Math.max(maxLength, nodeArrivalTimes(node))
        maxPath = node
      }
      return maxLength
    } 
    
    def findUnpipelinedPathLength(coloredNodes: HashMap[Node, ArrayBuffer[Int]]): Double = {
      val nodeArrivalTimes = new HashMap[Node, Double]
      def calculateArrivalTimes() = {
        def findArrivalTime(node: Node) : Double = {
          if(nodeArrivalTimes.contains(node)){
            return nodeArrivalTimes(node)
          } else if(isSource(node)){
            nodeArrivalTimes(node) = 0.0
            return 0.0
          } else if(node.isInstanceOf[Mem[_]]){
            nodeArrivalTimes(node) = 0.0
            return 0.0
          } else if(!requireStage(node)){
            nodeArrivalTimes(node) = 0.0
            return 0.0
          } else {
            var arrivalTime :Double= 0.0
            for(input <- node.inputs){
              arrivalTime = Math.max(arrivalTime, findArrivalTime(input))
            }
            nodeArrivalTimes(node) = arrivalTime + node.delay
            return arrivalTime + node.delay
          }
        }
        nodeArrivalTimes.clear()
        for(node <- sinkNodes()){
          findArrivalTime(node)
        }
      }
    
      calculateArrivalTimes()
      var maxLength = 0.0
      for(node <- nodeArrivalTimes.keys){
        maxLength = Math.max(maxLength, nodeArrivalTimes(node))
      }
      return maxLength
    }
    
    //randomly move a combinational node across a pipeline boundary
    def getNewPlacement() = {
      if(Math.random > 0.5){//move node up a stage
        val eligibleNodes = new ArrayBuffer[Node]//list of nodes that can be legally moved up one stage. A node can be legally moved up one stage if all of its consumer nodes have a stage number greater than its stage number
        
        //find eligibleNodes
        for((node, stageNums) <- coloredNodes.filter(_._2.length > 0)){ 
          if(requireStage(node) && !annotatedStages.contains(node) && !isSource(node) && !isSink(node) && !fillerNodes.contains(node) && coloredNodes(node)(0) < pipelineLength - 1) {
            val nodeStage = coloredNodes(node)(0)
            var consumersEligible = true
            Predef.assert(coloredNodes(node).length <= 1, "" + node + " " + coloredNodes(node))
            for(consumer <- node.consumers){
              if(requireStage(consumer)){
                if(coloredNodes.contains(consumer) && coloredNodes(consumer).length > 1){
                  Predef.assert(coloredNodes(consumer).length <= 2)
                  consumersEligible = consumersEligible && (Math.max(coloredNodes(consumer)(0), coloredNodes(consumer)(1)) > nodeStage)
                } else {
                  consumersEligible = consumersEligible && false
                  //Predef.assert(coloredNodes(consumer)(0) == nodeStage)
                }
              }
            }
            if(consumersEligible) eligibleNodes += node
          }
        }
        
        //randomly select eligible node to move
        //Predef.assert(eligibleNodes.length > 0)
        if(eligibleNodes.length > 0){
          val idx = scala.util.Random.nextInt(eligibleNodes.length)
          val movedNode = eligibleNodes(idx)
          lastMovedNodes += ((movedNode, coloredNodes(movedNode).clone()))
          val movedNodeStage = coloredNodes(movedNode)(0)
          coloredNodes(movedNode)(0) = coloredNodes(movedNode)(0) + 1
          for(input <- movedNode.inputs){
            if(requireStage(input)){
              lastMovedNodes += ((input, coloredNodes(input).clone()))
              if(coloredNodes(input).length == 2){
                if(coloredNodes(input)(0) == movedNodeStage){
                  coloredNodes(input)(0) = coloredNodes(input)(0) + 1
                } else {
                  coloredNodes(input)(1) = coloredNodes(input)(1) + 1
                }
                if(coloredNodes(input)(0) == coloredNodes(input)(1)){
                  coloredNodes(input) -= coloredNodes(input)(1)
                }
              } else {
                coloredNodes(input) += coloredNodes(input)(0) + 1
              }
            }
          }
          for(consumer <- movedNode.consumers){
            if(requireStage(consumer)){
              lastMovedNodes += ((consumer, coloredNodes(consumer).clone()))
              if(coloredNodes(consumer).length == 2){
                if(coloredNodes(consumer)(0) == movedNodeStage){
                  coloredNodes(consumer)(0) = coloredNodes(consumer)(0) + 1
                } else {
                  coloredNodes(consumer)(1) = coloredNodes(consumer)(1) + 1
                }
                if(coloredNodes(consumer)(0) == coloredNodes(consumer)(1)){
                  coloredNodes(consumer) -= coloredNodes(consumer)(1)
                }
              }
            }
          }
          
        }
      } else {//move node down a stage
        val eligibleNodes = new ArrayBuffer[Node]//list of nodes that can be legally moved up one stage. A node can be legally moved up one stage if all of its consumer nodes have a stage number less than its stage number
        
        //find eligible nodes
        for((node, stageNums) <- coloredNodes.filter(_._2.length > 0)){ 
          if(requireStage(node) && !annotatedStages.contains(node) && !isSource(node) && !isSink(node) && !fillerNodes.contains(node) && coloredNodes(node)(0) > 0) {
            val nodeStage = coloredNodes(node)(0)
            var producersEligible = true
            Predef.assert(coloredNodes(node).length <= 1, "" + node + " " + coloredNodes(node))
            for(producer <- node.inputs){
              if(requireStage(producer)){
                if(coloredNodes.contains(producer) && coloredNodes(producer).length > 1){
                  Predef.assert(coloredNodes(producer).length <= 2)
                  producersEligible = producersEligible && (Math.min(coloredNodes(producer)(0), coloredNodes(producer)(1)) < nodeStage)
                } else {
                  producersEligible = producersEligible && false
                  //Predef.assert(coloredNodes(producer)(0) == nodeStage)
                }
              }
            }
            if(producersEligible) eligibleNodes += node
          }
        }
        
        //Predef.assert(eligibleNodes.length > 0)
        if(eligibleNodes.length > 0){
          val idx = scala.util.Random.nextInt(eligibleNodes.length)
          val movedNode = eligibleNodes(idx)
          val movedNodeStage = coloredNodes(movedNode)(0)
          lastMovedNodes += ((movedNode, coloredNodes(movedNode).clone()))
          coloredNodes(movedNode)(0) = coloredNodes(movedNode)(0) - 1
          for(input <- movedNode.inputs){
            if(requireStage(input)){
              lastMovedNodes += ((input, coloredNodes(input).clone()))
              if(coloredNodes(input).length == 2){
                if(coloredNodes(input)(0) == movedNodeStage){
                  coloredNodes(input)(0) = coloredNodes(input)(0) - 1
                } else {
                  coloredNodes(input)(1) = coloredNodes(input)(1) - 1
                }
                if(coloredNodes(input)(0) == coloredNodes(input)(1)){
                  coloredNodes(input) -= coloredNodes(input)(1)
                }
              }
            }
          }
          for(consumer <- movedNode.consumers){
            if(requireStage(consumer)){
              lastMovedNodes += ((consumer, coloredNodes(consumer).clone()))
              if(coloredNodes(consumer).length == 2){
                if(coloredNodes(consumer)(0) == movedNodeStage){
                  coloredNodes(consumer)(0) = coloredNodes(consumer)(0) - 1
                } else {
                  coloredNodes(consumer)(1) = coloredNodes(consumer)(1) - 1
                }
                if(coloredNodes(consumer)(0) == coloredNodes(consumer)(1)){
                  coloredNodes(consumer) -= coloredNodes(consumer)(1)
                }
              } else {
                coloredNodes(consumer) += coloredNodes(consumer)(0) - 1
              }
            }
          }
        }
      }
    }
    
    //reset coloredNodes to its state before the last call of getNewPlacemement()
    def recoverOldPlacement() = {
      for((node, stages) <- lastMovedNodes){
        coloredNodes(node) = stages
      }
    }
    
    println("max unpipelined path delay")
    println(findUnpipelinedPathLength(coloredNodes))
    println("max path delay before optimization:")
    println(findEnergy(coloredNodes))
    while(temp > 0.001){
      val currentEnergy = findEnergy(coloredNodes)
      getNewPlacement()
      val newEnergy = findEnergy(coloredNodes)
      /*println(currentEnergy)
      println(newEnergy)*/
      if(acceptProbability(currentEnergy, newEnergy, temp) < Math.random()){
        recoverOldPlacement()
      } 
      lastMovedNodes.clear()
      temp = temp - temp*coolRate
      iterCount = iterCount + 1
      if(iterCount/1000 > 0 && iterCount%1000 == 0){
        println(temp)
        println(currentEnergy)
        println(maxPath)
      }
    }
    println("max path delay after optimization:")
    println(findEnergy(coloredNodes))
    println(temp)
    println(iterCount)
    visualizeGraph(coloredNodes, "stages.gv")
    visualizeGraph(nodeArrivalTimes, "delays.gv")
  }*/
  
  def insertPipelineRegisters() = {
    for(stage <- 0 until pipeline.size) {
      for ((p, enum) <- pipeline(stage) zip pipeline(stage).indices) {
        pipelineReg(stage) += insertRegister(p._1.asInstanceOf[Bits], p._2.asInstanceOf[Bits], "Huy_" + enum + "_" + p._1.name).comp.asInstanceOf[Reg]
      }
    }
  }
  
  def colorPipelineStages() = {
    println("coloring pipeline stages")
    //map of nodes to consumers for use later
    val consumerMap = getConsumers()
    //set to keep track of nodes already traversed
    val visited = new HashSet[Node]
    //set to keep track of nodes that are write points
    val writePoints = new HashSet[Node]
    //set to keep track of unresolved nodes
    val unresolvedNodes = new HashSet[Node]
    //set to keep track of unresolved nodes in the previous iteration
    var oldUnresolvedNodes = new HashSet[Node]
    //bfsQueue 
    val bfsQueue = new ScalaQueue[Node]
    //HashMap of nodes -> stages that gets returned
    val coloredNodes = new HashMap[Node, Int]
    //checks to see if any of n's consumers have been resolved; returns the stage of n's resovled consumers and returns -1 if none of n's consumers have been resolved
    def resolvedConsumerStage(n: Node, isReg: Boolean): Int = {
      var stageNumber = -1
      if(consumerMap.contains(n)){
        for(i <- consumerMap(n)){
          if(coloredNodes.contains(i)){
            i match {
              case mux: Mux => {
                if(!isReg){
                  stageNumber = coloredNodes(i)
                } else {
                  Predef.assert(backend.isInstanceOf[CppBackend], "prevents reg from inferring its stage from its default mux in Cpp backend")
                }
              }
              case mem: Mem[_] =>
              case _ => {
                stageNumber = coloredNodes(i)
              }
            }
          }
        }
      }
      stageNumber
    }
    //checks to see if any of n's producers have been resolved; returns the stage of n's resovled producers and returns -1 if none of n's producers have been resolved
    def resolvedProducerStage(n: Node): Int = {
      var stageNumber = -1
      for(i <- n.getProducers()){
        if(coloredNodes.contains(i) & !i.isMem){
          if(isPipeLineReg(i)){
            //node n is in stage x+1 if its producer is a pipeline reg and is in stage x
            stageNumber = coloredNodes(i) + 1
          } else {
            stageNumber = coloredNodes(i)
          }
        }
      }
      stageNumber
    }
   
    //if n is a user defined pipeline register, return n's stage number
    def findPipeLineRegStage(n: Node): Int = {
      var result = -1
      for(i <- pipelineReg.keys){
        if(pipelineReg(i).contains(n)){
          result = i
        }
      }
      result
    }
    
    //do initial pass to mark write points for regs
    for(p <- procs){
      p match {
        case r: Reg => {
          if(!isPipeLineReg(r)){
            for(i <- r.getProducers()){
              writePoints += i
            }
          }
        }
        case _ =>
      }
    }
    //do initial pass to to set the stage of the user defined pipeline registers in coloredNodes
    this.bfs((n: Node) => {
      if(isPipeLineReg(n)){
        coloredNodes(n) = findPipeLineRegStage(n)
      }
    })
    //initialize bfs queue with pipeline registers
    for(i <- pipelineReg.keys){
      for(n <- pipelineReg(i)){
        unresolvedNodes += n
      }
    }
    while(!unresolvedNodes.isEmpty && !oldUnresolvedNodes.equals(unresolvedNodes)){
      for(n <- unresolvedNodes){
        bfsQueue.enqueue(n)
        visited += n
      }
      oldUnresolvedNodes = unresolvedNodes.clone()
      unresolvedNodes.clear
      visited.clear
      while(!bfsQueue.isEmpty){
        //handle traversal
        val currentNode = bfsQueue.dequeue
        for(i <- currentNode.getProducers()){
          if(!visited.contains(i)) {
            bfsQueue.enqueue(i)
            visited += i
          }
        }
        if(consumerMap.contains(currentNode)){
          for(i <- consumerMap(currentNode)){
            if(!visited.contains(i)) {
              bfsQueue.enqueue(i)
              visited += i
            }
          }
        }
        //handle visit
        //only need to do stuff if currentNode does not already have a stage number
        if(!coloredNodes.contains(currentNode) & !currentNode.isMem & (currentNode.litOf == null)){
          if(currentNode.isReg && !isPipeLineReg(currentNode)){
            val consumerStageNum = resolvedConsumerStage(currentNode, true)
            if(consumerStageNum > -1){
              coloredNodes(currentNode) = consumerStageNum
            } else {
              unresolvedNodes += currentNode
            }
          } else if(writePoints.contains(currentNode)){
            val producerStageNum = resolvedProducerStage(currentNode)
            if(producerStageNum > -1){
              coloredNodes(currentNode) = producerStageNum
            } else {
              unresolvedNodes += currentNode
            }
          } else {
            val producerStageNum = resolvedProducerStage(currentNode)
            val consumerStageNum = resolvedConsumerStage(currentNode, false)
            if(producerStageNum > -1){
              coloredNodes(currentNode) = producerStageNum
            } else if(consumerStageNum > -1){
              coloredNodes(currentNode) = consumerStageNum
            } else {
              unresolvedNodes += currentNode
            }
          }       
        }
      }
    }
    nodeToStageMap = coloredNodes
  }

  //checks if n is a user defined pipeline register
  def isPipeLineReg(n: Node): Boolean = {
    var result = false
    for(i <- pipelineReg.values){
      if(i.contains(n)){
        result = true
      }
    }
    result
  }
  
  def findHazards() = {
    println("searching for hazards...")
    
    // handshaking stalls from variable latency components; stall entire pipe when data is not readyfor now
    globalStall = Bool(false)
    for (variableLatencyComponent <- VariableLatencyComponents) {
      val stall = variableLatencyComponent.io.req.valid && (!variableLatencyComponent.req_ready || !variableLatencyComponent.resp_valid)
      globalStall = globalStall || stall
    }

    // raw stalls
    var raw_counter = 0
    for (reg <- ArchitecturalRegs) {
      val readStage = getStage(reg)
      for (i <- 0 until reg.updates.length){
        val writeEn = reg.updates(i)._1
        val writeData = reg.updates(i)._2
        val writeStage = Math.max(getStage(writeEn), getStage(writeData))
        Predef.assert(writeStage > -1, "both writeEn and writeData are literals")
        val prevWriteEns = getVersions(writeEn, writeStage - readStage + 1)
        if (writeStage > readStage) {
          for(stage <- readStage + 1 until writeStage + 1) {
            var currentStageWriteEnable = Bool(true)
            if(-(stage - writeStage)  < prevWriteEns.length){
              currentStageWriteEnable = Bool()
              currentStageWriteEnable.inputs += prevWriteEns(-(stage - writeStage) )
            }
            val stageValidTemp = Bool()
            stageValidTemp.inputs += stageValids(stage)//hack to deal with empty bool() being merged with what its &&ed with randomly
            regRAWHazards(((reg, i, stage))) = (stageValidTemp && currentStageWriteEnable)
            regRAWHazards(((reg, i, stage))).nameIt("hazard_num" + raw_counter + "_" + reg.name + "_" + stage + "_" + writeEn.name)
            raw_counter = raw_counter + 1
            println("found reg RAW hazard " + writeEn.line.getLineNumber + " " + writeEn.line.getClassName)
          }
        }
      }
    }
    
    // FunMem hazards
    for (m <- TransactionMems) {
      for(i <- 0 until m.io.writes.length){
        val writePoint = m.io.writes(i)
        val writeAddr = writePoint.actualWaddr
        val writeEn = writePoint.actualWen
        val writeData = writePoint.actualWdata
        val writeStage = Math.max(getStage(writeEn),Math.max(getStage(writeAddr), getStage(writeData)))
        Predef.assert(writeStage > -1)
        for(j <- 0 until m.io.reads.length){
          val readPoint = m.io.reads(j)
          val readAddr = readPoint.adr
          val readData = readPoint.dat
          val readStage = Math.max(getStage(readAddr), getStage(readData))
          Predef.assert(readStage > -1)
          val writeEnables = getVersions(writeEn.asInstanceOf[Bool], writeStage - readStage + 1)
          val writeAddrs = getVersions(writeAddr.asInstanceOf[Bits], writeStage - readStage + 1)
          if(writeStage > readStage){
            for(stage <- readStage + 1 until writeStage + 1){
              var currentStageWriteEnable = Bool(true)
              var currentStageWriteAddr:Data = readAddr
              if(-(stage - writeStage)  < writeEnables.length){
                currentStageWriteEnable = Bool()
                currentStageWriteEnable.inputs += writeEnables(-(stage - writeStage) )
              }
              if(-(stage - writeStage)  < writeAddrs.length){
                currentStageWriteAddr = Bits()
                currentStageWriteAddr.inputs += writeAddrs(-(stage - writeStage) )
              }
              val stageValidTemp = Bool()
              stageValidTemp.inputs += stageValids(stage)//hack to deal with empty bool() being merged with what its &&ed with randomly
              tMemRAWHazards(((readPoint, i, stage))) = stageValidTemp && currentStageWriteEnable && (readAddr === currentStageWriteAddr)
              tMemRAWHazards(((readPoint, i, stage))).nameIt("hazard_num" + raw_counter + "_" +m.name + "_readport_num" + j + "_"+ stage + "_" + writeEn.name)
              raw_counter = raw_counter + 1
              println("found hazard" + writeEn.line.getLineNumber + " " + writeEn.line.getClassName + " " + j + " " + stage + " " + currentStageWriteEnable.name + " " + readAddr.name + " " + currentStageWriteAddr.name + " " + m.name)
            }
          }
        }
      }
    }
    
    println("reg raw hazards")
    println(regRAWHazards)
    println("tMem raw hazards")
    println(tMemRAWHazards)
  }
  
  def generateBypassLogic() = {
    //generate bypass logic for registers
    for(reg <- forwardedRegs){
      val forwardPoints = new HashMap[(Int, Int), Node]// map of (write port number, write stage) -> write data
      val readStage = getStage(reg)
      for(i <- 0 until reg.updates.length){
        val writeEn = reg.updates(i)._1
        val writeData = reg.updates(i)._2
        val writeStage = Math.max(getStage(writeEn), getStage(writeData))
        Predef.assert(writeStage > -1)
        val writeEns = getVersions(writeEn.asInstanceOf[Bool], writeStage - readStage + 1)
        val writeDatas = getVersions(writeData.asInstanceOf[Bits], writeStage - readStage + 1)
        val numStagesAvail = Math.min(writeEns.length, writeDatas.length)
        for(j <- 0 until numStagesAvail) {
          val writeDataBits = Bits()//needed to deal with op nodes
          writeDataBits.inputs += writeDatas(j)
          forwardPoints(((i, writeStage - j))) = writeDataBits
        }
      }

      val muxMapping = new ArrayBuffer[(Bool, Bits)]()
      for(j <- getStage(reg) + 1 to pipelineReg.size){
        for(i <- 0 until reg.updates.length){
          if(forwardPoints.contains(((i,j)))){ 
            val forwardCond = regRAWHazards(((reg, i, j)))
            muxMapping += ((forwardCond, forwardPoints(((i, j))).asInstanceOf[Bits]))
            regRAWHazards -= ((reg, i, j))
            println("added fowarding point (" + reg.name + ", write port #" + i + ", write stage: " + j +  ")")
          }
        }
      }

      val tempBitsNode = insertBitOnOutputs(reg).asInstanceOf[Bits]
      val originalConsumers = tempBitsNode.consumers.clone()
      val bypassMux = MuxCase(tempBitsNode, muxMapping)
      bypassMux.nameIt("bypassMux_" + reg.name)
      for (consumer <- originalConsumers){
        consumer.replaceProducer(tempBitsNode, bypassMux)
      }
    }
    
    //generate bypass logic for tmems
    for((tMem, readPort) <- forwardedMemReadPorts){
      val forwardPoints = new HashMap[(Int, Int), Node]()// map of (writeport number, write stage) -> write data
      val readAddr = readPort.adr
      val readData = readPort.dat
      val readStage = Math.max(getStage(readAddr), getStage(readData.asInstanceOf[Node]))
      Predef.assert(readStage > -1)
      for(i <- 0 until tMem.io.writes.length){
        val writePoint = tMem.io.writes(i)
        if(!memNonForwardedWritePoints.contains(writePoint)){
          val writeAddr = writePoint.actualWaddr
          val writeEn = writePoint.actualWen
          val writeData = writePoint.actualWdata
          val writeStage = Math.max(getStage(writeEn),Math.max(getStage(writeAddr), getStage(writeData)))
          Predef.assert(writeStage > -1)
          val writeEns = getVersions(writeEn.asInstanceOf[Bool], writeStage - readStage + 1)
          val writeAddrs = getVersions(writeAddr.asInstanceOf[Bits], writeStage - readStage + 1)
          val writeDatas = getVersions(writeData.asInstanceOf[Bits], writeStage - readStage + 1)
          val numStagesAvail = Math.min(writeEns.length, Math.min(writeDatas.length, writeAddrs.length))
          for(j <- 0 until numStagesAvail){
            Predef.assert(getStage(writeEns(j)) > -1)
            val writeDataBits = Bits()//needed to deal with op nodes
            writeDataBits.inputs += writeDatas(j)
            forwardPoints(((i, writeStage - j))) = writeDataBits
          }
        }
      }
      val muxMapping = new ArrayBuffer[(Bool, Bits)]()
      for(j <- getStage(readPort.adr) + 1 to pipelineReg.size){
        for(i <- 0 until tMem.io.writes.length){
          if(forwardPoints.contains(((i, j)))){
            val forwardCond = tMemRAWHazards(((readPort.asInstanceOf[FunRdIO[Data]], i, j)))
            muxMapping +=((forwardCond, forwardPoints(((i, j))).asInstanceOf[Bits]))
            tMemRAWHazards -= ((readPort.asInstanceOf[FunRdIO[Data]], i, j))
            println("added fowarding point (" + readPort.dat.asInstanceOf[Node].name + ", write port #" + i + ", write stage: " + j +  ")")
          }
        }
      }
      
      val bypassMux = MuxCase(readPort.dat.asInstanceOf[Bits], muxMapping)
      bypassMux.nameIt("bypassMux_" + tMem.name + "_" + readPort.dat.asInstanceOf[Node].name)
      val originalConsumers = readPort.dat.asInstanceOf[Bits].consumers.clone()
      for (consumer <- originalConsumers){
        consumer.replaceProducer(readPort.dat.asInstanceOf[Node], bypassMux)    
      }
    }
  }
  
  def generateSpeculationLogic() = {
    val tempKills = new ArrayBuffer[Bool]
    for(i <- 0 until pipelineLength){
      tempKills += Bool(false)
    }
    
    for((reg, specWriteData) <- speculatedRegs){
      ArchitecturalRegs -= reg
      val readStage = getStage(reg)
      val writeStage = Math.max(getStage(reg.updates(0)._1), getStage(reg.updates(0)._2))
      
      //generate registers to hold the speculated data
      var currentStageSpecWriteData = specWriteData
      for(stage <- readStage until writeStage){
        val specWriteDataReg = Reg(Bits())
        when(~stageStalls(stage) && ~globalStall){
          specWriteDataReg := currentStageSpecWriteData
        }
        when(pipelineComponent._reset){
          specWriteDataReg := Bits(0)
        }
        specWriteDataReg.comp.asInstanceOf[Reg].clock = pipelineComponent.clock
        specWriteDataReg.nameIt("spec_write_data_reg_" + reg.name + "_" + stage)
        specWriteDataReg := currentStageSpecWriteData
        currentStageSpecWriteData = specWriteDataReg
      }
      
      //figure out when we mis-speculate
      val actualWriteData = Bits()
      actualWriteData.inputs += reg.inputs(0)
      
      val stageValidTemp = Bool()
      stageValidTemp.inputs += stageValids(writeStage)//hack to deal with empty bool() being merged with what its &&ed with randomly
      val kill = ~(currentStageSpecWriteData === actualWriteData) && stageValidTemp
      kill.nameIt("spec_kill_" + reg.name)
      for(stage <- readStage until writeStage ){
        tempKills(stage) = tempKills(stage) || kill
      }
      
      //remove reg's raw hazards from regRAWHazards
      for (i <- 0 until reg.updates.length){
        val writeEn = reg.updates(i)._1
        val writeData = reg.updates(i)._2
        val writeStage = Math.max(getStage(writeEn), getStage(writeData))
        Predef.assert(writeStage > -1, "both writeEn and writeData are literals")
        val prevWriteEns = getVersions(writeEn, writeStage - readStage + 1)
        if (writeStage > readStage) {
          for(stage <- readStage + 1 until writeStage + 1) {
            regRAWHazards -= (((reg, i, stage)))
          }
        }
      }
      
      //modify reg's write port
      val doSpeculate = ~stageStalls(readStage) && ~globalStall && ~kill
      doSpeculate.nameIt("do_speculate_" + reg.name)
      reg.inputs(0) = Multiplex(kill, reg.inputs(0), reg)
      reg.inputs(0) = Multiplex(doSpeculate, specWriteData, reg.inputs(0))   
    }
    
    for(i <- 0 until pipelineLength){
      tempKills(i).nameIt("PipeStage_Kill_" + i)
      stageKills(i) = tempKills(i)//hack fix this(why does := work?)
    }
  }
  
  def generateInterlockLogic() = {
    //initialize temporpary valid signals
    val tempStalls = new ArrayBuffer[Bool]
    for(i <- 0 until pipelineLength){
      tempStalls += Bool(false)
    }
    
    //initialize registers for stageValid signals
    val validRegs = new ArrayBuffer[Bool]
    for (i <- 0 until pipelineLength - 1) {
      val validReg = Reg(Bool())
      when(~stageStalls(i) && ~globalStall){
        validReg := stageValids(i)
      }
      when(pipelineComponent._reset){
        validReg := Bool(false)
      }
      validReg.comp.asInstanceOf[Reg].clock = pipelineComponent.clock
      validRegs += validReg
      validReg.comp.nameIt("Stage_" + i + "_valid_reg")
    }
    
    //initialize temporpary valid signals
    val tempValids = new ArrayBuffer[Bool]
    tempValids += ~stageKills(0)
    for(i <- 1 until pipelineLength){
      tempValids += validRegs(i - 1) && ~stageKills(i)
    }

    //collect RAW hazards for valids and stalls for every stage
    //reg RAW hazards
    for ((key, value) <- regRAWHazards){
      val reg = key._1
      val RAWHazardSignal = value
      val readStage = getStage(reg)
      tempValids(readStage) = tempValids(readStage) && ~RAWHazardSignal
      if(readStage > 0 && readStage < pipelineLength){
        tempStalls(readStage - 1) = tempStalls(readStage - 1) || RAWHazardSignal
      }
    }
    
    //transactionMem RAW hazards
    for ((key, value) <- tMemRAWHazards){
      val readAddr = key._1.adr
      val RAWHazardSignal = value
      val readStage = getStage(readAddr)
      tempValids(readStage) = tempValids(readStage) && ~RAWHazardSignal
      if(readStage > 0 && readStage < pipelineLength ){
        tempStalls(readStage - 1) = tempStalls(readStage - 1) || RAWHazardSignal
      }
    }
    
    //connect actual stage valids to the tempValids
    for(i <- 0 until pipelineLength){
      stageValids(i).inputs += tempValids(i)
    }
    
    //generate logic for stall signals
    for(i <- (0 until pipelineLength - 2).reverse) {
      tempStalls(i) = tempStalls(i) || tempStalls(i+1)
    }

    //connect actual stage stalls to the tempStalls
    for(i <- 0 until pipelineLength){
      if(tempStalls(i).litOf != null){
        stageStalls(i) := ~(stageValids(0) === stageValids(0))//hack to get around stage(i) := const failing to code gen properly
      } else {
        stageStalls(i) := tempStalls(i)
      }
    }
    
    //wire stage valid and stall signals to architecural state write enables
    //regs
    for(reg <- ArchitecturalRegs){
      val writeStage = reg.updates.map(_._2).map(getStage(_)).filter(_ > - 1)(0)
      reg.inputs(0) = Multiplex( ~globalStall && stageValids(writeStage) && ~stageStalls(writeStage), reg.inputs(0), reg)
    }
    //transactionMems
    for(tmem <- TransactionMems){
      for(i <- 0 until tmem.io.writes.length){
        val writePoint = tmem.io.writes(i)
        val writeAddr = writePoint.actualWaddr
        val writeEn = writePoint.actualWen
        val writeData = writePoint.actualWdata
        val writeStage = Math.max(getStage(writeEn),Math.max(getStage(writeAddr), getStage(writeData)))
        val newWriteEn = writeEn.asInstanceOf[Bool] && ~globalStall && stageValids(writeStage) && ~stageStalls(writeStage)
        //fix writeEn's consumer list
        val writeEnMuxFillerInput = writePoint.is.inputs(0).inputs(0).inputs(0)//need a less hack way of finding this
        val consumer_index = writeEn.consumers.indexOf(writeEnMuxFillerInput)
        Predef.assert(consumer_index > -1)        
        writeEn.consumers(consumer_index) = newWriteEn
        //fix writeEnMux's inputs list
        val producer_index = writeEnMuxFillerInput.inputs.indexOf(writeEn)
        Predef.assert(producer_index > -1)
        writeEnMuxFillerInput.inputs(producer_index) = newWriteEn
        //populate newWriteEn's consumer list
        newWriteEn.addConsumers 
      }
    }
    //wire zero to all Bool outputs if stage is not valid or is stalled; this is a hack to let outside components know that a pipeline stage is invalid
    val outputs = pipelineComponent.io.flatten.filter(_._2.dir == OUTPUT).map(_._2)
    for(output <- outputs) {
      if(output.isInstanceOf[Bool]){
        val outputStage = getStage(output)
        val tempMux = Multiplex(~globalStall && stageValids(outputStage) && ~stageStalls(outputStage), output.inputs(0), Bool(false))
        output.inputs(0) = tempMux
      }
    }
  }
  
  def getVersions(node: Node, maxLen: Int): ArrayBuffer[Node] = {
    val result = new ArrayBuffer[Node]//stage of node decreases as the array index increases
    var currentNode = node
    if(!requireStage(currentNode)){
      for(i <- 0 until maxLen){
        result += currentNode
      }
    } else {
      while(currentNode.inputs.length == 1 && !currentNode.inputs(0).isInstanceOf[Op]){
        if(currentNode.inputs(0).isInstanceOf[Reg]){
          if(!isPipeLineReg(currentNode)){
            if(result.length < maxLen) result += currentNode
          } else {
            if(result.length < maxLen) result += currentNode
            return result
          }
          currentNode = currentNode.inputs(0).asInstanceOf[Reg].updates(0)._2
        } else {
          currentNode = currentNode.inputs(0)
        } 
      }
      if(result.length < maxLen) result += currentNode
    }
    result
  }

  def insertBubble(globalStall: Bool) = {
    for (stage <- 0 until pipelineReg.size) {
      val stall = stalls(stage+1).foldLeft(Bool(false))(_ || _)
      val kill = kills(stage).foldLeft(Bool(false))(_ || _)
      val speckill = speckills(stage).foldLeft(Bool(false))(_ || _)
      for (r <- pipelineReg(stage)) {
        r.updates += ((kill, r.init))
        r.updates += ((stall, r))
        r.updates += ((speckill, r.init))
        r.updates += ((globalStall, r))
        r.genned = false
      }
      val valid = valids(stage).comp
      valid.updates += ((kill, Bool(false)))
      valid.updates += ((stall, valid))
      valid.updates += ((speckill, Bool(false)))
      valid.updates += ((globalStall, valid))
      valid.genned = false
    }
    
  }
  
  def verifyLegalStageColoring(coloredNodes: HashMap[Node, ArrayBuffer[Int]]) = {
    object vDirection extends Enumeration {
      type vDirection = Value
      val PRODUCER, CONSUMER, DONTCARE = Value
    }
    import vDirection._
    def getStage(node: Node, dir: vDirection): Int = {
      if(requireStage(node)){
        if(coloredNodes(node).length > 1){
          if(dir == PRODUCER){//if node is a pipeline register and is viewed as a producer
            return Math.max(coloredNodes(node)(0), coloredNodes(node)(1))
          } else if(dir == CONSUMER){//if node is a pipeline register and is viewed as a consumer
            return Math.max(coloredNodes(node)(0), coloredNodes(node)(1))
          } else {
            Predef.assert(false, "node not expected to have more than 1 stage number")
            return -3
          }
        } else {
          return coloredNodes(node)(0)
        }
      } else if (!requireStage(node)){
        return -1
      } else {
        Predef.assert(false, "node expected to have stage number, but does not have stage number: " + node)
        return -2
      }
    }
    
    //check that all combinational logic nodes have all inputs from the same stage
    for(node <- pipelineComponent.nodes){
      if(node.isInstanceOf[Op] && requireStage(node)){
        if(!node.isInstanceOf[Mux] && !(node.consumers(0).isInstanceOf[Reg] && node.consumers(0).inputs.contains(node))){//hack because muxes generated for pipeline register reset values don't have stages //also hack to deal with or node generated for register write enable in verilog backend
          val nodeStage = getStage(node, DONTCARE)
          for(input <- node.inputs.filter(requireStage(_))){
            val inputStage = getStage(input, PRODUCER)
            Predef.assert(inputStage == nodeStage || inputStage == -1, "combinational node input does not have stage number")
          }
        }
      }
    }
    
    //check that all architectural register has write points in the same stage and that its write stage >= its read stage
    for(reg <- ArchitecturalRegs){
      val readStage = getStage(reg, DONTCARE)
      val writeEnables = reg.updates.map(_._1).filter(_.name != "reset")
      val writeDatas = reg.updates.filter(_._1.name != "reset").map(_._2)
      val writeEnableStages = writeEnables.map(getStage(_, DONTCARE)).filter(_ > -1)
      val writeDataStages = writeDatas.map(getStage(_, DONTCARE)).filter(_ > -1)
      
      Predef.assert(writeDataStages.length > 0, "register written by all constants; this case is not handled yet:" + writeDataStages)
      val writeStage = writeDataStages.head
      Predef.assert(writeStage >= readStage)
      if(writeEnableStages.length > 0){
        Predef.assert(writeEnableStages.tail.map( _ == writeStage).foldLeft(true)(_ && _), reg.name + " " + reg.line.getLineNumber + " " + reg.line.getClassName + " " + writeEnableStages) 
      }
      Predef.assert(writeDataStages.tail.map( _ == writeStage).foldLeft(true)(_ && _), reg.name + " " + reg.line.getLineNumber + " " + reg.line.getClassName + " " + writeDataStages)
    }
    
    for(tmem <- TransactionMems){//check all tmems read and write ports are annotated; data, addr, and enable of each port is in same stage; all read ports are in the same stage; all write ports are in same stage; write ports have stage >= read port stage
      val readPorts = tmem.io.reads
      val writePorts = tmem.io.writes
      
      val readStage = getStage(readPorts(0).dat, DONTCARE)
      Predef.assert(getStage(readPorts(0).adr, DONTCARE) == readStage, "transactionMem readport nodes do not all have same stage numbers")
      for(readPort <- readPorts){
        Predef.assert(getStage(readPort.adr, DONTCARE) == readStage, "transactionMem readport nodes do not all have same stage numbers")
        Predef.assert(getStage(readPort.dat, DONTCARE) == readStage, "transactionMem readport nodes do not all have same stage numbers")
      }
       
      val writeStage = getStage(writePorts(0).dat, DONTCARE)
      Predef.assert(getStage(writePorts(0).adr, DONTCARE) == writeStage, "transactionMem writeport nodes do not all have same stage numbers")
      for(writePort <- writePorts){
        Predef.assert(getStage(writePort.is, DONTCARE) == writeStage, "transactionMem writeport nodes do not all have same stage numbers")
        Predef.assert(getStage(writePort.adr, DONTCARE) == writeStage, "transactionMem writeport nodes do not all have same stage numbers")
        Predef.assert(getStage(writePort.dat, DONTCARE) == writeStage, "transactionMem writeport nodes do not all have same stage numbers")
      } 
      
      Predef.assert(writeStage >= readStage, "transactionMem has writePort at earlier stage than readPort")         
    }
    
    //check that all IO nodes are in the same stage
    val inputs = pipelineComponent.io.flatten.filter(_._2.dir == INPUT).map(_._2)
    val outputs = pipelineComponent.io.flatten.filter(_._2.dir == OUTPUT).map(_._2)
    if(inputs.length > 0){
      val inputStage = getStage(inputs(0), DONTCARE)
      for(input <- inputs) {
        Predef.assert(getStage(input, DONTCARE) == inputStage, "input nodes do not all belong to the same stage")
      }
    }
    
    if(outputs.length > 0){
      val outputStage = getStage(outputs(0), DONTCARE)
      for(output <- outputs) {
        Predef.assert(getStage(output, DONTCARE) == outputStage, "output nodes do not all belong to the same stage")
      }
    }
    //check that all variable latency units have IO nodes in the same stage
    for(vComponent <- VariableLatencyComponents){
      Predef.assert(getStage(vComponent.io.req.valid, DONTCARE) == getStage(vComponent.io.req.bits, DONTCARE))
      Predef.assert(getStage(vComponent.io.req.valid, DONTCARE) == getStage(vComponent.resp_ready, DONTCARE))
      Predef.assert(getStage(vComponent.io.req.valid, DONTCARE) == getStage(vComponent.io.resp, DONTCARE))
      Predef.assert(getStage(vComponent.io.req.valid, DONTCARE) == getStage(vComponent.resp_valid, DONTCARE))
      Predef.assert(getStage(vComponent.io.req.valid, DONTCARE) == getStage(vComponent.req_ready, DONTCARE))
    }
  }
  
  //outputs graphviz file for subgraph centered around root
  def visualizeSubGraph(root: Node, fileName: String, levels: Int) ={
    val outFile = new java.io.FileWriter("/home/eecs/wenyu/auto-pipelining/" + fileName)
    outFile.write("digraph G {\n")
    outFile.write("graph [rankdir=LR];\n")
    
    var nameEnum = 0
    var currentLevel = 0
    val nodeNames = new HashMap[Node, String]
    var currentQueue = new ScalaQueue[Node]
    var nextQueue = new ScalaQueue[Node]
    
    currentQueue.enqueue(root)
    outFile.write("n" + nameEnum + " [label=\"" + root.name +  """\n""" + "ROOT" +"\"];\n")
    nodeNames(root) = "n" + nameEnum
    nameEnum = nameEnum + 1
    
    while(currentLevel < levels){
      while(!currentQueue.isEmpty){
        val node = currentQueue.dequeue
        for(input <- node.inputs){
          if(!nodeNames.contains(input)){
            nextQueue.enqueue(input)
            outFile.write("n" + nameEnum + " [label=\"" + input.name + """\n""" + "level_" + currentLevel + "\"];\n")
            nodeNames(input) = "n" + nameEnum
            nameEnum = nameEnum + 1
            outFile.write(nodeNames(input) + " -> " + nodeNames(node) + ";\n")
          }
        }
        for(consumer <- node.consumers){
          if(!nodeNames.contains(consumer)){
            nextQueue.enqueue(consumer)
            outFile.write("n" + nameEnum + " [label=\"" + consumer.name + """\n""" + "level_" + currentLevel + "\"];\n")
            nodeNames(consumer) = "n" + nameEnum
            nameEnum = nameEnum + 1
            outFile.write(nodeNames(node) + " -> " + nodeNames(consumer) + ";\n")
          }
        }
      }
      currentLevel = currentLevel + 1
      currentQueue = nextQueue
      nextQueue = new ScalaQueue[Node]
    }
    outFile.write("}\n")
    outFile.close
  }
  
  def visualizeGraph(nodeMap: HashMap[Node,_], fileName: String) = {
    val outFile = new java.io.FileWriter("/home/eecs/wenyu/auto-pipelining/" + fileName)
    outFile.write("digraph G {\n")
    outFile.write("graph [rankdir=LR];\n")
    var nameEnum = 0
    val nodeNames = new HashMap[Node, String]
    for(node <- nodeMap.keys){
      var fillerStatus = ""
      if(fillerNodes.contains(node)){
        fillerStatus = "filler"
      }
      outFile.write("n" + nameEnum + " [label=\"" + node.name + " " + fillerStatus + """\n""" + nodeMap(node) + "\"];\n")
      nodeNames(node) = "n" + nameEnum
      nameEnum = nameEnum + 1
    }
    for(node <- nodeMap.keys){
      if(!isSource(node)){
        for(input <- node.inputs){
          if(nodeNames.contains(input)){
            outFile.write(nodeNames(input) + " -> " + nodeNames(node) + ";\n")
          }
        }
      }
    }
    outFile.write("}\n")
    outFile.close
  }
  
  def compBfs(comp: Module, visit: Node => Unit) : Unit = {
    val walked = new HashSet[Node]
    val bfsQueue = new ScalaQueue[Node]
    for((n, io) <- comp.io.flatten)
      bfsQueue.enqueue(io)
    while(!bfsQueue.isEmpty){
      val top = bfsQueue.dequeue
      walked += top
      visit(top)
      if(!top.isInstanceOf[Bits] || top.asInstanceOf[Bits].dir == OUTPUT || top.component != comp) {
        for(i <- top.inputs) {
          if(!(i == null)) {
            if(!walked.contains(i)) {
              bfsQueue.enqueue(i) 
              walked += i
            }
          }
        }
      }
    }
  }
}

