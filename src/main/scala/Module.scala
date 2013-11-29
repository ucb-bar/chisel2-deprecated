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

object Module {
  /* We have to keep a list of public methods which happen to be public,
   have no arguments yet should not be used to generate C++ or Verilog code. */
  val keywords = HashSet[String]("test")

  var warnInputs = false
  var warnOutputs = false
  var saveWidthWarnings = false
  var saveConnectionWarnings = false
  var saveComponentTrace = false
  var saveGraph = false       // by Donggyu
  var annotateSignals = false // by Donggyu
  var signalFilename = ""     // by Donggyu
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
    saveGraph = false       // by Donggyu
    annotateSignals = false // by Donggyu
    signalFilename = ""     // by Donggyu
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
  var pipeline = new HashMap[Int, ArrayBuffer[(Node, Bits)]]() //need to be deprecated
  var pipelineComponent: Module = null
  var pipelineReg = new HashMap[Int, ArrayBuffer[Reg]]()
  
  val forwardedRegs = new HashSet[Reg]
  val forwardedMemReadPoints = new HashSet[(TransactionMem[_], FunRdIO[_])]
  val memNonForwardedWritePoints = new HashSet[FunWrIO[_]]
  
  val speculation = new ArrayBuffer[(Bits, Bits)]
  
  var annotatedStages = new ArrayBuffer[(Node, Int)]()
  
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
  val fillerNodes = new HashSet[Node]
  
  val regRAWHazards = new HashMap[(Reg, Int, Int), Bool] //map of (register, updatelist num, write stage) -> RAW signal
  val tMemRAWHazards = new HashMap[(FunRdIO[Data], Int, Int), Bool] //map of (tmem readport, writeport number, write stage) -> RAW signal
  
  def setPipelineLength(length: Int) = {
    pipelineLength = length
    for (i <- 0 until length - 1) {
      pipelineReg += (i -> new ArrayBuffer[Reg]())
    }
    for (i <- 0 until length) {
      val valid = Bool()
      stageValids += valid
      valid.nameIt("PipeStageValid_" + i)
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
  def annotateNodeStage(n: Node, s:Int) = {//insert pipeline registers by annotating nodes with stages
    if(n.isInstanceOf[Data] && n.asInstanceOf[Data].comp != null){
      annotatedStages += ((n.asInstanceOf[Data].comp, s))
    } else {
      annotatedStages += ((n,s))
    }
  }
  
  def addForwardedReg(d: Reg) = {
    forwardedRegs += d
  }
  def addForwardedMemReadPoint(m: TransactionMem[_], r: FunRdIO[_]) = {
    forwardedMemReadPoints += ((m.asInstanceOf[TransactionMem[Data]], r.asInstanceOf[FunRdIO[Data]]))
  }
  def addNonForwardedMemWritePoint[T <: Data] (writePoint: FunWrIO[T]) = {
    memNonForwardedWritePoints += writePoint
  }

  def speculate(s: Bits, v: Bits) = {
    speculation += ((s, v))
  }
  
  //we should not propagate stage numbers to literals, reset signals, and clock signals
  def requiresNoStageNum(node: Node) : Boolean = {
    (node.litOf != null) || (node == pipelineComponent.clock) || (node == pipelineComponent._reset)
  }
  
  def getStage(n: Node): Int = {
    if (nodeToStageMap.contains(n))
      return nodeToStageMap(n)
    else if(requiresNoStageNum(n))
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

  val signals = new ArrayBuffer[Node]() // by Donggyu

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
    for(c <- Module.components)
      for((n, io) <- c.io.flatten)
        res.enqueue(io)

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
    for(node <- pipelineComponent.nodes){
      if (node.isInstanceOf[Reg] && !isPipeLineReg(node)) ArchitecturalRegs += node.asInstanceOf[Reg]
    }
    for(component <- pipelineComponent.children){
      if(component.isInstanceOf[TransactionMem[Data]]){
        TransactionMems += component.asInstanceOf[TransactionMem[Data]]
      }
    }
    for(component <- pipelineComponent.children){
      if(component.isInstanceOf[TransactionalComponent]){
        VariableLatencyComponents += component.asInstanceOf[TransactionalComponent]
      }
    }
  }
  
  def insertPipelineRegisters2() = { 
    println("finding valid pipeline register placement")
    val coloredNodes = propagateStages()
    var maxStage = 0
    for((node, stages) <- coloredNodes){
      if(stages.length > 0 && stages.max > maxStage){
        maxStage = stages.max
      }
    }
    setPipelineLength(maxStage + 1)

    /*println("optimizing pipeline register placement")
    optimizeRegisterPlacement(coloredNodes)
    println("inserting pipeline registers")*/
    
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
          currentNodeOut = insertRegister(currentNodeOut.asInstanceOf[Bits], Bits(0), "Stage_" + (Math.min(stgs(0),stgs(1)) + i) + "_" + "PipeReg_"+ currentNodeOut.name + counter)
          nodeToStageMap(currentNodeOut) = Math.min(stgs(0),stgs(1)) + i + 1
          nodeToStageMap(currentNodeOut.asInstanceOf[Bits].comp) = Math.min(stgs(0),stgs(1)) + i + 1
          counter = counter + 1
          pipelineReg(Math.min(stgs(0),stgs(1)) + i) += currentNodeOut.asInstanceOf[Bits].comp.asInstanceOf[Reg]
          println("DEBUG0")
        }
      } else if(stgs.length == 1){
        nodeToStageMap(node) = stgs(0)
      }
    }
  }
  
  
  def propagateStages() = {
    val regWritePoints = new HashSet[Node] //list of all register write inputs and transactionMem write port nodes(including write addr, write data, write en)
    val regReadPoints = new HashSet[Node] //list of all register read outputs and transactionMem read port
    val tMemWritePoints = new HashSet[Node] //list of all transactionMem write port nodes(including write addr, write data, write en)
    val tMemReadDatas = new HashSet[Node] //list of all transactionMem read port nodes(including read addr, read data, read en)
    val tMemReadAddrs = new HashSet[Node]
    val inputNodes = new HashSet[Node] //list of all module Input Nodes
    val outputNodes = new HashSet[Node] //list of all module Output Nodes
    
    val bfsQueue = new ScalaQueue[(Node, Boolean)] //the 2nd item of the tuple indicateds which direction the node should propagate its stage number to. True means propagate to outputs, false means propagate to inputs
    val retryNodes = new ArrayBuffer[(Node, Boolean)] //list of nodes that need to retry their stage propagation because their children weren't ready
    
    val coloredNodes = new HashMap[Node, ArrayBuffer[Int]] //map of node to stage numbers to be returned
    var oldColoredNodes = new HashMap[Node, ArrayBuffer[Int]] //remember old colored nodes so that we can see if the algorithm is making progress
    
    def insertFillerWires() = {//place extra wire nodes between all nodes
      val bfsQueue = new ScalaQueue[Node]
      val visited = new HashSet[Node]
      
      for(node <- regReadPoints){
        bfsQueue.enqueue(node)
      }
      for(node <- tMemReadDatas){
        bfsQueue.enqueue(node)
      }
      for(node <- inputNodes){
        bfsQueue.enqueue(node)
      }
      while(!bfsQueue.isEmpty){
        val currentNode = bfsQueue.dequeue
        for(child <- currentNode.consumers){
          if(!visited.contains(child) && !regWritePoints.contains(child) && !tMemWritePoints.contains(child) && !outputNodes.contains(child) && !tMemReadAddrs.contains(child)){
            bfsQueue.enqueue(child)
          }
        }
        visited +=currentNode
        fillerNodes ++= insertBitsOnOutputs(currentNode)
      }
    }
  
    def propagatedToChild(parentStage: Int, child: Node) : Boolean = {
      if(!requiresNoStageNum(child)){
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
    
    def propagateToProducers(node: Node) = {
      val propagatedChildren = new ArrayBuffer[Node]
      for(producer <- node.getProducers()){
        if(!propagatedToChild(coloredNodes(node)(0), producer)){//case if we have not already propagated to the producer
          if(!(fillerNodes.contains(node) && !fillerNodes.contains(producer) && coloredNodes.contains(producer) && (coloredNodes(producer).length > 0))){//don't propagate to producer if node is a fillerNode and producer is not and producer already has atleast 1 stage number
            if(producer.consumers.length <= 1){//case if producer only outputs to the current node
              if(!coloredNodes.contains(producer)){
                coloredNodes(producer) = new ArrayBuffer[Int]
              }
              coloredNodes(producer) += coloredNodes(node)(0)
              propagatedChildren += producer
            } else {
              var childResolvedAllProducers = true
              var lowestConsumerStage = Int.MaxValue
              for(childConsumer <- producer.consumers){
                if(!requiresNoStageNum(childConsumer)){
                  if(!coloredNodes.contains(childConsumer)){
                    childResolvedAllProducers = false
                  } else if(coloredNodes(childConsumer).length == 0) {
                    childResolvedAllProducers = false
                  } else{
                    if(coloredNodes(childConsumer).min < lowestConsumerStage){
                      lowestConsumerStage = coloredNodes(childConsumer).min
                    }
                  }
                }
              }
              if(childResolvedAllProducers){
                if(!coloredNodes.contains(producer)){
                  coloredNodes(producer) = new ArrayBuffer[Int]
                }
                //propagate stage to child
                if(!coloredNodes(producer).contains(lowestConsumerStage)){
                  coloredNodes(producer) += lowestConsumerStage  
                }
                //propagate child stage back to its producers
                for(childConsumer <- producer.consumers){
                  if(!requiresNoStageNum(childConsumer)){
                    if(!coloredNodes(childConsumer).contains(lowestConsumerStage)){
                      coloredNodes(childConsumer) += lowestConsumerStage  
                    }
                  }
                }
                propagatedChildren += producer
              }
            }
          }
        }
      }
      for(child <- propagatedChildren){
        if(child.name == "pc_reg"){
          println("DEBUG4")
          println(node)
          println(node.consumers)
          println(coloredNodes(node)(0))
        }
      }
      propagatedChildren
    }
    
    def propagateToConsumers(node: Node) = {
      val propagatedChildren = new ArrayBuffer[Node]
      for(consumer <- node.consumers){
        if(!propagatedToChild(coloredNodes(node)(0), consumer)){//case if we have not already propagated to the consumer
          if(!(fillerNodes.contains(node) && !fillerNodes.contains(consumer) && coloredNodes.contains(consumer) && (coloredNodes(consumer).length > 0))){//don't propagate to consumer if node is a fillerNode and consumer is not and consumer already has atleast 1 stage number
            if(consumer.getProducers.length <= 1){//case if consumer only consumes from the current node
              if(!coloredNodes.contains(consumer)){
                coloredNodes(consumer) = new ArrayBuffer[Int]
              }
              coloredNodes(consumer) += coloredNodes(node)(0)
              propagatedChildren += consumer
            } else {
              var childResolvedAllProducers = true
              var highestProducerStage = 0
              for(childProducer <- consumer.getProducers()){
                if(!requiresNoStageNum(childProducer)){
                  if(!coloredNodes.contains(childProducer)){
                    childResolvedAllProducers = false
                  } else if(coloredNodes(childProducer).length == 0) {
                    childResolvedAllProducers = false
                  } else{
                    if(coloredNodes(childProducer).max > highestProducerStage){
                      highestProducerStage = coloredNodes(childProducer).max
                    }
                  }
                }
              }
              if(childResolvedAllProducers){
                if(!coloredNodes.contains(consumer)){
                  coloredNodes(consumer) = new ArrayBuffer[Int]
                }
                //propagate stage to child
                if(!coloredNodes(consumer).contains(highestProducerStage)){
                  coloredNodes(consumer) += highestProducerStage  
                }
                //propagate child stage back to its consumers
                for(childProducer <- consumer.getProducers()){
                  if(!requiresNoStageNum(childProducer)){
                    if(!coloredNodes(childProducer).contains(highestProducerStage)){
                      coloredNodes(childProducer) += highestProducerStage  
                    }
                  }
                }
                propagatedChildren += consumer
              }
            }
          }
        }
      }
      for(child <- propagatedChildren){
        if(child.name == "pc_reg"){
          println("DEBUG3")
          println(node)
        }
      }
      propagatedChildren
    }
    
    //do initial pass to mark read and write points for regs
    for (reg <- ArchitecturalRegs) {
      regReadPoints += reg
      for(i <- reg.getProducers){
        regWritePoints += i
      }
    }
    
    //do initial pass to mark read and write ports for transactionMems
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
    
    //insert filler wires for easier stage propagation. ie make sure that there is a non-shared wire node between a combinational logic node and each of its inputs and consumers. Each filler node is gaurenteed to have only 1 producer and 1 consumer. This way we make sure that the pipeline boundary can always lie on these wire nodes, ensuring that we get a legal pipelining
    insertFillerWires()
    
    //initialize bfs queue and coloredNodes with user annotated nodes
    for((node, stage) <- annotatedStages){
      Predef.assert(!(regReadPoints.contains(node) && regWritePoints.contains(node)), "same node marked as both read point and write point")
      if(!regWritePoints.contains(node) && !tMemWritePoints.contains(node) && !outputNodes.contains(node) && !tMemReadAddrs.contains(node)){
        retryNodes += ((node, true))
      }
      if(!regReadPoints.contains(node) && !tMemReadDatas.contains(node) && !inputNodes.contains(node)){
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
          if(currentDirection){
            childrenPropagatedTo = propagateToConsumers(currentNode)
          } else {
            childrenPropagatedTo = propagateToProducers(currentNode)
          }
          for(child <- childrenPropagatedTo){
            if((coloredNodes(child).length < 2) && !regReadPoints.contains(child) && !tMemReadDatas.contains(child) && !inputNodes.contains(child) && !regWritePoints.contains(child) && !tMemWritePoints.contains(child) && !outputNodes.contains(child) && !tMemReadAddrs.contains(child)){
              bfsQueue.enqueue(((child, currentDirection)))
            }
          }
          if(!propagatedToAllChildren(currentNode, coloredNodes(currentNode)(0), currentDirection)){
            retryNodes += ((currentNode, currentDirection))
          }
        }
      }
    }
    println(regReadPoints)
    println(regWritePoints)
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
  
  //inserts additional bit node between *input* and its consumers(useful for automatic pipelining)
  def insertBitsOnOutputs(input: Node) : ArrayBuffer[Node] = {
    val newNodes = new ArrayBuffer[Node]()
    for(i <- 0 until input.consumers.size){
      val consumer = input.consumers(i)
      val producer_indices = new ArrayBuffer[Int]
      for(j <- 0 until consumer.inputs.size){
        if(consumer.inputs(j) == input) producer_indices += j
      }
      for(producer_index <- producer_indices){
        val new_bits = Bits()
        newNodes += new_bits
        consumer.inputs(producer_index) = new_bits
        new_bits.consumers += consumer
        new_bits.inputs += input
      }
    }
    input.consumers.clear()
    for(newNode <- newNodes){
      input.consumers += newNode
    }
    newNodes
  }
  
  //inserts additional bit node between *input* and its producers(useful for automatic pipelining)
  def insertBitsOnInputs(input: Node) = {
  val newNodes = new ArrayBuffer[Node]()
    for(i <- 0 until input.inputs.size){
      val new_bits = Bits()
      newNodes += new_bits
      val producer = input.inputs(i)
      val consumer_index = producer.consumers.indexOf(input)
      if(consumer_index > -1) producer.consumers(consumer_index) = new_bits
      new_bits.inputs += producer
      new_bits.consumers += input
      input.inputs(i) = new_bits
    }
    newNodes
  }
  
  
  def optimizeRegisterPlacement(coloredNodes: HashMap[Node, ArrayBuffer[Int]]) = {
    val regWritePoints = new HashSet[Node] //list of all register write inputs and transactionMem write port nodes(including write addr, write data, write en)
    val regReadPoints = new HashSet[Node] //list of all register read outputs and transactionMem read port
    val tMemWritePoints = new HashSet[Node] //list of all transactionMem write port nodes(including write addr, write data, write en)
    val tMemReadPoints = new HashSet[Node] //list of all transactionMem read port nodes(including read addr, read data, read en)
    val inputNodes = new HashSet[Node] //list of all module Input Nodes
    val outputNodes = new HashSet[Node] //list of all module Output Nodes
    
    val lastMovedNodes = new ArrayBuffer[(Node, ArrayBuffer[Int])]//keep track of nodes we just moved so that we can undo the move later
    
    var temp = 10000.0
    val coolRate = 0.03
    var iterCount = 0
    
    def acceptProbability(currentEnergy: Double, newEnergy: Double, temp: Double): Double = {
      if(newEnergy < currentEnergy){
        return 1
      } else {
        return Math.exp((currentEnergy-newEnergy)/temp)
      }
    }
    
    def findPaths(roots: ListBuffer[Node]): ListBuffer[ListBuffer[Node]] = {
      val result = new ListBuffer[ListBuffer[Node]]
      val unprocessedRoots = roots.clone()
      
      def traverse(currentNode: Node, path: ListBuffer[Node]): Unit = {
        currentNode match {
          case reg: Reg => {
            if(path.size > 0){
              result += path.clone()
            }
          }
          case node: Node => {
            if(coloredNodes.contains(node) && coloredNodes(node).length > 1 && (coloredNodes(node)(0) != coloredNodes(node)(1))){
              unprocessedRoots += node
              if(path.size > 0){
                result += path.clone()
              }
            } else if(!tMemReadPoints.contains(node) && !inputNodes.contains(node)){
              for(input <- node.inputs){
                val newPath = path.clone()
                newPath += node
                traverse(input, newPath)
              }
            } else{
              if(path.size > 0){
                result += path.clone()
              }
            }
          }
          case _ =>
        }
      }
      
      while(unprocessedRoots.size > 0){
        for(input <- unprocessedRoots(0).inputs){
          traverse(input, new ListBuffer[Node])
        }
        unprocessedRoots -= unprocessedRoots(0)
      }
      return result
    }
    
    def findEnergy(coloredNodes: HashMap[Node, ArrayBuffer[Int]]): Double = {
      val roots = new ListBuffer[Node]()
      for(node <- regWritePoints){
        roots += node
      }
      for(node <- tMemWritePoints){
        roots += node
      }
      for(node <- outputNodes){
        roots += node
      }
      val paths = findPaths(roots)
      var maxLength = 0.0
      for(path <- paths){
        var pathLength = 0.0
        for(node <- path){
          Predef.assert(!node.isInstanceOf[Reg], "register found in path")
          pathLength = pathLength + node.delay()
        }
        maxLength = Math.max(maxLength, pathLength)
      }
      return maxLength
    }
    
    //randomly move a combinational node across a pipeline boundary
    def getNewPlacement() = {
      if(Math.random > 0.5){//move node up a stage
        val eligibleNodes = new ArrayBuffer[Node]//list of nodes that can be legally moved up one stage. A node can be legally moved up one stage if all of its consumer nodes have a stage number greater than its stage number
        
        //find eligibleNodes
        for((node, stageNums) <- coloredNodes){
          if(!regWritePoints.contains(node) && !regReadPoints.contains(node) && !tMemWritePoints.contains(node) && !tMemReadPoints.contains(node) && !inputNodes.contains(node) && !outputNodes.contains(node) && !fillerNodes.contains(node) && coloredNodes(node)(0) < pipelineLength - 1) {
            val nodeStage = coloredNodes(node)(0)
            var consumersEligible = true
            Predef.assert(coloredNodes(node).length <= 1)
            for(consumer <- node.consumers){
              if(!requiresNoStageNum(consumer)){
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
            if(!requiresNoStageNum(input)){
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
            if(!requiresNoStageNum(consumer)){
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
        for((node, stageNums) <- coloredNodes){
          
          if(!regWritePoints.contains(node) && !regReadPoints.contains(node) && !tMemWritePoints.contains(node) && !tMemReadPoints.contains(node) && !inputNodes.contains(node) && !outputNodes.contains(node) && !fillerNodes.contains(node) && coloredNodes(node)(0) > 0) {
            val nodeStage = coloredNodes(node)(0)
            var producersEligible = true
            Predef.assert(coloredNodes(node).length <= 1)
            for(producer <- node.inputs){
              if(!requiresNoStageNum(producer)){
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
            if(!requiresNoStageNum(input)){
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
            if(!requiresNoStageNum(consumer)){
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
    
    //do initial pass to mark read and write points for regs
    for(p <- procs){
      p match {
        case r: Reg => {
          for(i <- r.getProducers()){
            regWritePoints += i
          }
          regReadPoints += r
        }
        case _ =>
      }
    }
    //do initial pass to mark read and write ports for transactionMems
    for(tmem <- TransactionMems){
      for(i <- 0 until tmem.readPortNum){
        tMemReadPoints += tmem.io.reads(i).adr
        tMemReadPoints += tmem.io.reads(i).dat
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
    
    println("max path delay before optimization:")
    println(findEnergy(coloredNodes))
    while(temp > 0.01 && iterCount < 1000){
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
    }
    println("max path delay after optimization:")
    println(findEnergy(coloredNodes))
  }
  
  def insertPipelineRegisters() = {
    for(stage <- 0 until pipeline.size) {
      /*val valid = Reg(init = Bool(false))
      valids += valid
      if (stage > 0) 
        valid := valids(stage-1)
      else
        valid := Bool(true)
      valid.setName("HuyValid_" + stage)*/
      for ((p, enum) <- pipeline(stage) zip pipeline(stage).indices) {
        /*val r = Reg(Bits())
        r := p._1.asInstanceOf[Bits]
        r.comp.asInstanceOf[Reg].clock = p._1.component.clock
        when(p._1.component._reset){
          r := p._2
        }
        //add pointer in p._1 to point to pipelined version of itself
        p._1.pipelinedVersion = r
        r.unPipelinedVersion = p._1
        r.comp.setName("Huy_" + enum + "_" + p._1.name)
        pipelineReg(stage) += r.comp.asInstanceOf[Reg]*/
        pipelineReg(stage) += insertRegister(p._1.asInstanceOf[Bits], p._2.asInstanceOf[Bits], "Huy_" + enum + "_" + p._1.name).comp.asInstanceOf[Reg]
        /*for (c <- p._1.consumers) {
          val producer_ind = c.inputs.indexOf(p._1)
          if(producer_ind > -1) c.inputs(producer_ind) = r
          val consumer_ind = p._1.consumers.indexOf(c)
          r.consumers += c
          p._1.consumers(consumer_ind) = r
        }*/
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
        val prevWriteEns = getVersions(writeEn)
        println("DEBUG1")
        println(getStage(writeEn))
        println(getStage(writeData))
        println(readStage)
        val writeStage = Math.max(getStage(writeEn), getStage(writeData))
        Predef.assert(writeStage > -1, "both writeEn and writeData are literals")
        if (writeStage > readStage) {
          for(stage <- readStage + 1 until writeStage + 1) {
            var currentStageWriteEnable = Bool(true)
            if(-(stage - writeStage)  <= prevWriteEns.length){
              currentStageWriteEnable = prevWriteEns(-(stage - writeStage) ).asInstanceOf[Bool]
            }
            regRAWHazards(((reg, i, stage))) = stageValids(writeStage) && currentStageWriteEnable
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
        /*val writeAddr = writePoint.adr.inputs(0).inputs(0).inputs(1)
        val writeEn = writePoint.is.inputs(0).inputs(0).inputs(1)
        val writeData = writePoint.dat.inputs(0).inputs(0).inputs(1)*/
        val writeAddr = writePoint.actualWaddr
        val writeEn = writePoint.actualWen
        val writeData = writePoint.actualWdata
        val writeStage = Math.max(getStage(writeEn),Math.max(getStage(writeAddr), getStage(writeData)))
        Predef.assert(writeStage > -1)
        val writeEnables = getVersions(writeEn.asInstanceOf[Bool])
        val writeAddrs = getVersions(writeAddr.asInstanceOf[Bits])
        for(j <- 0 until m.io.reads.length){
          val readPoint = m.io.reads(j)
          val readAddr = readPoint.adr
          val readData = readPoint.dat
          val readStage = Math.max(getStage(readAddr), getStage(readData))
          Predef.assert(readStage > -1)
          if(writeStage > readStage){
            for(stage <- readStage + 1 until writeStage + 1){
              var currentStageWriteEnable = Bool(true)
              var currentStageWriteAddr:Data = readAddr
              if(-(stage - writeStage)  <= writeEnables.length){
                currentStageWriteEnable = writeEnables(-(stage - writeStage) ).asInstanceOf[Bool]
              }
              if(-(stage - writeStage)  <= writeAddrs.length){
                currentStageWriteAddr = writeAddrs(-(stage - writeStage) ).asInstanceOf[Data]
              }
              tMemRAWHazards(((readPoint, i, stage))) = stageValids(writeStage) && currentStageWriteEnable && (readAddr === currentStageWriteAddr)
              tMemRAWHazards(((readPoint, i, stage))).nameIt("hazard_num" + raw_counter + "_" +m.name + "_readport_num" + j + "_"+ stage + "_" + writeEn.name)
              raw_counter = raw_counter + 1
              println("found hazard" + writeEn.line.getLineNumber + " " + writeEn.line.getClassName + " " + writeEn.name + " " + m.name)
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
  
  def generateInterlockLogic() = {
    //initialize stall signals
    val stageStalls = new ArrayBuffer[Bool]
    for (i <- 0 until pipelineLength) {
      val stall = Bool(false)
      stageStalls += stall
      stall.nameIt("PipeStageStall_" + i)
    }
    
    
    //initialize registers for stageValid signals
    val validRegs = new ArrayBuffer[Bool]
    for (i <- 0 until pipelineLength - 1) {
      val validReg = Reg(Bool())
      when(~stageStalls(i)){
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
    tempValids += Bool(true)
    for(i <- 1 until pipelineLength){
      tempValids += validRegs(i - 1)
    }
    
    //collect RAW hazards for valids and stalls for every stage
    //reg RAW hazards
    for ((key, value) <- regRAWHazards){
      val reg = key._1
      val RAWHazardSignal = value
      val readStage = getStage(reg)
      tempValids(readStage) = tempValids(readStage) && ~RAWHazardSignal
      if(readStage > 0 && readStage < pipelineLength){
        stageStalls(readStage - 1) = stageStalls(readStage - 1) || RAWHazardSignal
      }
    }
    
    //transactionMem RAW hazards
    for ((key, value) <- tMemRAWHazards){
      val readAddr = key._1.adr
      val RAWHazardSignal = value
      val readStage = getStage(readAddr)
      tempValids(readStage) = tempValids(readStage) && ~RAWHazardSignal
      if(readStage > 0 && readStage < pipelineLength ){
        stageStalls(readStage - 1) = stageStalls(readStage - 1) || RAWHazardSignal
      }
    }
    
    //connect actual stage valids to the tempValids
    for(i <- 0 until pipelineLength){
      stageValids(i) := tempValids(i)
    }
    
    //generate logic for stall signals
    for(i <- (0 until pipelineLength - 2).reverse) {
      stageStalls(i) = stageStalls(i) || stageStalls(i+1)
    }

    //wire stage valid and stall signals to architecural state write enables
    //regs
    for(reg <- ArchitecturalRegs){
      val writeStage = reg.updates.map(_._2).map(getStage(_)).filter(_ > - 1)(0)
      for (i <- 0 until reg.updates.length){
        val writeEn = reg.updates(i)._1
        reg.updates(i) = ((writeEn && ~globalStall && stageValids(writeStage) && ~stageStalls(writeStage), reg.updates(i)._2))
      }
      reg.genned = false
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
  }
  
  /*def findHazards() = {
    println("searching for hazards...")
    val comp = pipelineComponent
    //nodeToStageMap = colorPipelineStages()

    // handshaking stalls
    globalStall = Bool(false)
    for (tc <- VariableLatencyComponents) {
      val stall = tc.io.req.valid && (!tc.req_ready || !tc.resp_valid)
      globalStall = globalStall || stall
    }
    
    // raw stalls
    val speArchitecturalRegs = speculation.map(_._1.comp.asInstanceOf[Reg])
    ArchitecturalRegs = ArchitecturalRegs.filter(!speArchitecturalRegs.contains(_))
    for (p <- ArchitecturalRegs) {
      if (p.updates.length > 1 && nodeToStageMap.contains(p)) {
        val enables = p.updates.map(_._1).filter(_.name != "reset")
        val enStgs = enables.map(getStage(_)).filter(_ > -1)
        val stage = enStgs.head
        
        scala.Predef.assert(enStgs.tail.map( _ == stage).foldLeft(true)(_ && _), println(p.name + " " + p.line.getLineNumber + " " + p.line.getClassName + " " + enStgs)) // check all the stgs match
        val rdStg = getStage(p)
        for (en <- enables) {
          val wrStg = getStage(en)
          if (wrStg > rdStg) {
            if (wrStg - rdStg > 1) {
              val rdStgValid = if (rdStg == 0) Bool(true) else valids(rdStg-1)
              for (stg <- rdStg + 1 until wrStg) {
                hazards += ((rdStgValid && valids(stg-1), p, rdStg, stg, Bool(true), null))
              }
            }
            hazards += (((en && (if (wrStg > 0) valids(wrStg-1) else Bool(true))), p, rdStg, wrStg, Bool(true), null))
            println("found hazard " + en.line.getLineNumber + " " + en.line.getClassName)
          }
        }
      }
    }
    // FunMem hazards
    for (m <- TransactionMems) {
      for(i <- 0 until m.io.writes.length){
        val writePoint = m.io.writes(i)
        val writeAddr = writePoint.adr.inputs(0).inputs(1)
        val writeEn = writePoint.is.inputs(0).inputs(1)
        val writeData = writePoint.dat.inputs(0).inputs(1)
        val writeStage = getStage(writeEn)
        val writeEnables = getVersions(writeEn.asInstanceOf[Bool])
        val writeAddrs = getVersions(writeAddr.asInstanceOf[Bits])
        Predef.assert(getStage(writeEn) == getStage(writeData) || getStage(writeEn) == -1 || getStage(writeData) == -1, "writeEN stage" + "(" + writeEn.name + "): " + getStage(writeEn) + " writeData stage" + "(" + writeData + "): " + getStage(writeData))
        Predef.assert(getStage(writeData) == getStage(writeAddr) || getStage(writeAddr) == -1 || getStage(writeData) == -1, "writeData stage: " + getStage(writeData) + " writeAddr stage: " + getStage(writeAddr))
        var foundHazard = false
        var readStage = -1
        for(readPoint <- m.io.reads){
          val readAddr = readPoint.adr
          readStage = getStage(readAddr)
          if(writeStage > 0 && readStage > 0 && writeStage > readStage){
            Predef.assert((getStage(writeEnables.last) - readStage) == 1, println(writeEnables.length))
            Predef.assert(writeEnables.length == writeAddrs.length, println(writeEnables.length + " " + writeAddrs.length))
            for((en, waddr) <- writeEnables zip writeAddrs){
              hazards += ((en.asInstanceOf[Bool] && waddr === readPoint.adr, null, readStage, getStage(en), en.asInstanceOf[Bool], readPoint))
              println("found hazard" + en.line.getLineNumber + " " + en.line.getClassName + " " + en.name + " " + m.name)
            }
          }
        }
      }
    }
  }*/
  
  def getVersions(node: Node): ArrayBuffer[Node] = {
    val result = new ArrayBuffer[Node]//stage of node decreases as the array index increases
    var currentNode = node
    result += currentNode
    while(currentNode.inputs.length == 1){
      if(currentNode.inputs(0).isInstanceOf[Reg]){
        if(!isPipeLineReg(currentNode)){
          result += currentNode
        } else {
          return result
        }
        currentNode = currentNode.inputs(0).asInstanceOf[Reg].updates(0)._2
      } else {
        currentNode = currentNode.inputs(0)
      } 
    }
    result
  }

  /*def resolveHazards() = {

    // raw stalls
    for ((hazard, s, rdStg, wrStg, wEn, rPort) <- hazards) {
      stalls(rdStg) += hazard
      kills(rdStg) += hazard
    }

    // back pressure stalls
    for (stg <- 0 until stalls.size)
      for (nstg <- stg + 1 until stalls.size)
        stalls(stg) ++= stalls(nstg)

    for ((s, v) <- speculation)
      ArchitecturalRegs += s.comp.asInstanceOf[Reg]

    val wStageMap = new HashMap[Reg, Int]()

    for ((r, ind) <- (ArchitecturalRegs zip ArchitecturalRegs.indices)) {
      if (r.updates.length > 1 && nodeToStageMap.contains(r)) {
        val enStg = r.updates.map(_._1).map(getStage(_)).filter(_ > -1)(0)
        wStageMap += (r -> enStg)
        var mask = Bool(false)
        if(stalls(enStg).length > 0)
          mask = mask || stalls(enStg).reduceLeft(_ || _) // raw
        if (enStg > 0)
          mask = mask || !valids(enStg-1) // no transaction
        if (VariableLatencyComponents.length > 0) mask = mask || globalStall
        mask.nameIt("HuyMask_" + ind)
        for (i <- 0 until r.updates.length) {
          val en = r.updates(i)._1
          r.updates(i) = ((en && ! mask, r.updates(i)._2))
        }
        r.genned = false
      }
    }

    // speculation stuff
    for ((s, v) <- speculation) {
      val sStage = getStage(s)
      val reg = s.comp.asInstanceOf[Reg]
      val wStage = wStageMap(reg)
      var mask = stalls(sStage).foldLeft(Bool(false))(_ || _) || globalStall // stall

      var spec = v
      for (stg <- sStage until wStage) {
        val specReg = Reg(init = Bits(0))
        specReg := spec
        specReg.nameIt("Huy_specReg_" + stg)
        pipelineReg(stg) += specReg.comp.asInstanceOf[Reg]
        spec = specReg
      }

      val b = Bits()
      b.inputs += s.comp.inputs(0)
      val kill = (valids(wStage-1) && spec != b)
      for (i <- 0 until reg.updates.length) {
        val en = reg.updates(i)._1
        reg.updates(i) = ((en && kill, reg.updates(i)._2))
      }
      val default = !mask && !kill
      default.nameIt("Huy_doSpec")
      reg.updates += ((default, v))
      for (stg <- sStage until wStage)
        speckills(stg) += kill
    }

    insertBubble(globalStall)
    
    for (m <- cMems) {
      for (wprt <- m.writes) {
        wprt.inputs(1) = wprt.inputs(1).asInstanceOf[Bool] && (!stalls(getStage(wprt.inputs(1))).foldLeft(Bool(false))(_ || _)).asInstanceOf[Bool] && !(globalStall.asInstanceOf[Bool])
      }
    }
  }*/

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
  
  /*def generateForwardingLogic() = {
    def getPipelinedVersion(n: Node): Node = {
      var result = n
      if (n.pipelinedVersion != null){
        result = n.pipelinedVersion
      }
      result
    }
    var consumerMap = getConsumers()
    for(r <- forwardedRegs){
      val forwardPoints = new HashMap[Int, ArrayBuffer[(Node,Node)]]()
      for (i <- nodeToStageMap(r) + 1 to pipelineReg.size){
        forwardPoints(i) = new ArrayBuffer[(Node,Node)]()
      } 
      for ((writeEn, writeData) <- r.updates){Predef.assert(nodeToStageMap(writeEn) == nodeToStageMap(writeData))
        val writeEns = getVersions(writeEn)
        val writeDatas = getVersions(writeData.asInstanceOf[Bits])
        val numStagesAvail = Math.min(writeEns.length, writeDatas.length)
        for(i <- 0 until numStagesAvail) {
          forwardPoints(nodeToStageMap(writeEn) - i) += ((writeEns(i), writeDatas(i)))
        }
      }
      val muxMapping = new ArrayBuffer[(Bool, Bits)]()
      for(i <- nodeToStageMap(r) + 1 to pipelineReg.size){
        //generate muxes
        if(!forwardPoints(i).isEmpty){     
          for((j,k) <- forwardPoints(i)){
            muxMapping += ((j.asInstanceOf[Bool], k.asInstanceOf[Bits]))
            //append forward condition to hazards list
            for((cond, state, rStage, wStage, wEn, rPort) <- hazards){
              if(state == r && wStage == i){
                hazards -= ((cond, state, rStage, wStage, wEn, rPort))
              }
            }
          }
        }
      }
      val bypassMux = MuxCase(consumerMap(r)(0).asInstanceOf[Bits],muxMapping)
      for (n <- consumerMap(consumerMap(r)(0))){
        n.replaceProducer(consumerMap(r)(0), bypassMux)
      }
    }
    for((fm, rp) <- forwardedMemReadPoints){
      val forwardPoints = new HashMap[Int, ArrayBuffer[(Node, Node, Node)]]()
      for (i <- nodeToStageMap(rp.adr) + 1 to pipelineReg.size){
        forwardPoints(i) = new ArrayBuffer[(Node,Node,Node)]()
      }
      for(i <- 0 until fm.io.writes.length){
        val writePoint = fm.io.writes(i)
        if(!memNonForwardedWritePoints.contains(writePoint)){
          val delayedWriteEn = getPipelinedVersion(writePoint.is.inputs(0).inputs(1))
          val delayedWriteData = getPipelinedVersion(writePoint.dat.asInstanceOf[Node].inputs(0).inputs(1))
          val delayedWriteAddr = getPipelinedVersion(writePoint.adr.inputs(0).inputs(1))
          Predef.assert(nodeToStageMap(delayedWriteEn) == nodeToStageMap(delayedWriteData))
          Predef.assert(nodeToStageMap(delayedWriteEn) == nodeToStageMap(delayedWriteAddr))
          val writeEns = getVersions(delayedWriteEn.asInstanceOf[Bits])
          val writeDatas = getVersions(delayedWriteData.asInstanceOf[Bits])
          val writeAddrs = getVersions(delayedWriteAddr.asInstanceOf[Bits])
          val numStagesAvail = Math.min(writeEns.length, Math.min(writeDatas.length, writeAddrs.length))
          for(i <- 0 until numStagesAvail){
            println("found fowarding point ("+ writeEns(i) + "," + writeDatas(i) + "," + writeAddrs(i) + ")")
            forwardPoints(nodeToStageMap(delayedWriteEn) - i) += ((writeEns(i), writeDatas(i), writeAddrs(i))) 
          }
        }
      }
      val muxMapping = new ArrayBuffer[(Bool, Bits)]()
      for(i <- nodeToStageMap(rp.adr) + 1 to pipelineReg.size){
        //generate muxes
        if(!forwardPoints(i).isEmpty){
          for((writeEn, writeData, writeAddr) <- forwardPoints(i)){
            val forwardCond = writeEn.asInstanceOf[Bool] & writeAddr.asInstanceOf[Bits] === rp.adr.asInstanceOf[Bits]
            muxMapping += ((forwardCond, writeData.asInstanceOf[Bits]))
            //append forward condition to hazards list
            for((cond, state, rStage, wStage, wEn, rPort) <- hazards){
              if(wStage == i & wEn == writeEn.asInstanceOf[Bool] & rp == rPort){
                hazards -= ((cond, state, rStage, wStage, wEn, rPort))
              }
            }
          }
        }
      }
      val bypassMux = MuxCase(rp.dat.asInstanceOf[Bits], muxMapping)
      for (n <- consumerMap(rp.dat.asInstanceOf[Node])){
        n.replaceProducer(rp.dat.asInstanceOf[Node], bypassMux)    
      }
    }
  }*/
  
  def verifyLegalStageColoring() = {
    this.bfs((n: Node) => {
      n match {
        case reg: Reg => {//check all architectural reg read and write ports are annotated; data and enable of each port is in same stage; all write ports are in same stage; write ports have stage >= read port stage
          if(reg.component == pipelineComponent && !isPipeLineReg(reg)){
            val readStage = getStage(reg)
            val writeEnables = reg.updates.map(_._1).filter(_.name != "reset")
            val writeDatas = reg.updates.filter(_._1.name != "reset").map(_._2)
            val writeEnableStages = writeEnables.map(getStage(_)).filter(_ > -1)
            val writeDataStages = writeDatas.map(getStage(_)).filter(_ > -1)
            
            Predef.assert(writeDataStages.length > 0, "register written by all constants; this case is not handled yet:" + writeDataStages)
            val writeStage = writeDataStages.head
            Predef.assert(writeStage >= readStage)
            if(writeEnableStages.length > 0){
              Predef.assert(writeEnableStages.tail.map( _ == writeStage).foldLeft(true)(_ && _), reg.name + " " + reg.line.getLineNumber + " " + reg.line.getClassName + " " + writeEnableStages) 
            }
            Predef.assert(writeDataStages.tail.map( _ == writeStage).foldLeft(true)(_ && _), reg.name + " " + reg.line.getLineNumber + " " + reg.line.getClassName + " " + writeDataStages)
            
          }
        }
        case op: Op => {//check all combinational nodes have inputs coming from same stage
          if(op.component == pipelineComponent && !op.isInstanceOf[Mux]){//hack because muxes generated for pipeline register reset values don't have stages
            val opStage = getStage(op)
            for(input <- op.inputs){
              val inputStage = getStage(input)
              Predef.assert(inputStage == opStage || inputStage == -1, "combinational node input does not have stage number")
            }
          }
        }
        case _ =>
      }
    })
    
    
    for(tmem <- TransactionMems){//check all tmems read and write ports are annotated; data, addr, and enable of each port is in same stage; all read ports are in the same stage; all write ports are in same stage; write ports have stage >= read port stage
      val readPorts = tmem.io.reads
      val writePorts = tmem.io.writes
      
      val readStage = getStage(readPorts(0).dat)
      Predef.assert(getStage(readPorts(0).adr) == readStage, "transactionMem readport nodes do not all have same stage numbers")
      for(readPort <- readPorts){
        Predef.assert(getStage(readPort.adr) == readStage, "transactionMem readport nodes do not all have same stage numbers")
        Predef.assert(getStage(readPort.dat) == readStage, "transactionMem readport nodes do not all have same stage numbers")
      }
       
      val writeStage = getStage(writePorts(0).dat)
      Predef.assert(getStage(writePorts(0).adr) == writeStage, "transactionMem writeport nodes do not all have same stage numbers")
      for(writePort <- writePorts){
        Predef.assert(getStage(writePort.is) == writeStage, "transactionMem writeport nodes do not all have same stage numbers")
        Predef.assert(getStage(writePort.adr) == writeStage, "transactionMem writeport nodes do not all have same stage numbers")
        Predef.assert(getStage(writePort.dat) == writeStage, "transactionMem writeport nodes do not all have same stage numbers")
      } 
      
      Predef.assert(writeStage >= readStage, "transactionMem has writePort at earlier stage than readPort")         
    }
    
    //check that all IO nodes are in the same stage
    val ioNodes = pipelineComponent.io.flatten.map(_._2)
    if(ioNodes.length > 0){
      val IOstage = getStage(ioNodes(0))
      for(io <- ioNodes) {
        Predef.assert(nodeToStageMap(io) == IOstage, "IO nodes do not all belong to the same stage")
      }
    }

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

