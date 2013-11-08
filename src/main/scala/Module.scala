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
import scala.collection.mutable.ArrayBuffer
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
  val blackboxes = ArrayBuffer[BlackBox]();
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
    for(b <- blackboxes)
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
    for (b <- blackboxes)
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
      /* This is ugly and most likely unnecessary but as long as we are not
       sure of the subtle consequences of tracing through blackboxes, let's
       have the code here (instead of Verilog.doCompile). */
      for (c <- Module.components) {
        c match {
          case x: BlackBox => c.traceNodes();
          case _ =>
        }
      }
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
    for (b <- blackboxes)
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
    
  // automatic pipeline stuff
  var pipeline = new HashMap[Int, ArrayBuffer[(Node, Bits)]]()
  val chckStg = new ArrayBuffer[Bits]()
  var pipelineComponent: Module = null
  var pipelineReg = new HashMap[Int, ArrayBuffer[Reg]]()
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
  def addPipeReg(stage: Int, n: Node, rst: Bits) = {
    pipeline(stage) += (n -> rst)
  }
  val forwardedRegs = new HashSet[Reg]
  val forwardedMemReadPoints = new HashSet[(TransactionMem[_], FunRdIO[_])]
  val memNonForwardedWritePoints = new HashSet[FunWrIO[_]]
  def addForwardedReg(d: Reg) = {
    forwardedRegs += d
  }
  def addForwardedMemReadPoint(m: TransactionMem[_], r: FunRdIO[_]) = {
    forwardedMemReadPoints += ((m.asInstanceOf[TransactionMem[Data]], r.asInstanceOf[FunRdIO[Data]]))
  }
  def addNonForwardedMemWritePoint[T <: Data] (writePoint: FunWrIO[T]) = {
    memNonForwardedWritePoints += writePoint
  }

  val speculation = new ArrayBuffer[(Bits, Bits)]
  def speculate(s: Bits, v: Bits) = {
    speculation += ((s, v))
  }
  //new syntax stuff
  var nodeStages = new ArrayBuffer[(Node, Int)]()
  var conflictNodes = new ArrayBuffer[(Node, Int)]()
  def annotateNodeStage(n: Node, s:Int) = {
    nodeStages += ((n,s))
  }
  val tcomponents = new ArrayBuffer[TransactionalComponent]()
  var stages: HashMap[Node, Int] = new HashMap[Node, Int]()
  var cRegs: ArrayBuffer[Reg] = null
  var cMems: ArrayBuffer[Mem[ Data ]] = null
  var cTransactionMems: ArrayBuffer[TransactionMem[ Data ]] = null
  def getStage(n: Node): Int = {
    if (stages.contains(n))
      return stages(n)
    else
      return -1
  }
  val valids = new ArrayBuffer[Bool]
  val stalls = new HashMap[Int, ArrayBuffer[Bool]]
  val kills = new HashMap[Int, ArrayBuffer[Bool]]
  val speckills = new HashMap[Int, ArrayBuffer[Bool]]
  var globalStall: Bool = null
  val hazards = new ArrayBuffer[(Bool, Delay, Int, Int, Bool, FunRdIO[Data])]

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
  
  def insertPipelineRegisters2() = { 
    val coloredNodes = propagateStages()
    println("inserting pipeline registers")
    val consumerMap = getConsumers()
    var maxStage = 0
    for((node, stages) <- coloredNodes){
      if(stages.length > 0 && stages.max > maxStage){
        maxStage = stages.max
      }
    }
    /*
    for(stage <- 0 until maxStage){
      pipelineReg(stage) = new ArrayBuffer[Reg]
    }
    for (i <- 0 until maxStage + 1) {
      val valid = Reg(init = Bool(false))
      valids += valid
      if(i > 0){
        valid := valids(i - 1)
      } else {
        valid := Bool(true)
      }
      valid.nameIt("HuyValid_" + i)
      stalls += (i -> new ArrayBuffer[Bool])
      kills += (i -> new ArrayBuffer[Bool])
      speckills += (i -> new ArrayBuffer[Bool])
    }*/
    setNumStages(maxStage + 1)
    for(stage <- 0 until pipeline.size) {
      val valid = Reg(init = Bool(false))
      valids += valid
      if (stage > 0) 
        valid := valids(stage-1)
      else
        valid := Bool(true)
      valid.nameIt("HuyValid_" + stage)
    }
    for((node,stages) <- coloredNodes){
      Predef.assert(stages.length <= 2, stages)
      if(stages.length > 1){
        val stageDifference = Math.abs(stages(1) - stages(0))
        Predef.assert(stageDifference > 0, stageDifference)
        var actualNode = node
        if(node.isInstanceOf[Op]){
          actualNode = consumerMap(node)(0)
        }
        var currentNodeOut = actualNode
        for(i <- 0 until stageDifference){
          val r = Reg(init = Bits(0))
          r := currentNodeOut.asInstanceOf[Bits]
          currentNodeOut.pipelinedVersion = r
          r.unPipelinedVersion = currentNodeOut
          r.nameIt("Huy_" + Math.min(stages(0),stages(1) + i) + "_" + currentNodeOut.name)
          pipelineReg(Math.min(stages(0),stages(1)) + i) += r.comp.asInstanceOf[Reg]
          currentNodeOut = r
        }
        
        for(c <- consumerMap(actualNode)){
          val producerIndex = c.inputs.indexOf(actualNode)
          if(producerIndex > -1) c.inputs(producerIndex) = currentNodeOut
        }
      }
    }
  }
  
  def propagateStages() = {
    val consumerMap = getConsumers()
    val coloredNodes = new HashMap[Node, ArrayBuffer[Int]]
    val writePoints = new HashSet[Node]
    val bfsQueue = new ScalaQueue[Node]
    val conflicts = new HashMap[Node, ArrayBuffer[Int]]
    val visited = new HashSet[Node]
    val unresolvedNodes = new HashSet[Node]
    val propagatedNodes = new HashMap[Node, ArrayBuffer[Node]]
    var oldPropagatedNodes = new HashMap[Node, ArrayBuffer[Node]]
    def propagateToProducers(cur: Node) = {  
      val currentNodeStages = coloredNodes(cur)
      var unMarkedChild = false
      for(n <- cur.inputs){
        if(!n.isInstanceOf[Mem[_]]){//don't propagate to Mem nodes
          //enqueue children if not enqueued already
          if(!visited.contains(n) && n.litOf == null){
            visited += n
            coloredNodes(n) = new ArrayBuffer[Int]()
            bfsQueue.enqueue(n)
            propagatedNodes(n) = new ArrayBuffer[Node]()
          }
          //attempt to propagate cur's stage to its children
          if(consumerMap.contains(n)){
            if(n.litOf != null){
              propagatedNodes(cur) += n
            } else if(consumerMap(n).length <= 1){//case if child only produces to current node
              if(!coloredNodes(n).contains(currentNodeStages(0))){
                coloredNodes(n) += currentNodeStages(0)
              }
              propagatedNodes(cur) += n
            } else {
              var childResolvedAllConsumers = true
              var lowestConsumerStage = Int.MaxValue
              for(nc <- consumerMap(n)){
                if(!coloredNodes.contains(nc)){
                  childResolvedAllConsumers = false
                } else if(coloredNodes(nc).length == 0){
                  childResolvedAllConsumers = false
                } else if(nc.litOf == null) {//only look at node if is not a constant
                  if(coloredNodes(nc).min < lowestConsumerStage){
                    lowestConsumerStage = coloredNodes(nc).min
                  }
                }
              }
              if(childResolvedAllConsumers){
                if(!coloredNodes(n).contains(lowestConsumerStage)){
                  //propagate stage to child
                  coloredNodes(n) += lowestConsumerStage
                }
                //propagate child stage back to its consumers
                for(nc <- consumerMap(n)){
                  if(!coloredNodes(nc).contains(lowestConsumerStage)){
                    coloredNodes(nc) += lowestConsumerStage
                  }
                }
                propagatedNodes(cur) += n
              } 
            }
          } else {
            if(!coloredNodes(n).contains(currentNodeStages(0))){
              coloredNodes(n) += currentNodeStages(0)
            }
            propagatedNodes(cur) += n
          }
          //check to see if all children have been propagated to
          if(!propagatedNodes(cur).contains(n)){
            unMarkedChild = true
          }
        }
      }
      unMarkedChild
    }
    def propagateToConsumers(cur: Node) = {  
      val currentNodeStages = coloredNodes(cur)
      var unMarkedChild = false
      if(consumerMap.contains(cur)){
        for(n <- consumerMap(cur)){
          if(!n.isInstanceOf[Mem[_]]){//don't propagate to Mem nodes
            //enqueue children if not enqueued already
            if(!visited.contains(n) && n.litOf == null){
              visited += n
              coloredNodes(n) = new ArrayBuffer[Int]()
              bfsQueue.enqueue(n)
              propagatedNodes(n) = new ArrayBuffer[Node]()
            }
            //attempt to propagate cur's stage to its children
            if(n.litOf != null){//don't propagate stage to constants
              propagatedNodes(cur) += n
            } else if(n.inputs.length <= 1){//case if child only consumes from current node
              if(!coloredNodes(n).contains(currentNodeStages(0))){
                coloredNodes(n) += currentNodeStages(0)
              }
              propagatedNodes(cur) += n
            } else {
              var childResolvedAllProducers = true
              var highestProducerStage = 0
              for(nc <- n.inputs){
                if(!coloredNodes.contains(nc)){
                  childResolvedAllProducers = false
                } else if(coloredNodes(nc).length == 0){
                  childResolvedAllProducers = false
                } else if(nc.litOf == null){//only look at node if is not a constant
                  if(coloredNodes(nc).max > highestProducerStage){
                    highestProducerStage = coloredNodes(nc).max
                  }
                }
              }
              if(childResolvedAllProducers){
                if(!coloredNodes(n).contains(highestProducerStage)){
                  //propagate stage to child
                  coloredNodes(n) += highestProducerStage
                }
                //propagate child stage back to its consumers
                for(nc <- n.inputs){
                  if(!coloredNodes(nc).contains(highestProducerStage)){
                    coloredNodes(nc) += highestProducerStage
                  }
                }
                propagatedNodes(cur) += n
              } 
            }
            //check to see if all children have been propagated to
            if(!propagatedNodes(cur).contains(n)){
              unMarkedChild = true
            }
          }
        }
      }
      unMarkedChild
    }
    //do initial pass to mark write points for regs
    for(p <- procs){
      p match {
        case r: Reg => {
          for(i <- r.getProducers()){
            writePoints += i
          }
        }
        case _ =>
      }
    }
    
    //initialize bfs queue and coloredNodes with user annotated nodes
    for(i <- nodeStages){
      unresolvedNodes += i._1
      coloredNodes(i._1) = new ArrayBuffer[Int]()
      coloredNodes(i._1) += i._2
      visited += i._1
      propagatedNodes(i._1) = new ArrayBuffer[Node]()
    }
    while(!unresolvedNodes.isEmpty && !oldPropagatedNodes.equals(propagatedNodes)){
      for(n <- unresolvedNodes){
        bfsQueue.enqueue(n)
        visited += n
      }
      oldPropagatedNodes = propagatedNodes.clone()
      unresolvedNodes.clear
      while(!bfsQueue.isEmpty){
        val currentNode = bfsQueue.dequeue
        val currentNodeStages = coloredNodes(currentNode)
        if(currentNodeStages.length == 0){//can't propagate if current nodes doesn't have stage
          unresolvedNodes += currentNode
        } else if(currentNodeStages.length == 1){//propagate if current node has stage and does not have stage conflict
          var unMarkedChild = false
          if(writePoints.contains(currentNode)){//case for write points
            unMarkedChild = propagateToProducers(currentNode)
          } else {
            currentNode match {
              case r: Reg => {//case for read points
                unMarkedChild = propagateToConsumers(r)
              }
              case m: Mem[_] => {//don't do anything for mems
              }
              case c: Node => {//case for combinational nodes
                val unMarkedChild1 = propagateToProducers(c)
                val unMarkedChild2 = propagateToConsumers(c) 
                unMarkedChild = unMarkedChild1 || unMarkedChild2
              }
            }
          }
          //keep currentNode around if not all of its children have been propagated to
          if(unMarkedChild){
            unresolvedNodes += currentNode
          }
        }
        Predef.assert(currentNodeStages.length < 3, "propagate stages: node has too many stages: " + currentNodeStages)
      }
    }
    coloredNodes
  }
  
  def insertPipelineRegisters() = {
    val map = getConsumers()
    for(stage <- 0 until pipeline.size) {
      val valid = Reg(init = Bool(false))
      valids += valid
      if (stage > 0) 
        valid := valids(stage-1)
      else
        valid := Bool(true)
      valid.nameIt("HuyValid_" + stage)
      for ((p, enum) <- pipeline(stage) zip pipeline(stage).indices) {
        val r = Reg(init = p._2)
        r := p._1.asInstanceOf[Bits]
        //add pointer in p._1 to point to pipelined version of itself
        p._1.pipelinedVersion = r
        r.unPipelinedVersion = p._1
        r.nameIt("Huy_" + enum + "_" + p._1.name)
        pipelineReg(stage) += r.comp.asInstanceOf[Reg]
        val consumers = map(p._1)
        for (c <- consumers) {
          val ind = c.inputs.indexOf(p._1)
          if(ind > -1) c.inputs(ind) = r
        }
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
    stages = coloredNodes
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
    val comp = pipelineComponent
    //stages = colorPipelineStages()

    // handshaking stalls
    globalStall = Bool(false)
    for (tc <- tcomponents) {
      val stall = tc.io.req.valid && (!tc.req_ready || !tc.resp_valid)
      globalStall = globalStall || stall
    }

    cRegs = new ArrayBuffer[Reg]
    cMems = new ArrayBuffer[Mem[Data]]
    cTransactionMems = new ArrayBuffer[TransactionMem[Data]]
    compBfs(comp, 
            (n: Node) => {
              if (n.isMem) cMems += n.asInstanceOf[Mem[ Data ] ]
              if (n.isInstanceOf[Reg] && !isPipeLineReg(n)) cRegs += n.asInstanceOf[Reg]
            }
          )
    for(c <- comp.children){
      if(c.isInstanceOf[TransactionMem[Data]]){
        cTransactionMems += c.asInstanceOf[TransactionMem[Data]]
      }
    }
    
    //Reg hazards
    for (b <- chckStg)
      println("HUY: " + b.name + " " + getStage(b))

    // raw stalls
    val specRegs = speculation.map(_._1.comp.asInstanceOf[Reg])
    cRegs = cRegs.filter(!specRegs.contains(_))
    for (p <- cRegs) {
      if (p.updates.length > 1 && stages.contains(p)) {
        val enables = p.updates.map(_._1)
        val enStgs = enables.map(getStage(_)).filter(_ > -1)
        val stage = enStgs.head
        if (p.name == "pc_reg") println(enables.map(getStage(_)) + " " + p.updates.map(_._2).map(getStage(_)) + " RD: " + getStage(p))
        scala.Predef.assert(enStgs.tail.map( _ == stage).foldLeft(true)(_ && _), println(p.line.getLineNumber + " " + p.line.getClassName + " " + enStgs)) // check all the stgs match
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
    for (m <- cTransactionMems) {
      for(i <- 0 until m.io.writes.length){
        val writePoint = m.io.writes(i)
        val writeAddr = writePoint.adr.inputs(0).inputs(1)
        val writeEn = writePoint.is.inputs(0).inputs(1)
        val writeData = writePoint.dat.inputs(0).inputs(1)
        val writeStage = getStage(writeEn)
        val writeEnables = getVersions(writeEn.asInstanceOf[Bool])
        val writeAddrs = getVersions(writeAddr.asInstanceOf[Bits])
        Predef.assert(getStage(writeEn) == getStage(writeData), "writeEN stage: " + getStage(writeEn) + " writeData stage: " + getStage(writeData))
        Predef.assert(getStage(writeData) == getStage(writeAddr), "writeData stage: " + getStage(writeData) + " writeAddr stage: " + getStage(writeAddr))
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

    /*for (n <- cMems) {
      for(i <- n.writes){
        val waddr = i.addr
        val enable = i.inputs(1)
        val dataIn = i.inputs(2)
        val wrStg = getStage(enable)
        var hazard = Bool(false)
        var foundHazard = false
        var rdStg = -1
        for(j <- n.reads){
          val raddr = j.addr
          rdStg = getStage(raddr)
          val enables = getVersions(enable.asInstanceOf[Bool])
          val waddrs = getVersions(waddr.asInstanceOf[Bits])
          if(wrStg > 0 && rdStg > 0 && wrStg > rdStg){
            scala.Predef.assert((getStage(enables.last) - rdStg) == 1, println(enables.length))
            scala.Predef.assert(enables.length == waddrs.length, println(enables.length + " " + waddrs.length))
            for ((en, w) <- enables zip waddrs) {
              hazards += (((j.cond && en.asInstanceOf[Bool] && (w.asInstanceOf[Bits] === raddr.asInstanceOf[Bits])), n, rdStg, getStage(en), en.asInstanceOf[Bool], null))
              println("found hazard " + en.line.getLineNumber + " " + en.line.getClassName + " " + en.name + " " + n.name)
            }
          }
        }
      }
    }*/
    
  }
  
  def getVersions(b: Bits): ArrayBuffer[Bits] = {
    val res = new ArrayBuffer[Bits]
    var cur = b
    while (cur.inputs.length == 1) {
      if (cur.inputs(0).isInstanceOf[Reg]) {
        if(!isPipeLineReg(cur)){
          res += cur
        }
        cur = cur.comp.updates(0)._2.asInstanceOf[Bits]
      } else if (cur.inputs(0).isInstanceOf[Bits]) {
        cur = cur.inputs(0).asInstanceOf[Bits]
      } else {
        /*val visited = new HashSet[Node]
        val dfsStack = new Stack[(Node, Int)]
        val maxDepth = 4
        dfsStack.push((cur, 0))
        while(!dfsStack.isEmpty){
          val currentNode = dfsStack.pop()
          visited += currentNode._1
          for(i <- 0 until currentNode._2){
            print("<>")
          }
          println(currentNode._1)
          for(i <- currentNode._1.inputs){
            if(!visited.contains(i) & currentNode._2 <= maxDepth){
              dfsStack.push((i, currentNode._2 + 1))
            }
          }
        }*/
        return res
      }
    }
    return res
  }

  def resolveHazards() = {

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
      cRegs += s.comp.asInstanceOf[Reg]

    val wStageMap = new HashMap[Reg, Int]()

    for ((r, ind) <- (cRegs zip cRegs.indices)) {
      if (r.updates.length > 1 && stages.contains(r)) {
        val enStg = r.updates.map(_._1).map(getStage(_)).filter(_ > -1)(0)
        wStageMap += (r -> enStg)
        var mask = Bool(false)
        if(stalls(enStg).length > 0)
          mask = mask || stalls(enStg).reduceLeft(_ || _) // raw
        if (enStg > 0)
          mask = mask || !valids(enStg-1) // no transaction
        if (tcomponents.length > 0) mask = mask || globalStall
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
  
  def generateForwardingLogic() = {
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
      for (i <- stages(r) + 1 to pipelineReg.size){
        forwardPoints(i) = new ArrayBuffer[(Node,Node)]()
      } 
      for ((writeEn, writeData) <- r.updates){Predef.assert(stages(writeEn) == stages(writeData))
        val writeEns = getVersions(writeEn)
        val writeDatas = getVersions(writeData.asInstanceOf[Bits])
        val numStagesAvail = Math.min(writeEns.length, writeDatas.length)
        for(i <- 0 until numStagesAvail) {
          forwardPoints(stages(writeEn) - i) += ((writeEns(i), writeDatas(i)))
        }
      }
      val muxMapping = new ArrayBuffer[(Bool, Bits)]()
      for(i <- stages(r) + 1 to pipelineReg.size){
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
      for (i <- stages(rp.adr) + 1 to pipelineReg.size){
        forwardPoints(i) = new ArrayBuffer[(Node,Node,Node)]()
      }
      for(i <- 0 until fm.io.writes.length){
        val writePoint = fm.io.writes(i)
        if(!memNonForwardedWritePoints.contains(writePoint)){
          val delayedWriteEn = getPipelinedVersion(writePoint.is.inputs(0).inputs(1))
          val delayedWriteData = getPipelinedVersion(writePoint.dat.asInstanceOf[Node].inputs(0).inputs(1))
          val delayedWriteAddr = getPipelinedVersion(writePoint.adr.inputs(0).inputs(1))
          Predef.assert(stages(delayedWriteEn) == stages(delayedWriteData))
          Predef.assert(stages(delayedWriteEn) == stages(delayedWriteAddr))
          val writeEns = getVersions(delayedWriteEn.asInstanceOf[Bits])
          val writeDatas = getVersions(delayedWriteData.asInstanceOf[Bits])
          val writeAddrs = getVersions(delayedWriteAddr.asInstanceOf[Bits])
          val numStagesAvail = Math.min(writeEns.length, Math.min(writeDatas.length, writeAddrs.length))
          for(i <- 0 until numStagesAvail){
            println("found fowarding point ("+ writeEns(i) + "," + writeDatas(i) + "," + writeAddrs(i) + ")")
            forwardPoints(stages(delayedWriteEn) - i) += ((writeEns(i), writeDatas(i), writeAddrs(i))) 
          }
        }
      }
      val muxMapping = new ArrayBuffer[(Bool, Bits)]()
      for(i <- stages(rp.adr) + 1 to pipelineReg.size){
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
    /*case m: Mem[_] => {
      println("generating forwarding logic for Mems")
      println("annotated write points")
      println("hazards")
      for(h <- hazards){
        println(h)
      }
      //println(memWritePoints)
      for(readPort <- m.reads){
        val forwardPoints = new HashMap[Int, ArrayBuffer[(Node, Node, Node)]]()
        for (i <- stages(readPort.addr) + 1 to pipelineReg.size){
          forwardPoints(i) = new ArrayBuffer[(Node,Node,Node)]()
        }
        for((writeEn, writeData, writeAddr) <- memWritePoints(m)){
          val delayedWriteEn = getPipelinedVersion(writeEn)
          val delayedWriteData = getPipelinedVersion(writeData)
          val delayedWriteAddr = getPipelinedVersion(writeAddr)
          Predef.assert(stages(delayedWriteEn) == stages(delayedWriteData))
          Predef.assert(stages(delayedWriteEn) == stages(delayedWriteAddr))
          val earliestForwardingStage = stages(delayedWriteEn) - Math.min(findEarliestStageAvail(delayedWriteEn), Math.min(findEarliestStageAvail(delayedWriteData), findEarliestStageAvail(delayedWriteAddr)))
          for(i <- stages(readPort.addr) + 1 to stages(delayedWriteEn)){
            forwardPoints(i) += ((findPastNodeVersion(delayedWriteEn, stages(delayedWriteEn) - i), findPastNodeVersion(delayedWriteData, stages(delayedWriteData) - i), findPastNodeVersion(delayedWriteAddr, stages(delayedWriteAddr) - i)))
          }
        }
        println("Mem forward points")
        println(forwardPoints)
        val muxMapping = new ArrayBuffer[(Bool, Data)]()
        for(i <- stages(readPort.addr) + 1 to pipelineReg.size){
          //generate muxes
          if(!forwardPoints(i).isEmpty){
            println("has forwards from stage " + i)
            for((writeEn, writeData, writeAddr) <- forwardPoints(i)){
              val forwardCond = readPort.cond & writeEn.asInstanceOf[Bool] & writeAddr.asInstanceOf[Bits] === readPort.addr.asInstanceOf[Bits]
              muxMapping += ((forwardCond, writeData.asInstanceOf[Data]))
              //append forward condition to hazards list
              for((cond, state, rStage, wStage, wEn, rPort) <- hazards){
                //println((cond, state, rStage, wStage, rEn, wEn))
                //if(wStage == i & rEn == readPort.cond & wEn == writeEn.asInstanceOf[Bool]){
                if(wStage == i & rPort == readPort){
                  hazards -= ((cond, state, rStage, wStage, wEn, rPort.asInstanceOf[FunRdIO[Data]]))
                  println("wtf " + i)
                  println("rPort: " + System.identityHashCode(rPort))
                  println("readPort: " + System.identityHashCode(readPort))
                  println("wEn: " + System.identityHashCode(wEn))
                  println("writeEn: " + System.identityHashCode(writeEn))
                }
              }
            }
          }
        }
        val bypassMux = MuxCase(readPort.dataOut.asInstanceOf[Data], muxMapping)
        for (n <- consumerMap(readPort.dataOut)){
          n.replaceProducer(readPort.dataOut, bypassMux)    
        }
      }
    }*/
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

