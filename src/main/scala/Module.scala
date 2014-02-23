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
import AutoPipe._

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
  
  //auto pipelining stuff
  val pipelineComponents = new ArrayBuffer[Module]
  var autoPipe = false
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
    
  // automatic pipeline stuff
  var pipelineReg = new HashMap[Int, ArrayBuffer[Reg]]()
  
  val forwardedRegs = new HashSet[Reg]
  val forwardedMemReadPorts = new HashSet[(TransactionMem[_], RdIO[_])]
  val memNonForwardedWritePoints = new HashSet[WrIO[_]]
  
  val speculatedRegs = new ArrayBuffer[(Reg, Data)]
  
  val userAnnotatedStages = new HashMap[Node, Int]()
  val autoAnnotatedStages = new HashMap[Node, Int]()

  var nodeToStageMap: HashMap[Node, Int] = null
  
  var PipelineComponentNodes: ArrayBuffer[Node] = null
  var ArchitecturalRegs: ArrayBuffer[Reg] = null
  var TransactionMems: ArrayBuffer[TransactionMem[ Data ]] = null
  var VariableLatencyComponents: ArrayBuffer[TransactionalComponent] = null
  
  var APConsumers: HashMap[Node, ArrayBuffer[(Node,Int)]] = null

  var globalStall: Bool = null
  
  var pipelineLength = 0
  val stageValids = new ArrayBuffer[Bool]
  val stageStalls = new ArrayBuffer[Bool]
  val stageKills = new ArrayBuffer[Bool]
  val requireStageSet = new HashSet[Node]

  var autoAnnotate = false
  var autoAnnotateStageNum = 0
  
  val regWritePoints = new HashSet[Node] //list of all register write inputs and transactionMem write port nodes(including write addr, write data, write en)
  val regReadPoints = new HashSet[Node] //list of all register read outputs and transactionMem read port
  val tMemWritePoints = new HashSet[Node] //list of all transactionMem write port nodes(including write addr, write data, write en)
  val tMemReadDatas = new HashSet[Node] 
  val tMemReadAddrs = new HashSet[Node]
  val tMemReadEns = new HashSet[Node]

  val inputNodes = new HashSet[Node] //list of all module Input Nodes
  val outputNodes = new HashSet[Node] //list of all module Output Nodes
  val ioNodes = new HashSet[DecoupledIO[Data]]// of all module ioNodes in the form of DecoupledIO
  val variableLatencyUnitInputs = new HashSet[Node]// list of all variable latency unit input nodes
  val variableLatencyUnitOutputs = new HashSet[Node]// list of all variable latency unit output nodes

  val regRAWHazards = new HashMap[(Reg, Int, Int), Bool] //map of (register, updatelist num, write stage) -> RAW signal
  val tMemRAWHazards = new HashMap[(RdIO[Data], Int, Int), Bool] //map of (tmem readport, writeport number, write stage) -> RAW signal 
  
  var autoSourceNodes: ArrayBuffer[AutoNode] = null
  var autoSinkNodes: ArrayBuffer[AutoNode] = null
  
  def isSource(node: Node) =  {
    regReadPoints.contains(node) || tMemReadDatas.contains(node) || inputNodes.contains(node) || variableLatencyUnitOutputs.contains(node)
  }

  def isSink(node: Node) = {
    regWritePoints.contains(node) || tMemReadAddrs.contains(node) || tMemReadEns.contains(node) || tMemWritePoints.contains(node) || outputNodes.contains(node) || variableLatencyUnitInputs.contains(node)
  }

  def sourceNodes(): HashSet[Node] = {
    regReadPoints | tMemReadDatas | inputNodes |  variableLatencyUnitOutputs
  }
  def sinkNodes(): HashSet[Node] = {
    regWritePoints | tMemReadAddrs | tMemReadEns | tMemWritePoints | outputNodes | variableLatencyUnitInputs
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
  
  def setStageNum(num: Int) = {//insert pipeline registers by specifying number of stages
    autoAnnotate = true
    autoAnnotateStageNum = num
  }
  
  def setStage(n: Node, s:Int) = {//insert pipeline registers by annotating nodes with stages
    Predef.assert(!userAnnotatedStages.contains(n), n.name + " already annotated as stage " + userAnnotatedStages(n))
    if(n.isInstanceOf[Data] && n.asInstanceOf[Data].comp != null){
      userAnnotatedStages(n.asInstanceOf[Data].comp) = s
    } else {
      userAnnotatedStages(n) = s
    }
  }
  def setRegReadStage(node: Node, stage: Int) = {
    val reg = node
    if(!userAnnotatedStages.contains(reg)){
      userAnnotatedStages(reg) = stage
    }
  }
  def setRegWriteStage(node: Node, stage: Int) = {
    val reg = node
    for(producer <- reg.getProducers()){
      if(!userAnnotatedStages.contains(producer)){
        userAnnotatedStages(producer) = stage
      }
    }
  }
  def setTmemReadStage(tmem: TransactionMem[_], stage: Int) = {
    for(i <- 0 until tmem.io.reads.length){
      val readPoint = tmem.io.reads(i)
      val readAddr = readPoint.adr.asInstanceOf[Node]
      val readEn = readPoint.is.asInstanceOf[Node]
      if(!userAnnotatedStages.contains(readAddr)){
        userAnnotatedStages(readAddr) = stage
      }
      if(!userAnnotatedStages.contains(readEn)){
        userAnnotatedStages(readEn) = stage
      }
    }
  }
  def setTmemWriteStage(tmem: TransactionMem[_], stage: Int) = {
    for(i <- 0 until tmem.io.writes.length){
      val writePoint = tmem.io.writes(i)
      val writeAddr = writePoint.adr
      val writeEn = writePoint.is
      val writeData = writePoint.dat.asInstanceOf[Node]
      if(!userAnnotatedStages.contains(writeAddr)){
        userAnnotatedStages(writeAddr) = stage
      }
      if(!userAnnotatedStages.contains(writeData)){
        userAnnotatedStages(writeData) = stage
      }
      if(!userAnnotatedStages.contains(writeEn)){
        userAnnotatedStages(writeEn) = stage
      }
    }
  }
  def setDecoupledIOStage(decoupledIO: DecoupledIO[Data], stage: Int) = {
    if(!userAnnotatedStages.contains(decoupledIO.ready)){
      userAnnotatedStages(decoupledIO.ready) = stage
    }
    if(!userAnnotatedStages.contains(decoupledIO.valid)){
      userAnnotatedStages(decoupledIO.valid) = stage
    }
    for(data <- decoupledIO.bits.flatten.map(_._2)){
      if(!userAnnotatedStages.contains(data)){
        userAnnotatedStages(data) = stage
      }
    }
  }
  def setDecoupledIOStageAuto(decoupledIO: DecoupledIO[Data], stage: Int) = {
    if(!userAnnotatedStages.contains(decoupledIO.ready)){
      autoAnnotatedStages(decoupledIO.ready) = stage
    }
    if(!userAnnotatedStages.contains(decoupledIO.valid)){
      autoAnnotatedStages(decoupledIO.valid) = stage
    }
    for(data <- decoupledIO.bits.flatten.map(_._2)){
      if(!userAnnotatedStages.contains(data)){
        autoAnnotatedStages(data) = stage
      }
    }
  }
  def setVariableLatencyUnitStage(varComp: TransactionalComponent, stage: Int) = {
    userAnnotatedStages(varComp.io.req.valid) = stage
    userAnnotatedStages(varComp.io.req.bits) = stage
    userAnnotatedStages(varComp.resp_ready) = stage
    userAnnotatedStages(varComp.io.resp) = stage
    userAnnotatedStages(varComp.resp_valid) = stage
    userAnnotatedStages(varComp.req_ready) = stage
  }

  def addForwardedReg(d: Reg) = {
    forwardedRegs += d
  }
  def addForwardedMemReadPort(m: TransactionMem[_], r: RdIO[_]) = {
    forwardedMemReadPorts += ((m.asInstanceOf[TransactionMem[Data]], r.asInstanceOf[RdIO[Data]]))
  }
  def addNonForwardedMemWritePoint[T <: Data] (writePoint: WrIO[T]) = {
    memNonForwardedWritePoints += writePoint
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
  
  //automatic pipelining stuff
  def gatherSpecialComponents() = {
    println("gathering special pipelining components")
    PipelineComponentNodes = new ArrayBuffer[Node]
    ArchitecturalRegs = new ArrayBuffer[Reg]
    TransactionMems = new ArrayBuffer[TransactionMem[Data]]
    VariableLatencyComponents = new ArrayBuffer[TransactionalComponent]()
    
    //collect all nodes within in the pipeline component, including all nodes in the submodules except for nodes within TransactionMems and VariableLatencyUnits
    def registerNodes(module: Module): Unit = {
      if(!module.isInstanceOf[TransactionMem[_]] && !module.isInstanceOf[TransactionalComponent]){
        for(node <- module.nodes){
          PipelineComponentNodes += node
        }
        for(childModule <- module.children){
          registerNodes(childModule)
        }
      } else {
        for(io <- module.io.flatten.map(_._2)){
          PipelineComponentNodes += io
        }
      }
    }
    registerNodes(this)

    //mark architectural registers
    for(node <- PipelineComponentNodes){
      if (node.isInstanceOf[Reg] && !isPipeLineReg(node)) {
        ArchitecturalRegs += node.asInstanceOf[Reg]
      }
    }
    //mark TransactionMems
    def registerTransactionMems(module: Module): Unit = {
      if(!module.isInstanceOf[TransactionMem[_]] && !module.isInstanceOf[TransactionalComponent]){
        for(childModule <- module.children){
          registerTransactionMems(childModule)
        }
      } else if(module.isInstanceOf[TransactionMem[_]]){
        TransactionMems += module.asInstanceOf[TransactionMem[Data]]
      }
    }
    registerTransactionMems(this)
    //mark variable latency modules
    def registerVariableLatencyComponents(module: Module): Unit = {
      if(!module.isInstanceOf[TransactionMem[_]] && !module.isInstanceOf[TransactionalComponent]){
        for(childModule <- module.children){
          registerVariableLatencyComponents(childModule)
        }
      } else if(module.isInstanceOf[TransactionalComponent]){
        VariableLatencyComponents += module.asInstanceOf[TransactionalComponent]
        variableLatencyUnitInputs += module.asInstanceOf[TransactionalComponent].io.req.valid
        variableLatencyUnitInputs += module.asInstanceOf[TransactionalComponent].io.req.bits
        variableLatencyUnitOutputs += module.asInstanceOf[TransactionalComponent].io.resp
      }
    }
    registerVariableLatencyComponents(this)
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
        tMemReadEns += tmem.io.reads(i).is
      }
      for(i <- 0 until tmem.virtWritePortNum){
        tMemWritePoints += tmem.io.writes(i).adr
        tMemWritePoints += tmem.io.writes(i).dat
        tMemWritePoints += tmem.io.writes(i).is
      }
    }
    //find module IO nodes
    val inputs = this.io.flatten.filter(_._2.dir == INPUT)
    val outputs = this.io.flatten.filter(_._2.dir == OUTPUT)
    for(input <- inputs){
      if(input._2.name != "io_tid_x" && input._2.name != "io_tid_y"){//hack to ignore tid input for Dreamer
        inputNodes += input._2
      }
    }
    for(output <- outputs){
      outputNodes += output._2
    }
    
    def findDecoupledIO(collection: Data): Unit = {
      if(collection.isInstanceOf[Bundle]){
        for((name, elm) <- collection.asInstanceOf[Bundle].elements){
          elm match {
            case decoupled: DecoupledIO[_] => {
              ioNodes += decoupled.asInstanceOf[DecoupledIO[Data]]
            }
            case any => {
              findDecoupledIO(any.asInstanceOf[Data])  
            }
          }
        }
      } else if(collection.isInstanceOf[Vec[_]]){
        for(elm <- collection.asInstanceOf[Vec[Data]].self){
          elm match{
            case decoupled: DecoupledIO[_] => {
              ioNodes += decoupled.asInstanceOf[DecoupledIO[Data]]
            }
            case any => {
              findDecoupledIO(any.asInstanceOf[Data])
            }
          }
        }
      }
    }
    
    findDecoupledIO(this.io)
    
    //find our own consumers list with input numbers attached to the consumer pointers and trace into submodules
    findAPConsumers()
    
    //specify read/write point and IO stage numbers if auto annotate is on
    if(autoAnnotate){
      for(archReg <- ArchitecturalRegs){
        setRegReadStage(archReg, 0)
        setRegWriteStage(archReg, autoAnnotateStageNum - 1)
      }
      for(tMem <-TransactionMems){
        setTmemWriteStage(tMem, autoAnnotateStageNum - 1)
      }
      for(io <- ioNodes){
        //setDecoupledIOStage(io, autoAnnotateStageNum/2)
        setDecoupledIOStageAuto(io, autoAnnotateStageNum/2)
      }
    }

  }
 
  def findAPConsumers() = {
    APConsumers = new HashMap[Node, ArrayBuffer[(Node, Int)]]()
    val allNodes = new ArrayBuffer[Node]
    def findAllNodes(module: Module):Unit = {
      for(node <- module.nodes){
        allNodes += node
      }
      for(child <- module.children){
        findAllNodes(child)
      }
    }
    findAllNodes(this)
    for(node <- allNodes){
      APConsumers(node) = new ArrayBuffer[(Node, Int)]()
    }
    for(node <- allNodes){
      for(i <- 0 until node.inputs.length){
        val nodeInput = node.inputs(i)
        if(APConsumers.contains(nodeInput)){
          APConsumers(nodeInput) += ((node, i))
        }
      }
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
        for(child <- APConsumers(currentNode).map(_._1)){
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
  
  /*def insertPipelineRegisters() = { 
    setPipelineLength(userAnnotatedStages.map(_._2).max + 1)
    
    removeRegConsumerCycle()
   
    findNodesRequireStage()
    
    val autoNodes = createAutoNodeGraph()
    visualizeAutoLogicGraph(autoNodes, "debug.gv") 
    println("finding valid pipeline register placement")
    propagateStages(autoNodes)
    verifyLegalStageColoring(autoNodes)
    
    println("optimizing pipeline register placement")
    optimizeRegisterPlacement(autoNodes)
    verifyLegalStageColoring(autoNodes)
    
    println("inserting pipeline registers")

    var counter = 0//hack to get unique register names with out anything nodes being named already
    nodeToStageMap = new HashMap[Node, Int]()
    for(wire <- autoNodes.filter(_.isInstanceOf[AutoWire]).map(_.asInstanceOf[AutoWire])){
      if(wire.stages.length == 2){
        val stageDifference = Math.abs(wire.stages(1) - wire.stages(0))
        Predef.assert(stageDifference > 0, stageDifference)
        var currentChiselNode = insertBitOnInput(wire.consumerChiselNode, wire.consumerChiselNodeInputNum)
        nodeToStageMap(currentChiselNode) = Math.min(wire.stages(0), wire.stages(1))
        for(i <- 0 until stageDifference){
          println("inserting pipeline register")
          currentChiselNode = insertRegister(currentChiselNode, Bits(0), "Stage_" + (Math.min(wire.stages(0),wire.stages(1)) + i) + "_" + "PipeReg_"+ currentChiselNode.name + counter)
          nodeToStageMap(currentChiselNode) = Math.min(wire.stages(0),wire.stages(1)) + i + 1
          nodeToStageMap(currentChiselNode.asInstanceOf[Bits].comp) = Math.min(wire.stages(0),wire.stages(1)) + i + 1
          counter = counter + 1
          pipelineReg(Math.min(wire.stages(0),wire.stages(1)) + i) += currentChiselNode.asInstanceOf[Bits].comp.asInstanceOf[Reg]
        }
      }
    }

    for(node <- autoNodes.filter(_.isInstanceOf[AutoLogic])){
      for(inputChiselNode <- node.asInstanceOf[AutoLogic].inputChiselNodes){
        nodeToStageMap(inputChiselNode) = node.stages(0)
      }
      for(outputChiselNode <- node.asInstanceOf[AutoLogic].outputChiselNodes){
        nodeToStageMap(outputChiselNode) = node.stages(0)
      }
    }
  }*/

  def insertPipelineRegisters() = { 
    setPipelineLength(userAnnotatedStages.map(_._2).max + 1)
    
    removeRegConsumerCycle()
   
    findNodesRequireStage()
    
    val autoNodes = createAutoNodeGraph()
    visualizeAutoLogicGraph(autoNodes, "debug.gv") 
    println("finding valid pipeline register placement")
    propagateStages(autoNodes)
    verifyLegalStageColoring(autoNodes)
    
    println("optimizing pipeline register placement")
    optimizeRegisterPlacement(autoNodes)
    verifyLegalStageColoring(autoNodes)
    
    println("inserting pipeline registers")

    var counter = 0//hack to get unique register names with out anything nodes being named already
    nodeToStageMap = new HashMap[Node, Int]()
    for(wire <- autoNodes.filter(_.isInstanceOf[AutoWire]).map(_.asInstanceOf[AutoWire])){
      if(wire.inputStage != wire.outputStage){
        val stageDifference = Math.abs(wire.inputStage - wire.outputStage)
        Predef.assert(stageDifference > 0, stageDifference)
        var currentChiselNode = insertBitOnInput(wire.consumerChiselNode, wire.consumerChiselNodeInputNum)
        nodeToStageMap(currentChiselNode) = Math.min(wire.inputStage, wire.outputStage)
        for(i <- 0 until stageDifference){
          println("inserting pipeline register")
          currentChiselNode = insertRegister(currentChiselNode, Bits(0), "Stage_" + (Math.min(wire.inputStage,wire.outputStage) + i) + "_" + "PipeReg_"+ currentChiselNode.name + counter)
          nodeToStageMap(currentChiselNode) = Math.min(wire.inputStage,wire.outputStage) + i + 1
          nodeToStageMap(currentChiselNode.asInstanceOf[Bits].comp) = Math.min(wire.inputStage, wire.outputStage) + i + 1
          counter = counter + 1
          pipelineReg(Math.min(wire.inputStage, wire.outputStage) + i) += currentChiselNode.asInstanceOf[Bits].comp.asInstanceOf[Reg]
        }
      }
    }

    for(node <- autoNodes.filter(_.isInstanceOf[AutoLogic])){
      for(inputChiselNode <- node.asInstanceOf[AutoLogic].inputChiselNodes){
        nodeToStageMap(inputChiselNode) = node.inputStage
      }
      for(outputChiselNode <- node.asInstanceOf[AutoLogic].outputChiselNodes){
        nodeToStageMap(outputChiselNode) = node.outputStage
      }
    }
  }
  
  //hack to deal with register default updates causing a cycle in C++ backend
  def removeRegConsumerCycle() = {  
    def dfs(node: Node, reg: Node): Unit  = {
      if(node.inputs.contains(reg)){
        val consumerPairsToRemove = new ArrayBuffer[(Node, Int)]
        for(i <- 0 until APConsumers(reg).length){
          if(APConsumers(reg)(i)._1 == node){
            consumerPairsToRemove += APConsumers(reg)(i)
          }
        }
        for(consumerPair <- consumerPairsToRemove){
          APConsumers(reg) -= consumerPair
        }
        reg.consumers -= node//remove after all .consumer calls are removed
        //regWritePoints += node
      } else if(!isSink(node) && !isSource(node) && !((node.litOf != null) || (node == this.clock) || (node == this._reset))){
        for(input <- node.inputs){
          if(!isSink(input) && !isSource(input) && !((input.litOf != null) || (input == this.clock) || (input == this._reset))){
            dfs(input, reg)
          }
        }
      }
    }
    for(reg <- ArchitecturalRegs){
      for(input <- reg.inputs){
        dfs(input, reg)
      }
    }
  }
  
  def createAutoNodeGraph(): ArrayBuffer[AutoNode] = {
    val chiselNodeToAutoNodeMap = new HashMap[Node, AutoNode]
    val autoNodeGraph = new ArrayBuffer[AutoNode]
    autoSourceNodes = new ArrayBuffer[AutoNode]
    autoSinkNodes = new ArrayBuffer[AutoNode]

    //create AutoLogic nodes for all chisel nodes that require a stage
    for(reg <- ArchitecturalRegs){
      val readPoint = new AutoLogic
      readPoint.name = reg.name
      readPoint.isSource = true
      readPoint.findStage(reg, userAnnotatedStages)
      readPoint.outputChiselNodes += reg
      chiselNodeToAutoNodeMap(reg) = readPoint
      autoNodeGraph += readPoint
      autoSourceNodes += readPoint

      val writePoint = new AutoLogic
      writePoint.name = reg.name + "_writepoint"
      writePoint.isSink = true
      for(producer <- reg.getProducers){
        writePoint.findStage(producer, userAnnotatedStages)
        writePoint.inputChiselNodes += producer
        chiselNodeToAutoNodeMap(producer) = writePoint
      }
      autoNodeGraph += writePoint
      autoSinkNodes += writePoint
    }

    for(tMem <- TransactionMems){
      val readPoint = new AutoLogic
      readPoint.name = tMem.name + "_readports"
      readPoint.delay = 5.0
      if(tMem.isSeqRead){
        readPoint.isSeqReadPort = true
      }
      for(i <- 0 until tMem.readPortNum){
        val readAddr = tMem.io.reads(i).adr
        val readData = tMem.io.reads(i).dat
        val readEn = tMem.io.reads(i).is
        readPoint.findStage(readAddr, userAnnotatedStages)
        readPoint.findStage(readEn, userAnnotatedStages)
        readPoint.inputChiselNodes += readAddr
        readPoint.inputChiselNodes += readEn
        readPoint.outputChiselNodes += readData
        chiselNodeToAutoNodeMap(readAddr) = readPoint
        chiselNodeToAutoNodeMap(readEn) = readPoint
        chiselNodeToAutoNodeMap(readData) = readPoint
      }
      autoNodeGraph += readPoint
      
      val writePoint = new AutoLogic
      writePoint.name = tMem.name + "_writeports"
      writePoint.isSink = true
      for(i <- 0 until tMem.virtWritePortNum){
        val writeAddr = tMem.io.writes(i).adr
        val writeData = tMem.io.writes(i).dat
        val writeEn = tMem.io.writes(i).is
        writePoint.findStage(writeAddr, userAnnotatedStages)
        writePoint.findStage(writeData, userAnnotatedStages)
        writePoint.findStage(writeEn, userAnnotatedStages)
        writePoint.inputChiselNodes += writeAddr
        writePoint.inputChiselNodes += writeData
        writePoint.inputChiselNodes += writeEn
        chiselNodeToAutoNodeMap(writeAddr) = writePoint
        chiselNodeToAutoNodeMap(writeData) = writePoint
        chiselNodeToAutoNodeMap(writeEn) = writePoint
      }
      autoNodeGraph += writePoint
      autoSinkNodes += writePoint
    }

    for(varLatUnit <- VariableLatencyComponents){
      val inputs = varLatUnit.io.flatten.map(_._2).filter(_.dir == INPUT)
      val outputs = varLatUnit.io.flatten.map(_._2).filter(_.dir == OUTPUT)
      val varLatUnitNode = new AutoLogic
      varLatUnitNode.name = varLatUnit.name
      varLatUnitNode.delay = 5.0
      for(input <- inputs){
        varLatUnitNode.findStage(input, userAnnotatedStages)
        varLatUnitNode.inputChiselNodes += input
        chiselNodeToAutoNodeMap(input) = varLatUnitNode
      }
      for(output <- outputs){
        varLatUnitNode.findStage(output, userAnnotatedStages)
        varLatUnitNode.outputChiselNodes += output
        chiselNodeToAutoNodeMap(output) = varLatUnitNode
      }
      autoNodeGraph += varLatUnitNode 
    }
    
    for(io <- ioNodes){
      val ioNode = new AutoLogic
      ioNode.name = "io_node"
      ioNode.isDecoupledIO = true
      val dataBits = io.bits.flatten.map(_._2)
      
      ioNode.findStage(io.ready, userAnnotatedStages)
      ioNode.findStageAuto(io.ready, autoAnnotatedStages)
      if(io.ready.dir == INPUT){
        ioNode.outputChiselNodes += io.ready
      } else {
        ioNode.inputChiselNodes += io.ready
      }
      chiselNodeToAutoNodeMap(io.ready) = ioNode

      ioNode.findStage(io.valid, userAnnotatedStages)
      ioNode.findStageAuto(io.valid, autoAnnotatedStages)

      if(io.valid.dir == INPUT){
        ioNode.outputChiselNodes += io.valid
      } else {
        ioNode.inputChiselNodes += io.valid
      }
      chiselNodeToAutoNodeMap(io.valid) = ioNode

      for(data <- dataBits){
        ioNode.findStage(data, userAnnotatedStages)
        ioNode.findStageAuto(data, autoAnnotatedStages)
        if(data.dir == INPUT){
          ioNode.outputChiselNodes += data
        } else {
          ioNode.inputChiselNodes += data
        }
        chiselNodeToAutoNodeMap(data) = ioNode
      }
      autoNodeGraph += ioNode
    }
    

    for(node <- requireStageSet){
      if(!isSource(node) && !isSink(node)){
        val autoNode = new AutoLogic
        autoNode.name = node.name
        autoNode.delay = node.delay
        autoNode.findStage(node, userAnnotatedStages)
        autoNode.inputChiselNodes += node
        autoNode.outputChiselNodes += node
        chiselNodeToAutoNodeMap(node) = autoNode
        autoNodeGraph += autoNode
      }
    }

    //connect autologic nodes
    for(autoNode <- autoNodeGraph){
      for(input <- autoNode.asInstanceOf[AutoLogic].inputChiselNodes){
        for(i <- 0 until input.inputs.length){
          if(requireStage(input.inputs(i))){
            val autoInput = chiselNodeToAutoNodeMap(input.inputs(i))
            autoNode.inputs += autoInput
            autoNode.asInstanceOf[AutoLogic].inputMap(autoInput) = ((input, i))
          }
        }
      }
      if(autoNode.inputs.isEmpty){
        autoNode.isSource = true
        autoSourceNodes += autoNode
      }
      for(output <- autoNode.asInstanceOf[AutoLogic].outputChiselNodes){
        for(consumer <- APConsumers(output).map(_._1).filter(requireStage(_))){
          autoNode.consumers += chiselNodeToAutoNodeMap(consumer)
        }
      }
      if(autoNode.consumers.isEmpty){
        autoNode.isSink = true
        autoSinkNodes += autoNode
      }
    }

    //insert AutoWires between the AutoLogicNodes
    val bfsQueue = new ScalaQueue[AutoLogic]
    val visited = new HashSet[AutoLogic]
    for(autoNode <- autoSinkNodes){
      bfsQueue.enqueue(autoNode.asInstanceOf[AutoLogic])
    }
    while(!bfsQueue.isEmpty){
      val currentNode = bfsQueue.dequeue
      if(!visited.contains(currentNode)){
        visited += currentNode
        for(input <- currentNode.inputs){
          if(!visited.contains(input.asInstanceOf[AutoLogic]) && !autoSourceNodes.contains(input)){
            bfsQueue.enqueue(input.asInstanceOf[AutoLogic])
          }
        }
        for(i <- 0 until currentNode.inputs.length){
          val input = currentNode.inputs(i)
          //insert AutoWire
          val autoWire = new AutoWire
          autoWire.inputs += input
          autoWire.consumers += currentNode
          autoWire.consumerChiselNode = currentNode.inputMap(input)._1
          autoWire.consumerChiselNodeInputNum = currentNode.inputMap(input)._2
          autoNodeGraph += autoWire
          //fix input pointers on currentNode
          currentNode.inputs(i) = autoWire

          //fix consumers pointers on input
          var foundConsumer = false
          for(j <- 0 until input.consumers.length){
            if(input.consumers(j) == currentNode && !foundConsumer){
              foundConsumer = true
              input.consumers(j) = autoWire
            }
          }
        }
      }
    }

    return autoNodeGraph
  }
  
  /*def propagateStages(autoNodes: ArrayBuffer[AutoNode]) = {
    val bfsQueue = new ScalaQueue[(AutoNode, Direction)] //the 2nd item of the tuple indicateds which direction the node should propagate its stage number to. True means propagate to outputs, false means propagate to inputs
    val retryNodes = new ArrayBuffer[(AutoNode, Direction)] //list of nodes that need to retry their stage propagation because their children weren't ready
  
    def propagatedToChild(parentStage: Int, child: AutoNode) : Boolean = {
      return child.stages.contains(parentStage)
    }
    
    def propagatedToAllChildren(node: AutoNode, stage: Int, direction: Direction) : Boolean = {
      var children = node.inputs
      if(direction == FORWARD){
        children = node.consumers
      }
      var result = true
      for(child <- children){
        result = result && propagatedToChild(stage, child)
      }
      return result
    }
    
    def propagateToChildren(node: AutoNode, direction: Direction) = {
      val propagatedChildren = new ArrayBuffer[AutoNode] 
      
      //determine if we need/can propagate to child and return the stage number that should be propagated to child; direction = false means child is producer of node, direction = true means child is consumer of node
      def childEligibleForPropagation(child: AutoNode, direction: Direction): Boolean = {
        val childWasPropagated = propagatedToChild(node.stages(0), child)
        val wireToLogic = node.isInstanceOf[AutoWire] && child.isInstanceOf[AutoLogic] && child.stages.length > 0
        var allParentsResolved = true
        var childParents = new ArrayBuffer[AutoNode]
        if(direction == FORWARD){
          for(childProducer <- child.inputs){
            childParents += childProducer
          }
        } else {
          for(childConsumer <- child.consumers){
            childParents += childConsumer
          }
        }
        var edgeParentStage:Int = 0//this is the minimum stage of child's consumers when direction == true, this is the maximum stage of child's producers when direction == false
        if(direction == FORWARD){
          edgeParentStage = 0
        } else {
          edgeParentStage = Int.MaxValue
        }

        for(parent <- childParents){
          if(parent.stages.length == 0){
            allParentsResolved = false
          } else {
            if(direction == FORWARD){
              if(parent.stages.max > edgeParentStage){
                edgeParentStage = parent.stages.max
              }
            } else {
              if(parent.stages.min < edgeParentStage){
                edgeParentStage = parent.stages.min
              }
            }
          }
        }
        val childEligible = !childWasPropagated && !wireToLogic && allParentsResolved
        return childEligible
      }
      //propagate node's stage number to child and record all new nodes that have been propagated to; direction = false means child is producer of node, direction = true means child is consumer of node
      def doPropagation(child: AutoNode, direction: Direction) = {
        var childParents = new ArrayBuffer[AutoNode]
        if(direction == FORWARD){
          for(childProducer <- child.inputs){
            childParents += childProducer
          }
        } else {
          for(childConsumer <- child.consumers){
            childParents += childConsumer
          }
        }
        
        var edgeParentStage:Int = 0//this is the minimum stage of child's consumers when direction == true, this is the maximum stage of child's producers when direction == false
        if(direction == FORWARD){
          edgeParentStage = 0
        } else {
          edgeParentStage = Int.MaxValue
        }

        for(parent <- childParents){
          if(direction == FORWARD){
            if(parent.stages.max > edgeParentStage){
              edgeParentStage = parent.stages.max
            }
          } else {
            if(parent.stages.min < edgeParentStage){
              edgeParentStage = parent.stages.min
            }
          }
        }
        //propagate stage to child
        if(!child.stages.contains(edgeParentStage)){
          child.stages += edgeParentStage
        }
        //propagate child stage back to its parents
        for(parent <- childParents){
          if(!parent.stages.contains(edgeParentStage)){
            parent.stages += edgeParentStage
          }
        }
        
        //set propagatedChildren
        propagatedChildren += child
      }

      var children:Seq[AutoNode] = null
      if(direction == FORWARD){//propagate to consumers
        children = node.consumers
      } else {//propagate to producers
        children = node.inputs
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
    

    //initialize bfs queue and coloredNodes with user annotated nodes
    for(autoNode <- autoNodes){
      if(autoNode.isUserAnnotated || autoNode.isAutoAnnotated){
        if(!autoNode.isSink){
          retryNodes += ((autoNode, FORWARD))
        }
        if(!autoNode.isSource){
          retryNodes += ((autoNode, BACKWARD))
        }
      }
    }
    
    //do stage propagation
    while(!retryNodes.isEmpty){
      for((node, direction) <- retryNodes){
        bfsQueue.enqueue(((node, direction)))
      }
      retryNodes.clear
      while(!bfsQueue.isEmpty){
        val current = bfsQueue.dequeue
        val currentNode = current._1
        val currentDirection = current._2
        if(currentNode.stages.length < 2){
          var childrenPropagatedTo:ArrayBuffer[AutoNode] = null
          childrenPropagatedTo = propagateToChildren(currentNode, currentDirection)
          for(child <- childrenPropagatedTo){
            if((child.stages.length < 2) && !child.isSource && !child.isSink && !child.isUserAnnotated && !child.isAutoAnnotated){
              bfsQueue.enqueue(((child, currentDirection)))
            }
          }
          if(!propagatedToAllChildren(currentNode, currentNode.stages(0), currentDirection)){
            retryNodes += ((currentNode, currentDirection))
          }
        }
      }
    }
    visualizeAutoLogicGraph(autoNodes, "stages.gv")
  }*/
  
  def propagateStages(autoNodes: ArrayBuffer[AutoNode]) = {
    val bfsQueue = new ScalaQueue[(AutoNode, Direction)] //the 2nd item of the tuple indicateds which direction the node should propagate its stage number to. True means propagate to outputs, false means propagate to inputs
    val retryNodes = new ArrayBuffer[(AutoNode, Direction)] //list of nodes that need to retry their stage propagation because their children weren't ready
  
    //direction == FORWARD means parent is input of child, direction == BACKWARD means parent is consumer of child
    def propagatedToChild(parent:AutoNode, child: AutoNode, direction: Direction) : Boolean = {
      var result = false
      if(direction == FORWARD){
        result = parent.outputStage == child.inputStage
      } else {
        result = parent.inputStage == child.outputStage
      }
      return result
    }
    
    def propagatedToAllChildren(node: AutoNode, direction: Direction) : Boolean = {
      var children = node.inputs
      if(direction == FORWARD){
        children = node.consumers
      }
      var result = true
      for(child <- children){
        result = result && propagatedToChild(node, child, direction)
      }
      return result
    }
    
    def propagateToChildren(node: AutoNode, direction: Direction) = {
      val propagatedChildren = new ArrayBuffer[AutoNode] 
      
      //determine if we need/can propagate to child and return the stage number that should be propagated to child; direction = false means child is producer of node, direction = true means child is consumer of node
      def childEligibleForPropagation(child: AutoNode, direction: Direction): Boolean = {
        val childWasPropagated = propagatedToChild(node, child, direction)
        val wireToLogic = node.isInstanceOf[AutoWire] && child.isInstanceOf[AutoLogic] && child.propagatedTo
        var allParentsResolved = true
        var childParents = new ArrayBuffer[AutoNode]
        if(direction == FORWARD){
          for(childProducer <- child.inputs){
            childParents += childProducer
          }
        } else {
          for(childConsumer <- child.consumers){
            childParents += childConsumer
          }
        }
        var edgeParentStage:Int = 0//this is the minimum stage of child's consumers when direction == true, this is the maximum stage of child's producers when direction == false
        if(direction == FORWARD){
          edgeParentStage = 0
        } else {
          edgeParentStage = Int.MaxValue
        }

        for(parent <- childParents){
          if(!parent.propagatedTo){
            allParentsResolved = false
          } else {
            if(direction == FORWARD){
              if(parent.outputStage > edgeParentStage){
                edgeParentStage = parent.outputStage
              }
            } else {
              if(parent.inputStage < edgeParentStage){
                edgeParentStage = parent.inputStage
              }
            }
          }
        }
        val childEligible = !childWasPropagated && !wireToLogic && allParentsResolved
        return childEligible
      }
      //propagate node's stage number to child and record all new nodes that have been propagated to; direction = false means child is producer of node, direction = true means child is consumer of node
      def doPropagation(child: AutoNode, direction: Direction) = {
        var childParents = new ArrayBuffer[AutoNode]
        if(direction == FORWARD){
          for(childProducer <- child.inputs){
            childParents += childProducer
          }
        } else {
          for(childConsumer <- child.consumers){
            childParents += childConsumer
          }
        }
        
        var edgeParentStage:Int = 0//this is the minimum stage of child's consumers when direction == true, this is the maximum stage of child's producers when direction == false
        if(direction == FORWARD){
          edgeParentStage = 0
        } else {
          edgeParentStage = Int.MaxValue
        }

        for(parent <- childParents){
          if(direction == FORWARD){
            if(parent.outputStage > edgeParentStage){
              edgeParentStage = parent.outputStage
            }
          } else {
            if(parent.inputStage < edgeParentStage){
              edgeParentStage = parent.inputStage
            }
          }
        }
        //propagate stage to child
        child.propagateStage(edgeParentStage, direction)
        
        //propagate child stage back to its parents
        for(parent <- childParents){
          if(direction == FORWARD){
            parent.outputStage = edgeParentStage
          } else {
            parent.inputStage = edgeParentStage
          }
        }
        
        //set propagatedChildren
        propagatedChildren += child
      }

      var children:Seq[AutoNode] = null
      if(direction == FORWARD){//propagate to consumers
        children = node.consumers
      } else {//propagate to producers
        children = node.inputs
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
    

    //initialize bfs queue and coloredNodes with user annotated nodes
    for(autoNode <- autoNodes){
      if(autoNode.isUserAnnotated || autoNode.isAutoAnnotated){
        if(!autoNode.isSink){
          retryNodes += ((autoNode, FORWARD))
        }
        if(!autoNode.isSource){
          retryNodes += ((autoNode, BACKWARD))
        }
      }
    }
    
    //do stage propagation
    while(!retryNodes.isEmpty){
      for((node, direction) <- retryNodes){
        bfsQueue.enqueue(((node, direction)))
      }
      retryNodes.clear
      while(!bfsQueue.isEmpty){
        val current = bfsQueue.dequeue
        val currentNode = current._1
        val currentDirection = current._2
        if(currentNode.inputStage == currentNode.outputStage || currentNode.isSeqReadPort){
          var childrenPropagatedTo:ArrayBuffer[AutoNode] = null
          childrenPropagatedTo = propagateToChildren(currentNode, currentDirection)
          for(child <- childrenPropagatedTo){
            if((child.inputStage == child.outputStage || child.isSeqReadPort) && !child.isSource && !child.isSink && !child.isUserAnnotated && !child.isAutoAnnotated){
              bfsQueue.enqueue(((child, currentDirection)))
            }
          }
          if(!propagatedToAllChildren(currentNode, currentDirection)){
            retryNodes += ((currentNode, currentDirection))
          }
        }
      }
    }
    visualizeAutoLogicGraph(autoNodes, "stages.gv")
  }

  /*def optimizeRegisterPlacement(autoNodes: ArrayBuffer[AutoNode]) = { 
    val nodeArrivalTimes = new HashMap[AutoNode, Double]
    val forwardArrivalTimes = new HashMap[AutoNode, Double]
    val backwardArrivalTimes = new HashMap[AutoNode, Double]

    val stageDelays = ArrayBuffer.fill(pipelineLength)(0.0)
    
    def calculateArrivalTimes() = {
      def findArrivalTime(node: AutoNode, dir: Direction): Double = {
        var arrivalTimes = forwardArrivalTimes
        if(dir == FORWARD){
          arrivalTimes = forwardArrivalTimes
        } else {
          arrivalTimes = backwardArrivalTimes
        }

        if(arrivalTimes.contains(node)){
          return arrivalTimes(node)
        } else if(node.stages.length > 1 && (node.stages(0) != node.stages(1))) {
          arrivalTimes(node) = 0.0
          return 0.0
        } else if(node.isDecoupledIO){
          arrivalTimes(node) = 0.0
          return 0.0
        } else {
          var arrivalTime :Double= 0.0
          val parents = new ArrayBuffer[AutoNode]
          if(dir == FORWARD){
            for(input <- node.inputs){
              parents += input
            }
          } else {
            for(consumer <- node.consumers){
              parents += consumer
            }
          }
          for(parent <- parents){
            arrivalTime = Math.max(arrivalTime, findArrivalTime(parent, dir))
          }
          arrivalTimes(node) = arrivalTime + node.delay
          return arrivalTimes(node)
        }
      }
      //find forward delay times
      forwardArrivalTimes.clear()
      for(node <- autoNodes){
        if(node.stages.length > 1 && (node.stages(0) != node.stages(1))){
          for(input <- node.inputs){
            findArrivalTime(input, FORWARD)
          }
        }
      }
      for(node <- autoSinkNodes){
        findArrivalTime(node, FORWARD)
      }
      //find backward delay times
      backwardArrivalTimes.clear()
      for(node <- autoNodes){
        if(node.stages.length > 1 && (node.stages(0) != node.stages(1))){
          for(consumer <- node.consumers){
            findArrivalTime(consumer, BACKWARD)
          }
        }
      }
      for(node <- autoSourceNodes){
        findArrivalTime(node, BACKWARD)
      }
    }
    
    def findUnpipelinedPathLength(): Double = {
      val nodeArrivalTimes = new HashMap[AutoNode, Double]
      def calculateArrivalTimes() = {
        def findArrivalTime(node: AutoNode) : Double = {
          if(nodeArrivalTimes.contains(node)){
            return nodeArrivalTimes(node)
          } else if(node.isDecoupledIO){
            nodeArrivalTimes(node) = 0.0
            return 0.0
          } else {
            var arrivalTime: Double= 0.0
            val parents = new ArrayBuffer[AutoNode]
            for(input <- node.inputs){
              parents += input
            }

            for(parent <- parents){
              arrivalTime = Math.max(arrivalTime, findArrivalTime(parent))
            }
            
            nodeArrivalTimes(node) = arrivalTime + node.delay
            return nodeArrivalTimes(node)
          }
        }
        nodeArrivalTimes.clear()
        for(node <- autoSinkNodes){
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
      for(node <- autoNodes.filter(_.stages.length == 1)){ 
        if(forwardArrivalTimes.contains(node)){
          if(forwardArrivalTimes(node) > stageDelays(node.stages(0))){
            stageDelays(node.stages(0)) = forwardArrivalTimes(node)
          }
        }
      }
    }

    def movePipelineBoundary(boundaryNum: Int, direction: Direction) = {
      val boundaryNodes = new ArrayBuffer[AutoNode]
      val possibleMoveNodes = new ArrayBuffer[AutoNode]
      val eligibleMoveNodes = new ArrayBuffer[AutoNode]
      
      //find nodes from possibleMoveNodes that can have the pipeline boundary be legally moved in "direction" accross the node and store them in eligibleMoveNodes
      def findEligibleNodes(direction: Direction) = {
        for(node <- possibleMoveNodes){
          if(!node.isUserAnnotated && node.isInstanceOf[AutoLogic] && !node.isSource && !node.isSink){
            val nodeStage = node.stages(0)
            var parentsEligible = true
            val parents = new ArrayBuffer[AutoNode]
            
            if(direction == FORWARD){
              for(input <- node.inputs){
                parents += input
              }
            } else {
              for(consumer <- node.consumers){
                parents += consumer
              }
            }
          
            for(parent <- parents){
              Predef.assert(parent.stages.length > 0)
              Predef.assert(parent.stages.length <= 2)
              if(parent.stages.length == 1){
                parentsEligible = false
              } else {
                Predef.assert(parent.stages.contains(nodeStage))
              }
            }
            if(parentsEligible){
              eligibleMoveNodes += node
            }
          }
        }
      }
      //move pipeline boundary in "direction" across "node"
      def moveNode(movedNode: AutoNode, direction: Direction) = {
        val nodeStage = movedNode.stages(0)
        var stageDelta = 0
        if(direction == FORWARD){
          stageDelta = -1
        } else {
          stageDelta = 1
        }
        
        val nodesToMove = new ArrayBuffer[AutoNode]
        nodesToMove += movedNode
        
        for(n <- nodesToMove){
          n.stages(0) = n.stages(0) + stageDelta
        }

        val parents = new ArrayBuffer[AutoNode]
        for(input <- movedNode.inputs){
          parents += input
        }
        for(consumer <- movedNode.consumers){
          parents += consumer
        }
        
        for(parent <- parents){
          if(parent.stages.length == 2){
            if(parent.stages(0) == nodeStage){
              parent.stages(0) = parent.stages(0) + stageDelta
            } else {
              parent.stages(1) = parent.stages(1) + stageDelta
            }
            if(parent.stages(0) == parent.stages(1)){
              parent.stages -= parent.stages(1)
            }
          } else {
            parent.stages += parent.stages(0) + stageDelta
          }
        }
      }
      
      //populate boundaryNodes
	    for(node <- autoNodes.filter(_.stages.length == 2)){
	      val stages = node.stages
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
      var criticalNode: AutoNode = null
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
      } 
    }

    var iterCount = 1
    calculateArrivalTimes()
    findStageDelays()
    println(stageDelays)
    val unPipelinedDelay = findUnpipelinedPathLength()
    var oldMaxDelay = unPipelinedDelay
    println("max unpipelined path: " + unPipelinedDelay)
    println("max delay before optimizeation: " + stageDelays.max)
    //while(!(iterCount % 100 == 0 && oldMaxDelay == stageDelays.max) && iterCount < 10000){
    while(stageDelays.max > unPipelinedDelay/pipelineLength && iterCount < 5000){   
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
    visualizeAutoLogicGraph(autoNodes, "stages.gv")
    visualizeAutoLogicGraph(forwardArrivalTimes, "fdelays.gv")
    visualizeAutoLogicGraph(backwardArrivalTimes, "bdelays.gv")
  }*/

  def optimizeRegisterPlacement(autoNodes: ArrayBuffer[AutoNode]) = { 
    val nodeArrivalTimes = new HashMap[AutoNode, Double]
    val forwardArrivalTimes = new HashMap[AutoNode, Double]
    val backwardArrivalTimes = new HashMap[AutoNode, Double]

    val stageDelays = ArrayBuffer.fill(pipelineLength)(0.0)
    
    def calculateArrivalTimes() = {
      def findArrivalTime(node: AutoNode, dir: Direction): Double = {
        var arrivalTimes = forwardArrivalTimes
        if(dir == FORWARD){
          arrivalTimes = forwardArrivalTimes
        } else {
          arrivalTimes = backwardArrivalTimes
        }

        if(arrivalTimes.contains(node)){
          return arrivalTimes(node)
        } else if(node.inputStage != node.outputStage) {
          arrivalTimes(node) = 0.0
          return 0.0
        } else if(node.isDecoupledIO){
          arrivalTimes(node) = 0.0
          return 0.0
        } else {
          var arrivalTime :Double= 0.0
          val parents = new ArrayBuffer[AutoNode]
          if(dir == FORWARD){
            for(input <- node.inputs){
              parents += input
            }
          } else {
            for(consumer <- node.consumers){
              parents += consumer
            }
          }
          for(parent <- parents){
            arrivalTime = Math.max(arrivalTime, findArrivalTime(parent, dir))
          }
          arrivalTimes(node) = arrivalTime + node.delay
          return arrivalTimes(node)
        }
      }
      //find forward delay times
      forwardArrivalTimes.clear()
      for(node <- autoNodes){
        if(node.inputStage != node.outputStage){
          for(input <- node.inputs){
            findArrivalTime(input, FORWARD)
          }
        }
      }
      for(node <- autoSinkNodes){
        findArrivalTime(node, FORWARD)
      }
      //find backward delay times
      backwardArrivalTimes.clear()
      for(node <- autoNodes){
        if(node.inputStage != node.outputStage){
          for(consumer <- node.consumers){
            findArrivalTime(consumer, BACKWARD)
          }
        }
      }
      for(node <- autoSourceNodes){
        findArrivalTime(node, BACKWARD)
      }
    }
    
    def findUnpipelinedPathLength(): Double = {
      val nodeArrivalTimes = new HashMap[AutoNode, Double]
      def calculateArrivalTimes() = {
        def findArrivalTime(node: AutoNode) : Double = {
          if(nodeArrivalTimes.contains(node)){
            return nodeArrivalTimes(node)
          } else if(node.isDecoupledIO){
            nodeArrivalTimes(node) = 0.0
            return 0.0
          } else {
            var arrivalTime: Double= 0.0
            val parents = new ArrayBuffer[AutoNode]
            for(input <- node.inputs){
              parents += input
            }

            for(parent <- parents){
              arrivalTime = Math.max(arrivalTime, findArrivalTime(parent))
            }
            
            nodeArrivalTimes(node) = arrivalTime + node.delay
            return nodeArrivalTimes(node)
          }
        }
        nodeArrivalTimes.clear()
        for(node <- autoSinkNodes){
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
      for(node <- autoNodes){
        if(node.inputStage == node.outputStage){
          if(forwardArrivalTimes.contains(node)){
            if(forwardArrivalTimes(node) > stageDelays(node.inputStage)){
              stageDelays(node.inputStage) = forwardArrivalTimes(node)
            }
          }
        }
      }
    }

    def movePipelineBoundary(boundaryNum: Int, direction: Direction) = {
      val boundaryNodes = new ArrayBuffer[AutoNode]
      val possibleMoveNodes = new ArrayBuffer[AutoNode]
      val eligibleMoveNodes = new ArrayBuffer[AutoNode]
      
      //find nodes from possibleMoveNodes that can have the pipeline boundary be legally moved in "direction" accross the node and store them in eligibleMoveNodes
      def findEligibleNodes(direction: Direction) = {
        for(node <- possibleMoveNodes){
          if(!node.isUserAnnotated && node.isInstanceOf[AutoLogic] && !node.isSource && !node.isSink){
            var parentsEligible = true
            val parents = new ArrayBuffer[AutoNode]
            
            if(direction == FORWARD){
              for(input <- node.inputs){
                parents += input
              }
            } else {
              for(consumer <- node.consumers){
                parents += consumer
              }
            }
          
            for(parent <- parents){
              Predef.assert(parent.inputStage >= 0 && parent.outputStage >= 0)
              if(parent.inputStage == parent.outputStage){
                parentsEligible = false
              } else {
                if(direction == FORWARD){
                  Predef.assert(parent.outputStage == node.inputStage)
                } else {
                  Predef.assert(parent.inputStage == node.outputStage)
                }
              }
            }
            if(parentsEligible){
              eligibleMoveNodes += node
            }
          }
        }
      }
      //move pipeline boundary in "direction" across "node"
      def moveNode(movedNode: AutoNode, direction: Direction) = {
        val nodeStage = movedNode.inputStage
        var stageDelta = 0
        if(direction == FORWARD){
          stageDelta = -1
        } else {
          stageDelta = 1
        }
        
        movedNode.inputStage = movedNode.inputStage + stageDelta
        movedNode.outputStage = movedNode.outputStage + stageDelta

        val oldBoundaryWires = new ArrayBuffer[AutoWire]
        val newBoundaryWires = new ArrayBuffer[AutoWire]

        for(input <- movedNode.inputs){
          if(direction == FORWARD){
            oldBoundaryWires += input.asInstanceOf[AutoWire]
          } else {
            newBoundaryWires += input.asInstanceOf[AutoWire]
          }
        }
        for(consumer <- movedNode.consumers){
          if(direction == FORWARD){
            newBoundaryWires += consumer.asInstanceOf[AutoWire]
          } else {
            oldBoundaryWires += consumer.asInstanceOf[AutoWire]
          }
        }
       
        for(wire <- oldBoundaryWires){
          if(direction == FORWARD){
            wire.outputStage = movedNode.inputStage
          } else {
            wire.inputStage = movedNode.outputStage
          }
        }
        for(wire <- newBoundaryWires){
          if(direction == FORWARD){
            wire.inputStage = movedNode.outputStage
          } else {
            wire.outputStage = movedNode.inputStage
          }
        }
      }
      
      //populate boundaryNodes
	    for(node <- autoNodes.filter(n => n.inputStage != n.outputStage && !n.isSeqReadPort)){
        if(node.inputStage == boundaryNum && node.outputStage == boundaryNum + 1){
          boundaryNodes += node
        } else {
          if(direction == FORWARD){
            if(node.outputStage == boundaryNum + 1 && node.inputStage < boundaryNum + 1){
              boundaryNodes += node
            }
          } else if(direction == BACKWARD) {
            if(node.inputStage == boundaryNum && node.outputStage > boundaryNum){
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
      var criticalNode: AutoNode = null
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
      } 
    }

    var iterCount = 1
    calculateArrivalTimes()
    findStageDelays()
    println(stageDelays)
    val unPipelinedDelay = findUnpipelinedPathLength()
    var oldMaxDelay = unPipelinedDelay
    println("max unpipelined path: " + unPipelinedDelay)
    println("max delay before optimizeation: " + stageDelays.max)
    //while(!(iterCount % 100 == 0 && oldMaxDelay == stageDelays.max) && iterCount < 10000){
    while(stageDelays.max > unPipelinedDelay/pipelineLength && iterCount < 5000){   
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
    visualizeAutoLogicGraph(autoNodes, "stages.gv")
    visualizeAutoLogicGraph(forwardArrivalTimes, "fdelays.gv")
    visualizeAutoLogicGraph(backwardArrivalTimes, "bdelays.gv")
  }
  
  def visualizeAutoLogicGraph(autoNodes: ArrayBuffer[AutoNode], fileName: String) = {
    val outFile = new java.io.FileWriter("/home/eecs/wenyu/auto-pipelining/" + fileName)
    outFile.write("digraph G {\n")
    outFile.write("graph [rankdir=LR];\n")
    var nameEnum = 0
    val nodeNames = new HashMap[AutoNode, String]
    for(node <- autoNodes){
      var fillColor = "red"
      if(node.isInstanceOf[AutoLogic]){
        fillColor = "green"
      }
      outFile.write("n" + nameEnum + " [label=\"" + node.name + " " + """\n""" + node.inputStage + " " + node.outputStage + " " + node.propagatedTo + "\"" + ", style = filled, fillcolor = " + fillColor + "];\n")
      nodeNames(node) = "n" + nameEnum
      nameEnum = nameEnum + 1
    }
    for(node <- autoNodes){
      if(!node.isSource){
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

  def visualizeAutoLogicGraph(autoNodeMap: HashMap[AutoNode, _], fileName: String) = {
    val outFile = new java.io.FileWriter("/home/eecs/wenyu/auto-pipelining/" + fileName)
    outFile.write("digraph G {\n")
    outFile.write("graph [rankdir=LR];\n")
    var nameEnum = 0
    val nodeNames = new HashMap[AutoNode, String]
    for(node <- autoNodeMap.keys){
      var fillColor = "red"
      if(node.isInstanceOf[AutoLogic]){
        fillColor = "green"
      }
      outFile.write("n" + nameEnum + " [label=\"" + node.name + " " + """\n""" + node.stages + " " + autoNodeMap(node) + "\"" + ", style = filled, fillcolor = " + fillColor + "];\n")
      nodeNames(node) = "n" + nameEnum
      nameEnum = nameEnum + 1
    }
    for(node <- autoNodeMap.keys){
      if(!node.isSource){
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
  
  //inserts register between *input* and its consumers
  def insertRegister(input: Bits, init_value: Bits, name: String) : Bits = {
    val new_reg = Reg(Bits())
    PipelineComponentNodes += new_reg
    APConsumers(new_reg) = new ArrayBuffer[(Node, Int)]
    new_reg := input
    new_reg.comp.asInstanceOf[Reg].clock = this.clock
    when(this._reset){
      new_reg := init_value
    }
    input.pipelinedVersion = new_reg
    new_reg.unPipelinedVersion = input
    new_reg.comp.setName(name)
    for(i <- 0 until APConsumers(input).size){
      val consumer = APConsumers(input)(i)._1
      val consumerInputIndex = APConsumers(input)(i)._2
      consumer.inputs(consumerInputIndex) = new_reg
      APConsumers(new_reg) += ((consumer, consumerInputIndex))
    }
    APConsumers(input).clear()
    APConsumers(input) += ((new_reg, 0))// the 0 is questionable, may need to actually figure out the matching new_reg input index
    new_reg
  }
  
  
  //insert additional bit node between *output* and its input specified by *inputNum*
  def insertBitOnInput(output: Node, inputNum: Int): Bits = {
    val new_bits = Bits()
    PipelineComponentNodes += new_bits
    APConsumers(new_bits) = new ArrayBuffer[(Node, Int)]
    val input = output.inputs(inputNum)
    for(i <- 0 until APConsumers(input).size){
      val inputConsumer = APConsumers(input)(i)._1
      val inputConsumerInputIndex = APConsumers(input)(i)._2
      if(inputConsumer == output && inputConsumerInputIndex == inputNum){
        APConsumers(input)(i) = ((new_bits, 0))
        output.inputs(inputNum) = new_bits
        new_bits.inputs += input
        APConsumers(new_bits) += ((output,inputConsumerInputIndex))
      }
    }
    return new_bits
  }

  //inserts additional (SINGULAR) bit node between *input* and its consumers(useful for automatic pipelining)
  def insertBitOnOutputs(input: Node) : Node = {
    val new_bits = Bits()
    PipelineComponentNodes += new_bits
    APConsumers(new_bits) = new ArrayBuffer[(Node, Int)]
    new_bits.inputs += input
    for(i <- 0 until APConsumers(input).size){
      val consumer = APConsumers(input)(i)._1
      val consumerInputIndex = APConsumers(input)(i)._2
      consumer.inputs(consumerInputIndex) = new_bits
      APConsumers(new_bits) += ((consumer, consumerInputIndex))
    }
    APConsumers(input).clear()
    APConsumers(input) += ((new_bits, 0))
    new_bits
  }
  
  //replace all occurances of *delete* with *add* in *node*'s inputs list
  def replaceProducer(node: Node, delete: Node, add: Node): Unit = {
    for(i <- 0 until node.inputs.length){
      if(node.inputs(i) == delete){
        node.inputs(i) = add
        APConsumers(add) += ((node, i))
      }
    }
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
        val writeAddr = writePoint.adr
        val writeEn = writePoint.is
        val writeData = writePoint.dat
        val writeStage = Math.max(getStage(writeEn),Math.max(getStage(writeAddr), getStage(writeData)))
        Predef.assert(writeStage > -1)
        for(j <- 0 until m.io.reads.length){
          val readPoint = m.io.reads(j)
          val readAddr = readPoint.adr
          val readData = readPoint.dat
          val readEn = readPoint.is
          val readStage = Math.max(getStage(readAddr), getStage(readEn))
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
              tMemRAWHazards(((readPoint, i, stage))) = stageValidTemp && currentStageWriteEnable && readEn && (readAddr === currentStageWriteAddr)
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
      val bypassMux = MuxCase(tempBitsNode, muxMapping)
      PipelineComponentNodes += bypassMux
      APConsumers(bypassMux) = new ArrayBuffer[(Node, Int)]
      bypassMux.nameIt("bypassMux_" + reg.name)
      for (consumer <- APConsumers(tempBitsNode).map(_._1)){
        replaceProducer(consumer, tempBitsNode, bypassMux)
      }
      APConsumers(tempBitsNode).clear()//TODO may need to fill this in right here; for now, we are relying a call to findAPConsumers after the transformations are done to get the correct APConsumers after this insertion
      
    }
   
    //generate bypass logic for tmems
    var delayedBypassDatas: HashMap[(TransactionMem[_], Node), Node] = new HashMap[(TransactionMem[_], Node), Node]
    for((tMem, readPort) <- forwardedMemReadPorts){
      val forwardPoints = new HashMap[(Int, Int), Node]()// map of (writeport number, write stage) -> write data
      val readAddr = readPort.adr
      val readData = readPort.dat
      val readEn = readPort.is
      val readStage = Math.max(getStage(readAddr), Math.max(getStage(readData.asInstanceOf[Node]), getStage(readEn)))
      Predef.assert(readStage > -1)
      for(i <- 0 until tMem.io.writes.length){
        val writePoint = tMem.io.writes(i)
        if(!memNonForwardedWritePoints.contains(writePoint)){
          val writeAddr = writePoint.adr
          val writeEn = writePoint.is
          val writeData = writePoint.dat.asInstanceOf[Node]
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
      
      if(tMem.isSeqRead){
        for(((i, j), bypassData) <- forwardPoints){
          if(!delayedBypassDatas.contains(((tMem, bypassData)))){
            val delayedBypassData = Reg(Bits())
            delayedBypassData := bypassData.asInstanceOf[Bits]
            delayedBypassData.comp.asInstanceOf[Reg].clock = this.clock
            when(this._reset){
              delayedBypassData := Bits(0)
            }
            delayedBypassDatas(((tMem, bypassData))) = delayedBypassData
          }
        }
      }
      
      val muxMapping = new ArrayBuffer[(Bool, Bits)]()
      for(j <- getStage(readPort.adr) + 1 to pipelineReg.size){
        for(i <- 0 until tMem.io.writes.length){
          if(forwardPoints.contains(((i, j)))){
            val forwardCond = tMemRAWHazards(((readPort.asInstanceOf[RdIO[Data]], i, j)))
            var delayedForwardCond:Bool = null
            if(tMem.isSeqRead){
              delayedForwardCond = Reg(Bool())
              delayedForwardCond := forwardCond.asInstanceOf[Bool]
              delayedForwardCond.comp.asInstanceOf[Reg].clock = this.clock
              when(this._reset){
                delayedForwardCond := Bool(false)
              }
            }
            if(tMem.isSeqRead){
              muxMapping += ((delayedForwardCond, delayedBypassDatas( ((tMem, forwardPoints(((i, j))))) ).asInstanceOf[Bits] ))
            } else {
              muxMapping +=((forwardCond, forwardPoints(((i, j))).asInstanceOf[Bits]))
            }
            tMemRAWHazards -= ((readPort.asInstanceOf[RdIO[Data]], i, j))
            println("added fowarding point (" + readPort.dat.asInstanceOf[Node].name + ", write port #" + i + ", write stage: " + j +  ")")
          }
        }
      }
      val bypassMux = MuxCase(readPort.dat.asInstanceOf[Bits], muxMapping)
      PipelineComponentNodes += bypassMux
      APConsumers(bypassMux) = new ArrayBuffer[(Node, Int)]
      bypassMux.nameIt("bypassMux_" + tMem.name + "_" + readPort.dat.asInstanceOf[Node].name)
      val originalConsumers = readPort.dat.asInstanceOf[Bits].consumers.clone()
      for (consumer <- APConsumers(readPort.dat.asInstanceOf[Bits]).map(_._1)){
        replaceProducer(consumer, readPort.dat.asInstanceOf[Node], bypassMux)    
      }
      APConsumers(readPort.dat.asInstanceOf[Bits]).clear()//TODO may need to fill this in right here; for now, we are relying on a call to findAPConsumers after the transformations are done to get the correct APConumers after this insertion
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
        when(this._reset){
          specWriteDataReg := Bits(0)
        }
        specWriteDataReg.comp.asInstanceOf[Reg].clock = this.clock
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
      when(this._reset){
        validReg := Bool(false)
      }
      validReg.comp.asInstanceOf[Reg].clock = this.clock
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
        val writeAddr = writePoint.adr
        val writeEn = writePoint.is
        val writeData = writePoint.dat
        val writeStage = Math.max(getStage(writeEn),Math.max(getStage(writeAddr), getStage(writeData)))
        val newWriteEn = writeEn.asInstanceOf[Bool] && ~globalStall && stageValids(writeStage) && ~stageStalls(writeStage)
        
        for((consumer, consumerInputIndex) <- APConsumers(writeEn)){
          val writeEnIndex = consumer.inputs.indexOf(writeEn)
          Predef.assert(writeEnIndex > -1)
          consumer.inputs(writeEnIndex) = newWriteEn
        }

        //populate newWriteEn's consumer list
        writeEn.addConsumers
        newWriteEn.addConsumers 
      }
    }
    //wire zero to all Bool outputs if stage is not valid or is stalled; this is a hack to let outside components know that a pipeline stage is invalid
    for(ready <- ioNodes.map(_.ready).filter(_.dir == OUTPUT)){
      val readyStage = getStage(ready)
      val tempMux = Multiplex(~globalStall && stageValids(readyStage) && ~stageStalls(readyStage), ready.inputs(0), Bool(false))
      ready.inputs(0) = tempMux
    }
    for(valid <- ioNodes.map(_.valid).filter(_.dir == OUTPUT)){
      val validStage = getStage(valid)
      val tempMux = Multiplex(~globalStall && stageValids(validStage) && ~stageStalls(validStage), valid.inputs(0), Bool(false))
      valid.inputs(0) = tempMux
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

  def verifyLegalStageColoring(autoNodes: ArrayBuffer[AutoNode]) = {
    //add check that no node is both a register read point and a register write point
    
    //check that all nodes have a stage number
    for(node <- autoNodes){
      Predef.assert(node.propagatedTo, "progate stages failed to give all nodes a stage number")
      Predef.assert(node.inputStage >= 0)
      Predef.assert(node.outputStage >= 0)
      Predef.assert(node.outputStage >= node.inputStage)
    }

    //check that all combinational logic nodes have all inputs from the same stage
    for(node <- autoNodes.filter(_.isInstanceOf[AutoLogic])){
      if(node.isSeqReadPort){
        Predef.assert(node.outputStage == node.inputStage + 1)
      } else {
        Predef.assert(node.outputStage == node.inputStage)
      }
      for(input <- node.inputs){
        Predef.assert(input.outputStage == node.inputStage, "combinational logic node does not have inputs all in the same stage")
      }
    }
    
    //check that all architectural register has write points in the same stage and that its write stage >= its read stage
    
    //check all tmems read and write ports are annotated; data, addr, and enable of each port is in same stage; all read ports are in the same stage; all write ports are in same stage; write ports have stage >= read port stage
    
    //check that all IO nodes are in the same stage
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
        for(consumer <- APConsumers(node).map(_._1)){
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
      outFile.write("n" + nameEnum + " [label=\"" + node.name + " " + fillerStatus + """\n""" + nodeMap(node) + "\"" + ", style = filled, fillcolor = red" + "];\n")
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
  
  def visualizePipeColoring(nodeMap: HashMap[Node, ArrayBuffer[Int]], fileName: String) = {
    val outFile = new java.io.FileWriter("/home/eecs/wenyu/auto-pipelining/" + fileName)
    outFile.write("digraph G {\n")
    outFile.write("graph [rankdir=LR];\n")
    var nameEnum = 0
    val nodeNames = new HashMap[Node, String]
    for(node <- nodeMap.keys){
      var fillerStatus = ""
      /*var fillColor = "white"
      if(nodeMap(node).length == 1){
        if(nodeMap(node)(0) == 0){
          fillColor = "red"
        } else if(nodeMap(node)(0) == 1){
          fillColor = "green"
        } else if(nodeMap(node)(0) == 2){
          fillColor = "blue"
        } else if(nodeMap(node)(0) == 3){
          fillColor = "orange"
        }
      }*/
      var fillColor = "blue"
      outFile.write("n" + nameEnum + " [label=\"" + node.name + " " + fillerStatus + """\n""" + nodeMap(node) + "\"" + ", style = filled, fillcolor = " + fillColor + "];\n")
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

