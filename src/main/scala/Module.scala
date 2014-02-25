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
  var dontFindCombLoop = false
  var isDebug = false;
  var isCSE = false
  var isIoDebug = true;
  var isClockGatingUpdates = false;
  var isClockGatingUpdatesInline = false;
  var isVCD = false;
  var isInlineMem = true;
  var isGenHarness = false;
  var isReportDims = false;
  var isPruning = false;
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
      if (io.dir == null)
         ChiselErrors += new ChiselError(() => {"All IO's must be ports (dir set): " + io}, io.line);
      // else if (io.width_ == -1)
      //   ChiselErrors += new ChiselError(() => {"All IO's must have width set: " + io}, io.line);
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
    isCSE = false
    isIoDebug = true;
    isClockGatingUpdates = false;
    isClockGatingUpdatesInline = false;
    isVCD = false;
    isReportDims = false;
    targetDir = "."
    components.clear();
    compStack.clear();
    stackIndent = 0;
    printfs.clear();
    printStackStruct.clear();
    procs.clear();
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

    isInGetWidth = false
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

  // XXX Remove and instead call current()
  def getComponent(): Module = if(compStack.length != 0) compStack.top else null
  def current: Module = getComponent

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
  val debugs = HashSet[Node]();

  val switchKeys = Stack[Bits]()
  val whenConds = Stack[Bool]()
  private lazy val trueCond = Bool(true)
  def hasWhenCond: Boolean = !whenConds.isEmpty
  def whenCond: Bool = if (hasWhenCond) whenConds.top else trueCond

  val nodes = new ArrayBuffer[Node]
  val mods = new ArrayBuffer[Node];
  val omods = new ArrayBuffer[Node];

  val regs  = new ArrayBuffer[Reg];
  val nexts = new ScalaQueue[Node];
  val names = new HashMap[String, Node]
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
    val p = new Printf(Module.current.whenCond && !this.reset, message, args)
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
    getPathName()
  }
  def getPathName(separator: String = "_"): String = {
    if ( parent == null ) name else parent.getPathName(separator) + separator + name;
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

  override val hashCode: Int = components.size
  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
}

