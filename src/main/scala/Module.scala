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

object Module {
  /* We have to keep a list of public methods which happen to be public,
   have no arguments yet should not be used to generate C++ or Verilog code. */
  val keywords = HashSet[String]("test")

  var _scope: Scope = null

  def scope(): Scope = {
    if( _scope == null ) {
      _scope = new Scope()
    }
    _scope
  }

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
  val nodes = ArrayBuffer[Node]()
  val blackboxes = ArrayBuffer[BlackBox]()
  /* XXX ioMap is solely set in the Bits constructor and solely used in
   a sort function in Bundle. It is unclear what its purpose is.  */
  var ioMap = new HashMap[Bits, Int];
  var ioCount = 0;
  var chiselOneHotMap = new HashMap[(UInt, Int), UInt]
  var chiselOneHotBitMap = new HashMap[(Bits, Int), Bool]
  var chiselAndMap = new HashMap[(Bits, Bits), Bool]
  var searchAndMap = true
  val printfs = ArrayBuffer[Printf]()
  val randInitIOs = new ArrayBuffer[Node]()

  /* Any call to a *Module* constructor without a proper wrapping
   into a Module.apply() call will be detected when trigger is false. */
  var trigger: Boolean = false

  def apply[T <: Module](c: => T): T = {
    trigger = true
    /* *push* is done in the Module constructor because we don't have
     a *this* pointer before then, yet we need to store it before the subclass
     constructors are built. */
    val res = c
    Module.scope.pop()
    for ((n, io) <- res.wires) {
      io.isIo = true
    }
    res
  }

  def splitArg (s: String) : List[String] = s.split(' ').toList;

  def initChisel () {
    ChiselError.clear();
    Module._scope = null
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
    printfs.clear();
    blackboxes.clear();
    ioMap.clear()
    ioCount = 0;
    chiselOneHotMap.clear()
    chiselOneHotBitMap.clear()
    chiselAndMap.clear()
    searchAndMap = false
    isEmittingComponents = false;
    isCompiling = false;
    isCheckingPorts = false
    isTesting = false;
    backend = new CppBackend
    topComponent = null;
    randInitIOs.clear()

    /* Re-initialize global variables defined in object Node {} */
    nodes.clear()
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

  def setAsTopComponent(mod: Module) {
    topComponent = mod;
  }

  def current: Module = scope.compStack.top
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
abstract class Module(var clock: Clock = null, var reset: Bool = null) {
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
  var parent: Module = null;
  val children = new ArrayBuffer[Module];
  val debugs = HashSet[Node]();
  val mems = new ArrayBuffer[MemDelay];

  /* These are caches generated by addConsumers. */

  /** Accessible nodes from the toplevel IOs */
  val nodes = new ArrayBuffer[Node]

  /** All clocks reachable from this module outputs and its children outputs */
  var clocks = new ArrayBuffer[Update]

  /** All resets reachable from this module outputs and its children outputs */
  var resets = new ArrayBuffer[Node]

  val omods = new ArrayBuffer[Node];

  var pathParent: Module = null;
  var verilog_parameters = "";

  Module.components += this;
  Module.scope.push(this);

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

  def io: Data

  var isWalking = new HashSet[Node];
  var isWalked = new HashSet[Node];
  // override def toString: String = name this one isn't really working...

  /** Returns a sequence of pairs (name, IOBound) that represents
    the I/O interface of the component.
    */
  def wires: Array[(String, Node)] = {
    io.flatten.map(x => (x._1, x._2.node))
  }

  /** Add an assertion in the code generated by a backend. */
  def assert(cond: Bool, message: String): Unit = {
    debug(new Assert((cond || Module.scope.reset).node, message))
  }

  /** Insures a backend does not remove a signal because it is unreachable
    from the outputs. */
  def debug(x: Data): Unit = {
    // XXX Because We cannot guarentee x is flatten later on in collectComp.
    debug(x.toBits.node)
  }

  def debug(node: Node): Unit = {
    // XXX Because We cannot guarentee x is flatten later on in collectComp.
    node.component = this
    debugs += node
  }

  def printf(message: String, args: Data*): Unit = {
    val p = new Printf((Module.scope.topCond && !Module.scope.reset).node, message, args.map(_.toBits.node))
    Module.printfs += p
    debug(p)
    p.inputs.foreach(debug _)
  }

  def <>(src: Module) {
    io <> src.io
  }

  def initializeBFS: ScalaQueue[Node] = {
    val res = new ScalaQueue[Node]

    for (c <- Module.components; a <- c.debugs)
      res.enqueue(a)
    for(b <- Module.blackboxes)
      res.enqueue(b.io.toBits.node)
    for(c <- Module.components)
      for((n, io) <- c.io.flatten)
        res.enqueue(io.node)

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
      res.push(flat.node)
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

  /** Since we are relying on the out-degree of nodes (i.e. consumers.length),
    this method should only be called after the forward edges have been
    constructed. */
  def findRoots(): ArrayBuffer[Node] = {
    val roots = new ArrayBuffer[Node];
    for (c <- Module.components) {
      roots ++= c.debugs
    }
    for (b <- Module.blackboxes)
      roots += b.io.toBits.node;
    for (m <- nodes) {
      m match {
        case io: IOBound => {
          if (io.isDirected(OUTPUT)) {
            if (io.consumers.length == 0) roots += m;
          }
        }
        case d: Delay => roots += m;
        case any      =>
      }
    }
    roots
  }

  /** Set of nodes connecting this component to its sub-components.
    */
  def bindings: ArrayBuffer[IOBound] = {
    (nodes.filter(x => x.isInstanceOf[IOBound]
      && x.inputs.length > 0 && x.inputs(0) != null
      && x.component != x.inputs(0).component) -- outputs()).map(
      x => x.asInstanceOf[IOBound])
  }

  /** Returns the roots of the component which are connected to IO
    or debug fields.

    Nodes which are unreachable from those roots will not be returned
    even though they may be roots (zero output degree) by themselves.
    */
  def outputs(): ArrayBuffer[Node] = {
    var res = ArrayBuffer[Node]()
    for( a <- this.debugs ) {
      res += a
    }
    for((n, flat) <- this.io.flatten) {
      res += flat.node
    }
    for( m <- mems ) {
      res ++= m.writes()
    }
    res
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
      if( comp != null ) {
        // XXX What to do when the node is not part of any components?
        if (newDepth == -1) {
          comp.omods += node;
        } else {
          node.visitDepth = max(node.visitDepth, newDepth);
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
  }

  def findOrdering(): Unit = visitNodes(findRoots().toArray);

  def findGraphDims(): (Int, Int, Int) = {
    var maxDepth = 0;
    val imods = new ArrayBuffer[Node]();
    for (m <- nodes) {
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
        case m: MuxOp => name = "Mux";
        case op: Op => name = op.name;
        case o      => name = name.substring(name.indexOf('.') + 1);
      }
      if (hist.contains(name)) {
        hist(name) = hist(name) + 1;
      } else {
        hist(name) = 1;
      }
    }
    for (m <- imods)
      maxDepth = max(m.visitDepth, maxDepth);
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
      widths(m.visitDepth) = widths(m.visitDepth) + 1;
    var numNodes = 0;
    for (m <- imods)
      numNodes += 1;
    var maxWidth = 0;
    for (i <- 0 until maxDepth + 1)
      maxWidth = max(maxWidth, widths(i));
    (numNodes, maxWidth, maxDepth)
  }


  /** Returns true if this module contains at least one reachable
    register. */
  def containsReg: Boolean = {
    val delays = new ByClassVisitor[Delay]()
    GraphWalker.depthFirst(io.nodes, delays, new InnerEdges())
    delays.items.length > 0
  }

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

  def stripComponent(s: String): String = s.split("__").last

    /** Returns the absolute path to a component instance from toplevel. */
  def getPathName: String = {
    if ( parent == null ) name else parent.getPathName + "_" + name;
  }


  def findCombLoop() {
    // Tarjan's strongly connected components algorithm to find loops
    var sccIndex = 0
    val stack = new Stack[Node]
    val sccList = new ArrayBuffer[ArrayBuffer[Node]]

/* XXX do the right thing here!
    GraphWalker.tarjan(nodes, /*XXX*/)
 */
    // check for combinational loops
    var containsCombPath = false
    for (nodelist <- sccList) {
      if(nodelist.length > 1) {
        containsCombPath = true
        ChiselError.error("FOUND COMBINATIONAL PATH!")
        for((node, ind) <- nodelist zip nodelist.indices) {
/* XXX fix later: really needs node.line here.
          val ste = null
          ChiselError.error("  (" + ind +  ") on line " + ste.getLineNumber +
                                  " in class " + ste.getClassName +
                                  " in file " + ste.getFileName +
                                  ", " + node.name)
 */
        }
      }
    }
  }

  def isInput(node: Node): Boolean =
    node match { case b:IOBound => b.isDirected(INPUT); case o => false }
  def keepInputs(nodes: Seq[Bits]): Seq[Bits] =
    nodes.filter(n => isInput(n.node))
  def removeInputs(nodes: Seq[Bits]): Seq[Bits] =
    nodes.filter(n => !isInput(n.node))

  override val hashCode: Int = Module.components.size
  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
}

