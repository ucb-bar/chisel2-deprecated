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
import java.io.File;
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import scala.sys.process._
import scala.math.max;
import Node._
import Literal._
import Component._
import Bundle._
import ChiselError._

object Component {
  var resourceStream = getClass().getResourceAsStream("/emulator.h")
  var saveWidthWarnings = false
  var saveConnectionWarnings = false
  var saveComponentTrace = false
  var saveDot = false
  var dontFindCombLoop = false
  var widthWriter: java.io.FileWriter = null;
  var connWriter: java.io.FileWriter = null;
  var isDebug = false;
  var isIoDebug = true;
  var isClockGatingUpdates = false;
  var isClockGatingUpdatesInline = false;
  var isVCD = false;
  var isInlineMem = true;
  var isFolding = true;
  var isGenHarness = false;
  var isReportDims = false;
  var scanFormat = "";
  var scanArgs: ArrayBuffer[Node] = null;
  var printFormat = "";
  var printArgs: ArrayBuffer[Node] = null;
  var tester: Tester[Component] = null;
  var includeArgs: List[String] = Nil;
  var targetDir: String = null;
  var isEmittingComponents = false;
  var isCompiling = false;
  var isCheckingPorts = false
  var isTesting = false;
  var backend: Backend = null;
  var topComponent: Component = null;
  val components = ArrayBuffer[Component]();
  val procs = ArrayBuffer[proc]();
  val resetList = ArrayBuffer[Node]();
  val muxes = ArrayBuffer[Node]();
  val nodes = ArrayBuffer[Node]()
  var ioMap = new HashMap[Node, Int];
  var chiselOneHotMap = new HashMap[(UFix, Int), Bits]
  var chiselOneHotBitMap = new HashMap[(Bits, Int), Bool]
  var chiselAndMap = new HashMap[(Node, Node), Bool]
  var searchAndMap = true
  var ioCount = 0;
  val compStack = new Stack[Component]();
  var stackIndent = 0;
  var printStackStruct = ArrayBuffer[(Int, Component)]();

  def splitArg (s: String) : List[String] = s.split(' ').toList;

  // TODO: MAYBE CHANGE NAME TO INITCOMPONENT??
  // TODO: ADD INIT OF TOP LEVEL NODE STATE
  // TODO: BETTER YET MOVE ALL TOP LEVEL STATE FROM NODE TO COMPONENT
  def defTests(nodes: Node*)(body: => Boolean) = {
  }
  def initChisel () = {
    ChiselError.ChiselErrors.clear();
    saveWidthWarnings = false
    saveConnectionWarnings = false
    saveComponentTrace = false
    saveDot = false
    dontFindCombLoop = false
    widthWriter = null
    connWriter = null
    isGenHarness = false;
    isDebug = false;
    isIoDebug = true;
    isClockGatingUpdates = false;
    isClockGatingUpdatesInline = false;
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

    /* Re-initialize global variables defined in object Node {} */
    isCoercingArgs = true
    isInGetWidth = false
    conds.clear()
    conds.push(Bool(true))
    keys.clear()
  }

  def ensureDir(dir: String) = {
    val d = dir + (if (dir == "" || dir(dir.length-1) == '/') "" else "/");
    new File(d).mkdirs();
    d
  }

  //component stack handling stuff

  def isSubclassOfComponent(x: java.lang.Class[_]): Boolean = {
    val classString = x.toString;
    if(classString == "class java.lang.Object") {
      return false;
    } else if(classString == "class Chisel.Component") {
      return true;
    } else {
      isSubclassOfComponent(x.getSuperclass)
    }
  }

  def printStack = {
    var res = ""
    for((i, c) <- printStackStruct){
      res += (genIndent(i) + c.moduleName + " " + c.name + "\n")
    }
    println(res)
  }

  def genIndent(x: Int): String = {
    if(x == 0) "" else "    " + genIndent(x-1);
  }

  def push(c: Component){
    if(compStack.isEmpty){
      compStack.push(c);
      printStackStruct += ((stackIndent, c));
    } else {
      val st = Thread.currentThread.getStackTrace;
      //for(elm <- st)
      //println(elm.getClassName + " " + elm.getMethodName + " " + elm.getLineNumber);
      var skip = 3;
      for(elm <- st){
        if(skip > 0) {
          skip -= 1;
        } else {
          if(elm.getMethodName == "<init>") {

            val className = elm.getClassName;

            if(isSubclassOfComponent(Class.forName(className)) && !c.isSubclassOf(Class.forName(className))) {
              if(saveComponentTrace) {
                println("marking " + className + " as parent of " + c.getClass);
              }
              while(compStack.top.getClass != Class.forName(className)){
                pop;
              }

              val dad = compStack.top;
              c.parent = dad;
              dad.children += c;

              compStack.push(c);
              stackIndent += 1;
              printStackStruct += ((stackIndent, c));
              return;
            }
          }
        }
      }
    }
  }

  def pop(){
    val node = compStack.pop;
    stackIndent -= 1;
    node.level = 0;
    for(child <- node.children) {
      node.level = math.max(node.level, child.level + 1);
    }
  }

  def getComponent(): Component = if(compStack.length != 0) compStack.top else {
    // val st = Thread.currentThread.getStackTrace;
    // println("UNKNOWN COMPONENT ");
    // for(frame <- st)
    //   println("  " + frame);
    null
  };

  def assignResets() {
    for(c <- components) {
      if(c.reset.inputs.length == 0 && c.parent != null) {
        // XXX Why += when length == 0? why inputs += reset and not reset=reset?
        c.reset.inputs += c.parent.reset
      }
    }
  }
}


abstract class Component(resetSignal: Bool = null) {
  /** A backend(Backend.scala) might generate multiple module source code
    from one Component, based on the parameters to instanciate the component
    instance. Since we do not want to blindly generate one module per instance
    the backend will keep a cache of each module's implementation source code
    and discard textual duplicates. By walking the nodes from level zero
    (leafs) to level N (root), we are guarenteed to generate all
    Component/modules source text before their first instantiation. */
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
  var parent: Component = null;
  var containsReg = false;
  val children = new ArrayBuffer[Component];
  var inputs = new ArrayBuffer[Node];
  var outputs = new ArrayBuffer[Node];
  val asserts = ArrayBuffer[Assert]();
  val blackboxes = ArrayBuffer[BlackBox]();
  val debugs = HashSet[Node]();

  val nodes = new HashSet[Node]()
  val mods  = new ArrayBuffer[Node];
  val omods = new ArrayBuffer[Node];
  // val gmods = new ArrayBuffer[Node];
  val regs  = new ArrayBuffer[Reg];
  val nexts = new ScalaQueue[Node];
  var nindex = -1;
  var defaultWidth = 32;
  var pathParent: Component = null;
  var verilog_parameters = "";

  components += this;
  push(this);

  //true if this is a subclass of x
  def isSubclassOf(x: java.lang.Class[_]): Boolean = {
    var className: java.lang.Class[_] = this.getClass;
    while(className.toString != x.toString){
      if(className.toString == "class Chisel.Component") return false;
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
  def ownIo() = {
    val wires = io.flatten;
    for ((n, w) <- wires) {
      // This assert is a sanity check to make sure static resolution of IOs didn't fail
      scala.Predef.assert(this == w.staticComp, {
        println("Statically resolved component differs from dynamically resolved component of IO: " + w + " crashing compiler")})
      w.component = this;
    }
  }


  def findBinding(m: Node): Binding = {
    // println("FINDING BINDING " + m + " OUT OF " + bindings.length + " IN " + this);
    for (b <- bindings) {
      // println("LOOKING AT " + b + " INPUT " + b.inputs(0));
      if (b.inputs(0) == m) {
        return b
      }
    }
    // println("UNABLE TO FIND BINDING FOR " + m);
    return null
  }
  //def io: Data = ioVal;
  def io: Data
  def nextIndex : Int = { nindex = nindex + 1; nindex }

  var isWalking = new HashSet[Node];
  var isWalked = new HashSet[Node];
  override def toString: String = name
  def wires: Array[(String, Bits)] = {
    if (wiresCache == null) {
      wiresCache = io.flatten;
    }
    wiresCache
  }
  def assert(cond: Bool, message: String) =
    asserts += Assert(cond, message);
  def debug(x: Node) =
    debugs += x.getNode
  def <>(src: Component) = io <> src.io;
  def apply(name: String): Data = io(name);
  // COMPILATION OF REFERENCE
  def emitDec(b: Backend): String = {
    var res = "";
    val wires = io.flatten;
    for ((n, w) <- wires)
      res += b.emitDec(w);
    res
  }

  val reset = Bool(INPUT)
  resetList += reset
  reset.component = this
  reset.setName("reset")
  if(!(resetSignal == null)) reset := resetSignal

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

    for(a <- asserts)
      res.enqueue(a)
    for(b <- blackboxes)
      res.enqueue(b.io)
    for(c <- components)
      for((n, io) <- c.io.flatten)
        res.enqueue(io)

    for(r <- resetList)
      res.enqueue(r)

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

  def inferAll(): Unit = {
    println("started inference")
    val nodesList = ArrayBuffer[Node]()
    bfs { nodesList += _ }

    def verify = {
      var hasError = false
      for (elm <- nodesList) {
        if (elm.infer || elm.width == -1) {
          println("chisel: error: Could not infer the width on: " + elm)
          hasError = true
        }
      }
      if (hasError) throw new Exception("Could not elaborate code due to uninferred width(s)")
    }

    var count = 0
    // Infer all node widths by propagating known widths
    // in a bellman-ford fashion.
    for(i <- 0 until nodesList.length) {

      var done = true;
      for(elm <- nodesList){
        val updated = elm.infer
        done = done && !updated
        //done = done && !(elm.infer) TODO: why is this line not the same as previous two?
      }

      count += 1

      if(done){
        verify
        println(count)
        println("finished inference")
        return;
      }
    }
    verify
    println(count)
    println("finished inference")
  }

  /** All classes inherited from Data are used to add type information
   and do not represent logic itself. */
  def removeTypeNodes() {
    println("started flattenning")

    def getNode(x: Node): Node = {
      var res = x
      while(res.isTypeNode && res.inputs.length != 0){
        res = res.inputs(0)
      }
      res
    }

    var count = 0
    bfs {x =>
      scala.Predef.assert(!x.isTypeNode)
      count += 1
      for (i <- 0 until x.inputs.length)
        if (x.inputs(i) != null && x.inputs(i).isTypeNode) {
          x.inputs(i) = getNode(x.inputs(i))
        }
    }

    println(count)
    println("finished flattening")
  }

  def forceMatchingWidths = {
    println("start width checking")
    bfs(_.forceMatchingWidths)
    println("finished width checking")
  }

  def findConsumers() = {
    for (m <- mods) {
      m.addConsumers;
    }
  }
  def findRoots(): ArrayBuffer[Node] = {
    val roots = new ArrayBuffer[Node];
    for (a <- asserts)
      roots += a.cond;
    for (b <- blackboxes)
      roots += b.io;
    for (m <- mods) {
      m match {
        case io: Bits => if (io.dir == OUTPUT) { if (io.consumers.length == 0) roots += m; }
        case d: Delay => roots += m;
        case any      =>
      }
    }
    roots
  }
  def visitNodes(roots: Array[Node]) = {
    val stack = new Stack[(Int, Node)]();
    for (root <- roots)
      stack.push((0, root));
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
  def findOrdering() = visitNodes(findRoots().toArray);

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
    println("%6s: %s".format("name", "count"));
    for (n <- hist.keys.toList.sortWith((a, b) => a < b))
      println("%6s: %4d".format(n, hist(n)));
    println("%6s: %s".format("width", "count"));
    for (w <- whist.keys.toList.sortWith((a, b) => a < b))
      println("%3d: %4d".format(w, whist(w)));
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
  def collectNodes(c: Component) = {
    for (m <- c.mods) {
      // println("M " + m.name);
      m match {
        case io: Bits  =>
          if (io.dir == INPUT) {
            inputs += m;
          } else if (io.dir == OUTPUT) {
            outputs += m;
          }
        case r: Reg    => regs += r;
        case other     =>
      }
    }
  }
  def traceableNodes = io.traceableNodes;
  def childrenContainsReg: Boolean = {
    var res = containsReg;
    if(children.isEmpty) return res;
    for(child <- children){
      res = res || child.containsReg || child.childrenContainsReg;
      if(res) return res;
    }
    res
  }

  // 1) name the component
  // 2) name the IO
  // 3) name and set the component of all statically declared nodes through introspection
  def markComponent() = {
    ownIo();
     // We are going through all declarations, which can return Nodes,
     // ArrayBuffer[Node], Cell, BlackBox and Components.
     for (m <- getClass().getDeclaredMethods) {
       val name = m.getName();
       val types = m.getParameterTypes();
       if (types.length == 0 && name != "test") {
         val o = m.invoke(this);
         o match {
         case node: Node => {
           if (node.isReg || node.isClkInput) containsReg = true;
         }
         case buf: ArrayBuffer[Node] => {
           if(!buf.isEmpty && buf(0).isInstanceOf[Node]){
             for(elm <- buf){
               if (elm.isReg || elm.isClkInput) {
                 containsReg = true;
               }
             }
           }
         }
         // TODO: THIS CASE MAY NEVER MATCH
         case bufbuf: ArrayBuffer[ArrayBuffer[_]] => {
           var i = 0;
           println(name);
           for(buf <- bufbuf){
             var j = 0;
             for(elm <- buf){
               elm match {
                 case node: Node => {
                   if (node.isReg || node.isClkInput) {
                     containsReg = true;
                   }
                   j += 1;
                 }
                 case any =>
               }
             }
             i += 1;
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
         case comp: Component => {
           comp.pathParent = this;
         }
         case any =>
       }
     }
     }
  }


  def genAllMuxes = {
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
  def verifyAllMuxes = {
    for(m <- muxes) {
      if(m.inputs(0).width != 1 && m.component != null && (!isEmittingComponents || !m.component.isInstanceOf[BlackBox])) {
        ChiselErrors += ChiselError({"Mux " + m.name + " has " + m.inputs(0).width + "-bit selector " + m.inputs(0).name}, m);
      }
    }
  }
  /* XXX Not sure what the two following do.
   They never get overridden yet it is called
   for each component (See Backend implementations). */
  def elaborate(fake: Int = 0) = {}
  def postMarkNet(fake: Int = 0) = {}
  def stripComponent(s: String) = s.split("__").last

    /** Returns the absolute path to a component instance from toplevel. */
  def getPathName: String = {
    if ( parent == null ) name else parent.getPathName + "_" + name;
  }

  def traceNodes() {
    val queue = Stack[() => Any]();

    /* XXX Why do we do something different here? */
    if (!backend.isInstanceOf[VerilogBackend]) {
      queue.push(() => io.traceNode(this, queue));
      /* This is ugly and most likely unnecessary but as long as we are not
       sure of the subtle consequences of tracing through blackboxes, let's
       have the code here (instead of Verilog.doCompile). */
      for (c <- components) {
        c match {
          case x: BlackBox => c.traceNodes();
          case _ =>
        }
      }
    } else {
      for (c <- components) {
        queue.push(() => c.reset.traceNode(c, queue))
        queue.push(() => c.io.traceNode(c, queue))
      }
    }
    for (a <- asserts)
      queue.push(() => a.traceNode(this, queue));
    for (b <- blackboxes)
      queue.push(() => b.io.traceNode(this, queue));
    while (queue.length > 0) {
      val work = queue.pop();
      work();
    }
  }

  def findCombLoop() = {
    println("BEGINNING COMBINATIONAL LOOP CHECKING")

    // Tarjan's strongly connected components algorithm to find loops
    println("BEGINNING SEARCHING CIRCUIT FOR COMBINATIONAL LOOP")
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
    println("FINISHED ANALYZING CIRCUIT")
    var containsCombPath = false
    for (nodelist <- sccList) {
      if(nodelist.length > 1) {
        containsCombPath = true
        println("FOUND COMBINATIONAL PATH!")
        for((node, ind) <- nodelist zip nodelist.indices) {
          val ste = node.line
          println("  (" + ind +  ") on line " + ste.getLineNumber +
                                  " in class " + ste.getClassName +
                                  " in file " + ste.getFileName +
                                  ", " + node.name)
        }
      }
    }
    if(containsCombPath) throw new Exception("CIRCUIT CONTAINS COMBINATIONAL PATH")
    println("NO COMBINATIONAL LOOP FOUND")
  }
  def isInput(node: Node) =
    node match { case b:Bits => b.dir == INPUT; case o => false }
  def keepInputs(nodes: Seq[Node]): Seq[Node] =
    nodes.filter(isInput)
  def removeInputs(nodes: Seq[Node]): Seq[Node] =
    nodes.filter(n => !isInput(n))

}
