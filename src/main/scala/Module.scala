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
import scala.collection.mutable.{ArrayBuffer, Stack, BitSet}
import scala.collection.mutable.{LinkedHashSet, HashSet, HashMap}
import scala.collection.mutable.{Queue=>ScalaQueue}
import java.lang.reflect.Modifier._
import scala.sys.process._
import scala.math.max
import Literal._
import Bundle._
import ChiselError._
import Module._

object Module {
  def apply[T <: Module](c: => T): T = {
    Driver.modStackPushed = true
    /* *push* is done in the Module constructor because we don't have
     a *this* pointer before then, yet we need to store it before the subclass
     constructors are built. */
    val res = c
    pop()
    for ((n, io) <- res.wires) {
      if (io.dir == null)
         ChiselErrors += new ChiselError(() => {"All IO's must be ports (dir set): " + io}, io.line)
      // else if (io.width_ == -1)
      //   ChiselErrors += new ChiselError(() => {"All IO's must have width set: " + io}, io.line)
      io.isIo = true
    }
    res
  }

  private def push(c: Module) {
    if (!Driver.modStackPushed) {
      ChiselError.error(
        c.getClass.getName + " was not properly wrapped into a module() call.")
    }
    Driver.modStackPushed = false
    Driver.compStack.push(c)
    Driver.printStackStruct += ((Driver.stackIndent, c))
    Driver.stackIndent += 1
  }

  private def pop(){
    val c = Driver.compStack.pop
    if( !Driver.compStack.isEmpty ) {
      val dad = Driver.compStack.top
      c.parent = dad
      dad.children += c
    }
    Driver.stackIndent -= 1
    c.level = 0
    for(child <- c.children) {
      c.level = math.max(c.level, child.level + 1)
    }
  }

  // XXX Remove and instead call current()
  def getComponent(): Module = if(Driver.compStack.length != 0) Driver.compStack.top else null
  def current: Module = getComponent

  // despite being notionally internal, these have leaked into the API
  def backend: Backend = Driver.backend
  def components: ArrayBuffer[Module] = Driver.components

  protected[Chisel] def asModule(m: Module)(block: => Unit): Unit = {
    Driver.modStackPushed = true
    push(m)
    block
    pop()
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
  var pName = ""
  var named = false;
  val bindings = new ArrayBuffer[Binding];
  var wiresCache: Array[(String, Bits)] = null;
  var parent: Module = null;
  val children = ArrayBuffer[Module]()
  val debugs = LinkedHashSet[Node]()
  val printfs = ArrayBuffer[Printf]()
  val asserts = ArrayBuffer[Assert]()

  val switchKeys = Stack[Bits]()
  val whenConds = Stack[Bool]()
  private lazy val trueCond = Bool(true)
  def hasWhenCond: Boolean = !whenConds.isEmpty
  def whenCond: Bool = if (hasWhenCond) whenConds.top else trueCond

  val nodes = new ArrayBuffer[Node]
  val mods = new ArrayBuffer[Node];
  val omods = new ArrayBuffer[Node];
  val signals = new LinkedHashSet[Node]

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

  Driver.components += this
  push(this)

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
    val a = new Assert(!Module.current.whenCond || cond, this.reset, message)
    debug(a)
    asserts += a
  }

  /** Insures a backend does not remove a signal because it is unreachable
    from the outputs. */
  def debug(x: Node): Unit = {
    // XXX Because We cannot guarentee x is flatten later on in collectComp.
    x.getNode.component = this
    debugs += x.getNode
  }

  def counter(x: Node) {
    x.getNode match {
      case _: VecLike[_] =>
      case _: Aggregate =>
      case _: ROMData =>
      case _: Literal =>
      case any if !(Driver.signals contains any) => {
        if (!any.isIo) debug(x)
        Driver.signals += any
      }
      case _ =>
    }
  }

  def counter(xs: Node*) {
    xs.foreach(counter _)
  }

  def printf(message: String, args: Node*): Unit = {
    val p = new Printf(Module.current.whenCond && !this.reset, message, args)
    printfs += p
    debug(p)
    p.inputs.foreach(debug _)
    for (arg <- args)
      if (arg.isInstanceOf[Aggregate])
        ChiselErrors += new ChiselError(() => { "unable to printf aggregate argument " + arg }, arg.line)
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

  // returns the pin connected to the reset signal, creates a new one if
  // no such pin exists
  def addResetPin(reset: Bool): Bool = {
    def makeIO = {
      val res = Bool(INPUT)
      res.isIo = true
      res.component = this
      res
    }
    def pin =
      if (reset == _reset) this.reset
      else makeIO
    this.resets.getOrElseUpdate(reset, pin)
  }

  def addClock(clock: Clock) {
    if (!this.clocks.contains(clock))
      this.clocks += clock
  }

  // COMPILATION OF BODY
  def initializeBFS: ScalaQueue[Node] = {
    val res = new ScalaQueue[Node]

    for (c <- Driver.components; a <- c.debugs)
      res.enqueue(a)
    for(b <- Driver.blackboxes)
      res.enqueue(b.io)
    for(c <- Driver.components)
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

  def dfs(visit: Node => Unit): Unit = {
    val walked = new HashSet[Node]
    val dfsStack = initializeDFS

    def isVisiting(node: Node) =
      !(node == null) && !(walked contains node) && 
      (node.component == this || node.isIo)

    while(!dfsStack.isEmpty) {
      val top = dfsStack.pop
      walked += top
      visit(top)
      for(i <- top.inputs) {
        if (isVisiting(i)) {
          dfsStack push i
          walked += i
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

  def lowerNodes(needsLowering: Set[String]): Unit = if (!needsLowering.isEmpty) {
    val lowerTo = new HashMap[Node, Node]
    bfs { x =>
      for (i <- 0 until x.inputs.length) x.inputs(i) match {
        case op: Op =>
          if (needsLowering contains op.op)
            x.inputs(i) = lowerTo.getOrElseUpdate(op, op.lower)
        case _ =>
      }
    }
    if (!lowerTo.isEmpty)
      inferAll
  }

  def forceMatchingWidths {
    bfs(_.forceMatchingWidths)
  }

  def addDefaultReset {
    if (!(defaultResetPin == null)) {
      addResetPin(_reset)
      if (this != Driver.topComponent && hasExplicitReset)
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
    bfs { _ match {
        case x: Delay =>
          val clock = if (x.clock == null) x.component.clock else x.clock
          val clockingSource = clock.getClockingSource()
          val reset =
            /*if (x.component.hasExplicitReset) x.component._reset
            else if (x.clock != null) x.clock.getReset
            else if (x.component.hasExplicitClock) x.component.clock.getReset
            else x.component._reset*/
            if (x.component.hasExplicitReset) x.component._reset //legacy hasExplicitReset ?
            else clock.getReset
            
          x.assignReset(x.component.addResetPin(reset))
          x.assignClock(clockingSource)
          x.component.addClock(clockingSource)
        case _ =>
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
    for (c <- Driver.components) {
      roots ++= c.debugs
      if (c.parent == null) {
        val topIOs = c.io.flatten;
        for ((name, wire) <- topIOs) {
          roots += wire
        }
      }
    }
    for (b <- Driver.blackboxes)
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
    val imods = mods.filter(!_.isInstanceOf[Literal])
    val mhist = new HashMap[String, Int]
    val whist = new HashMap[Int, Int]
    val hist = new HashMap[String, Int]
    for (m <- imods) {
      mhist(m.component.toString) = 1 + mhist.getOrElse(m.component.toString, 0)
      whist(m.width) = 1 + whist.getOrElse(m.width, 0)
      val name = m match {
        case op: Op => op.op
        case o      => {
          val name = m.getClass.getName
          name.substring(name.indexOf('.') + 1)
        }
      }
      hist(name) = 1 + hist.getOrElse(name, 0)
    }
    ChiselError.info("%60s %7s".format("module", "node count"));
    for (n <- mhist.keys.toList.sortWith((a, b) => mhist(a) > mhist(b)))
      ChiselError.info("%60s %7d".format(n, mhist(n)))
    ChiselError.info("%12s %7s".format("name", "count"));
    for (n <- hist.keys.toList.sortWith((a, b) => hist(a) > hist(b)))
      ChiselError.info("%12s %7d".format(n, hist(n)))
    ChiselError.info("%5s %s".format("width", "count"));
    for (w <- whist.keys.toList.sortWith((a, b) => a < b))
      ChiselError.info("%5d %7d".format(w, whist(w)))
    val maxDepth = imods.map(_.depth).foldLeft(0)(_ max _)
    val widths = new Array[Int](maxDepth + 1)
    for (m <- imods)
      widths(m.depth) += 1
    val maxWidth = widths.foldLeft(0)(_ max _)
    (imods.length, maxWidth, maxDepth)
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

  def getClassValNames(c: Class[_]): ArrayBuffer[String] = {
    val valnames = new ArrayBuffer[String]()
    for (v <- c.getDeclaredFields) {
      v.setAccessible(true)
      valnames += v.getName
    }
    val sc = c.getSuperclass
    if (sc != null) { valnames ++= getClassValNames(sc) }
    valnames
  }

  // Allow checking if a method name is also the name of a val -- reveals accessors
  def getValNames = {
    val valnames = new ArrayBuffer[String]()
    valnames ++= getClassValNames(getClass)    
    valnames
  }

  object isValName {
    val valnames = Module.this.getValNames
    def apply(name: String) = valnames.contains(name)
  }

  // 1) name the component
  // 2) name the IO
  // 3) name and set the component of all statically declared nodes through introspection
  // 4) set variable names
  def markComponent() {
    ownIo();
    io setPseudoName ("io", true)
    /* We are going through all declarations, which can return Nodes,
     ArrayBuffer[Node], BlackBox and Modules.
     Since we call invoke() to get a proper instance of the correct type,
     we have to insure the method is accessible, thus all fields
     that will generate C++ or Verilog code must be made public. */
     for (m <- getClass().getDeclaredMethods.sortWith(
      (x, y) => (x.getName() < y.getName())
    )) {
       val name = m.getName();
       val types = m.getParameterTypes();
       if (types.length == 0 && isValName(name) // patch to avoid defs
        && isPublic(m.getModifiers())) {
         val o = m.invoke(this);
         o match {
         case node: Node => {
           node setPseudoName (name, false)
         }
         case buf: ArrayBuffer[_] => {
           /* We would prefer to match for ArrayBuffer[Node] but that's
            impossible because of JVM constraints which lead to type erasure.
            XXX Using Seq instead of ArrayBuffer will pick up members defined
            in Module that are solely there for implementation purposes. */
           if(!buf.isEmpty && buf.head.isInstanceOf[Node]){
             val nodebuf = buf.asInstanceOf[Seq[Node]];
             for((elm, i) <- nodebuf.zipWithIndex){
               elm setPseudoName (name + "_" + i, false)
             }
           }
         }
         case buf: collection.IndexedSeq[_] => {
           if(!buf.isEmpty && buf.head.isInstanceOf[Node]){
             val nodebuf = buf.asInstanceOf[Seq[Node]];
             for((elm, i) <- nodebuf.zipWithIndex){
               elm setPseudoName (name + "_" + i, false)
             }
           }
         }
         case comp: Module => {
           comp.pathParent = this;
           comp.pName = name
         }
         case any =>
       }
     }
     }
  }


  def genAllMuxes {
    bfs { _ match {
        case p: proc => p.verifyMuxes
        case _ =>
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
    if (!Driver.backend.isInstanceOf[VerilogBackend]) {
      queue.push(() => io.traceNode(this, queue));
    } else {
      for (c <- Driver.components) {
        queue.push(() => c.io.traceNode(c, queue))
      }
    }
    for (c <- Driver.components) {
        if (!(c.defaultResetPin == null)) { // must manually add reset pin cuz it isn't part of io
          queue.push(() => c.defaultResetPin.traceNode(c, queue))
        }
    }
    for (c <- Driver.components; d <- c.debugs)
      queue.push(() => d.traceNode(c, queue))
    for (b <- Driver.blackboxes)
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
          ChiselError.error("  (" + ind +  ")", node.line)
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

  override val hashCode: Int = Driver.components.size
  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
}

