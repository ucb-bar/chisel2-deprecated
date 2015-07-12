/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
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
import scala.collection.mutable.{ArrayBuffer, LinkedHashSet, HashSet, HashMap, Stack, Queue=>ScalaQueue}

object Module {
  def topMod = Driver.topComponent getOrElse (throwException("no top component"))

  def apply[T <: Module](m: => T)(implicit p: Parameters = params): T = {
    Driver.modStackPushed = true
    Driver.parStack.push(p.push)
    val res = init(m)
    Driver.parStack.pop
    res
  }
  def apply[T <: Module](m: => T, f: PartialFunction[Any,Any]): T = {
    val q = params.alterPartial(f)
    apply(m)(q)
  }

  private def init[T<:Module](c: =>T):T = {
    val res = c
    pop()
    for ((n, io) <- res.wires) {
      if (io.dir == NODIR) {
        val io_str = "<" + n + " (" + io.getClass.getName + ")>"
        ChiselError.error(new ChiselError(() => {
           "All IO's must be ports (dir set): " + io_str + " in " + res }, io.line))
      }
      // else if (! io.isKnownWidth)
      //   ChiselError.error(new ChiselError(() => {"All IO's must have width set: " + io}, io.line))
      io.isIo = true
    }
    res
  }
  private def params = if(Driver.parStack.isEmpty) Parameters.empty else Driver.parStack.top

    /* *push* is done in the Module constructor because we don't have
     a *this* pointer before then, yet we need to store it before the subclass
     constructors are built. */

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
    if (Driver.compStack.isEmpty) {
      ChiselError.error("Empty driver component stack. Do you have a bare module wrapping a bare module?")
    } else {
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
  }

  // XXX Remove and instead call current()
  def getComponent: Module = if (Driver.compStack.length != 0) Driver.compStack.top else null
  def current = {
    val comp = getComponent
    if (comp == null) topMod else comp
  }

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
         ( + ) overridden if Delay specifies its own clock
   ( + ) reset parameter
         ( + ) sets the default reset signal
         ( + ) overridden if Delay specifies its own clock w/ reset != implicitReset
*/
abstract class Module(var clock: Option[Clock] = None, private[Chisel] var _reset: Option[Bool] = None) {
  /** A backend(Backend.scala) might generate multiple module source code
    from one Module, based on the parameters to instantiate the component
    instance. Since we do not want to blindly generate one module per instance
    the backend will keep a cache of each module's implementation source code
    and discard textual duplicates. By walking the nodes from level zero
    (leafs) to level N (root), we are guaranteed to generate all
    Module/modules source text before their first instantiation. */
  private[Chisel] var level = 0
  private[Chisel] var traversal = 0
  /** Name of the instance. */
  var name: String = ""
  /** Name of the module this component generates (defaults to class name). */
  var moduleName: String = ""
  var named = false
  var parent: Module = null
  val children = ArrayBuffer[Module]()

  private[Chisel] val bindings = ArrayBuffer[Binding]()
  private[Chisel] val printfs = ArrayBuffer[Printf]()
  private[Chisel] val asserts = ArrayBuffer[Assert]()
  private[Chisel] val debugs = LinkedHashSet[Node]()
  private[Chisel] val nodes = LinkedHashSet[Node]()
  private[Chisel] val names = HashMap[String, Node]()

  private lazy val trueCond = Bool(true)
  private[Chisel] val switchKeys = Stack[Bits]()
  private[Chisel] val whenConds = Stack[Bool]()
  private[Chisel] def hasWhenCond: Boolean = !whenConds.isEmpty
  private[Chisel] def whenCond: Bool = if (hasWhenCond) whenConds.top else trueCond

  //Parameter Stuff
  lazy val params = Module.params
  params.path = this.getClass :: params.path

  if (clock == null) clock = None
  if (_reset == null) _reset = None
  Driver.components += this
  Module.push(this)

  var verilog_parameters = "";

  private[Chisel] val clocks = ArrayBuffer[Clock]()
  private[Chisel] val resets = HashMap[Bool, Bool]()
  private[Chisel] var resetPin: Option[Bool] = None
  private[Chisel] def hasExplicitClock = (this eq Module.topMod) || (clock match {
    case None => false
    case Some(c) => c ne Driver.implicitClock 
  })
  private[Chisel] def hasExplicitReset = (this eq Module.topMod) || (_reset match {
    case None => false
    case Some(r) => r ne Driver.implicitReset
  })
  def reset = resetPin match {
    case None => {
      val r = Bool(INPUT)
      r.isIo = true
      r.component = this
      r setName ("reset")
      resetPin = Some(r)
      r
    }
    case Some(r) => r
  }
  def reset_=(r: Bool) {
    _reset = Some(r)
  }
  // returns the pin connected to the reset signal, creates a new one if
  // no such pin exists
  def addResetPin(r: Bool): Bool = {
    val pin = _reset match {
      case Some(p) if p == r => reset
      case _ => 
        val p = Bool(INPUT)
        p.isIo = true
        p.component = this
        p
    }
    resets getOrElseUpdate (r, pin)
  }
  def addClock(clock: Clock) { if (!(clocks contains clock)) clocks += clock }

  override def toString = s"<${this.name} (${this.getClass.toString})>"

  def findBinding(m: Node) = bindings find (_.inputs(0) == m)

  def io: Data

  var nindex: Option[Int] = None
  def nextIndex : Int = {
    nindex = nindex match { case None => Some(0) case Some(i) => Some(i+1) }
    nindex.get
  }

  // override def toString: String = name this one isn't really working...
  def wires: Array[(String, Bits)] = io.flatten

  /** Add an assertion in the code generated by a backend. */
  def assert(cond: Bool, message: String): Unit = {
    val a = new Assert(!Module.current.whenCond || cond, reset, message)
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

  def printf(message: String, args: Node*): Unit = {
    val p = new Printf(Module.current.whenCond && !reset, message, args)
    printfs += p
    debug(p)
    p.inputs.foreach(debug _)
    args foreach {
      case arg: Aggregate =>
        ChiselError.error(new ChiselError(() => { "unable to printf aggregate argument " + arg }, arg.line))
      case _ =>
    }
  }

  def <>(src: Module) { io <> src.io }

  def apply(name: String): Data = io(name)
  // COMPILATION OF REFERENCE
  def emitDec(b: Backend): String = {
    var res = ""
    // val wires = io.flatten;
    for ((n, w) <- wires)
      res += b.emitDec(w)
    res
  }

  def addPin[T <: Data](pin: T, name: String = "") = {
    val gen = pin.clone
    io match {
      case b: Bundle => {
        for ((n, io) <- gen.flatten) {
          io.component = this
          io.isIo = true
        }
        if (name != "") gen nameIt (name, true)
        b.elements += ((gen.name, gen))
      }
      case _ => // Is it possible?
    }
    gen
  }

  def addModule[T<:Module](c: =>T, f: PartialFunction[Any,Any]) = {
    Driver.modStackPushed = true
    Driver.modAdded = true
    val q = params.alterPartial(f)
    Driver.compStack.push(this)
    Driver.parStack.push(q)
    val res = Module.init(c)
    Driver.parStack.pop
    Driver.compStack.pop
    res
  }
  def addModule[T <: Module](c: => T)(implicit p:Parameters = params) = {
    Driver.modStackPushed = true
    Driver.modAdded = true
    Driver.compStack.push(this)
    Driver.parStack.push(p.push)
    val res = Module.init(c)
    res.markComponent
    Driver.parStack.pop
    Driver.compStack.pop
    res
  }

  def bfs (visit: Node => Unit) = {
    // initialize BFS
    val queue = new ScalaQueue[Node]

    for (a <- debugs)
      queue enqueue a
    for ((n, io) <- wires ; if io.isIo && io.dir == OUTPUT)
      queue enqueue io
    for (child <- children ; (n, io) <- child.wires ; if io.isIo && io.dir == INPUT)
      queue enqueue io
    for (pin <- resets.values)
      queue enqueue pin

    // Do BFS
    val walked = HashSet[Node]()
    while (!queue.isEmpty) {
      val top = queue.dequeue
      walked += top
      visit(top)
      top match {
        case v: Vec[_] =>
          for ((n, e) <- v.flatten;
          if !(e == null) && !(walked contains e) && !e.isIo) {
            queue enqueue e
            walked += e
          }
          for (i <- top.inputs;
          if !(i == null) && !(walked contains i) && !i.isIo) {
            queue enqueue i
            walked += i
          }
        case _ => {
          for (i <- top.inputs;
          if !(i == null) && !(walked contains i) && !i.isIo) {
            queue enqueue i
            walked += i
          }
        }
      }
    }
  }

  def dfs(visit: Node => Unit): Unit = {
    val stack = new Stack[Node]
    // initialize DFS
    for ((n, io) <- wires ; if io.isIo && io.dir == OUTPUT)
      stack push io
    for (child <- children ; (n, io) <- child.wires ; if io.isIo && io.dir == INPUT)
      stack push io
    for (pin <- resets.values)
      stack push pin
    for (a <- debugs)
      stack push a

    // Do DFS
    val walked = HashSet[Node]()
    while (!stack.isEmpty) {
      val top = stack.pop
      walked += top
      visit(top)
      top match {
        case v: Vec[_] => {
          for ((n, e) <- v.flatten;
          if !(e == null) && !(walked contains e) && !e.isIo) {
            stack push e
            walked += e
          }
          for (i <- top.inputs;
          if !(i == null) && !(walked contains i) && !i.isIo) {
            stack push i
            walked += i
          }
        }
        case _ => {
          for (i <- top.inputs;
          if !(i == null) && !(walked contains i) && !i.isIo) {
            stack push i
            walked += i
          }
        }
      }
    }
  }

  def addDefaultReset {
    _reset match {
      case None => throw new RuntimeException
      case Some(r) => resetPin match {
        case None => 
        case Some(p) =>
          addResetPin(r)
          if ((this ne Module.topMod) && hasExplicitReset) p := r
      }
    }
  }

  // This function sets the IO's component.
  private def ownIo() {
    for ((n, w) <- wires ; if this != w.component) {
      // This assert is a sanity check to make sure static resolution
      // of IOs didn't fail
      val io_str = "<" + n + " (" + w.getClass.getName + ")>"
      ChiselError.error("Statically resolved component(" + this + 
        ") differs from dynamically resolved component(" + w.component + 
        ") of IO: " + io_str + " crashing compiler")
    }
    // io naming
    io nameIt ("io", true)
  }

  private def getClassValNames(c: Class[_]): ArrayBuffer[String] = {
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
  private def getValNames = {
    val valnames = new ArrayBuffer[String]()
    valnames ++= getClassValNames(getClass)
    valnames
  }

  private object isValName {
    val valnames = Module.this.getValNames
    def apply(name: String) = valnames.contains(name)
  }

  // 1) name the component
  // 2) name the IO
  // 3) name and set the component of all statically declared nodes through introspection
  // 4) set variable names
  private[Chisel] def markComponent {
    import Driver.backend
    ownIo()
    /* We are going through all declarations, which can return Nodes,
     ArrayBuffer[Node], BlackBox and Modules.
     Since we call invoke() to get a proper instance of the correct type,
     we have to insure the method is accessible, thus all fields
     that will generate C++ or Verilog code must be made public. */
     // get all super classes' methods
     def getMethods(c: Class[_]): Set[java.lang.reflect.Method] = {
       if (c.toString.split('.').last == "Module") Set[java.lang.reflect.Method]() 
       else c.getDeclaredMethods.toSet ++ getMethods(c.getSuperclass)
     }
     for (m <- getMethods(getClass).toList.sortWith(_.getName < _.getName)) {
       val name = m.getName();
       val types = m.getParameterTypes();
       if (types.length == 0 && isValName(name) // patch to avoid defs
        && java.lang.reflect.Modifier.isPublic(m.getModifiers())) {
         val o = m.invoke(this);
         o match {
         case node: Node => {
           node.getNode match {
             case _: Literal =>
             case _ => node.getNode nameIt (backend.asValidName(name), false)
           }
           backend.nameSpace += node.getNode.name
         }
         case buf: ArrayBuffer[_] => {
           /* We would prefer to match for ArrayBuffer[Node] but that's
            impossible because of JVM constraints which lead to type erasure.
            XXX Using Seq instead of ArrayBuffer will pick up members defined
            in Module that are solely there for implementation purposes. */
           if(!buf.isEmpty && buf.head.isInstanceOf[Node]){
             val nodebuf = buf.asInstanceOf[Seq[Node]];
             for((elm, i) <- nodebuf.zipWithIndex){
               elm.getNode match {
                 case _: Literal =>
                 case _ => elm.getNode nameIt (backend.asValidName(name + "_" + i), false)
               }
               backend.nameSpace += elm.getNode.name
             }
           }
         }
         case buf: collection.IndexedSeq[_] => {
           if(!buf.isEmpty && buf.head.isInstanceOf[Node]){
             val nodebuf = buf.asInstanceOf[Seq[Node]];
             for((elm, i) <- nodebuf.zipWithIndex){
               elm.getNode match {
                 case _: Literal =>
                 case _ =>
                   elm.getNode nameIt (backend.asValidName(name + "_" + i), false)
               }
               backend.nameSpace += elm.getNode.name
             }
           }
         }
         case bb: BlackBox => {
           if (!bb.named) {
             bb.name = name
             bb.named = true
           }
           backend.nameSpace += bb.name
         }
         case comp: Module => {
           if (!comp.named) {
             comp.name = backend.asValidName(name)
             comp.named = true
           }
           backend.nameSpace += comp.name
         }
         case any =>
       }
     }
     }
  }

  /* XXX Not sure what the two following do.
   They never get overridden yet it is called
   for each component (See Backend implementations). */
  def stripComponent(s: String): String = s.split("__").last
  /** Returns the absolute path to a component instance from toplevel. */
  def getPathName: String = getPathName()
  def getPathName(separator: String = "_"): String = {
    if ( parent == null ) name else parent.getPathName(separator) + separator + name;
  }

  def isInput(node: Node): Boolean =
    node match { case b: Bits => b.dir == INPUT case o => false }
  def keepInputs(nodes: Seq[Node]): Seq[Node] = nodes.filter(isInput)
  def removeInputs(nodes: Seq[Node]): Seq[Node] = nodes.filter(n => !isInput(n))

  override val hashCode: Int = Driver.components.size
  override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
  // Chisel3
  private[Chisel] val assignments = HashMap[Data, StackTraceElement]()
  private[Chisel] def addAssignment(assignee: Data) = {
    val stack = Thread.currentThread().getStackTrace
    if (!assignments.contains(assignee)) {
      assignments += ((assignee, ChiselError.findFirstUserLine(stack) getOrElse stack(0)))
    }
  }
  
  /** verifyWireWrap (Chisel3) - verify assignment semantics (type-only nodes must be wire-wrapped)
    *  @return - HashMap of source lines (and associated nodes) requiring Wire() wrapping.
    */
  type neededWireWraps = HashMap[StackTraceElement, ArrayBuffer[Data]]
  private[Chisel] def verifyWireWrap: neededWireWraps = {
    // Go through all assignments for this module and add those needing Wire()-wrap to a map.
    val wireWrapLineToNode = new neededWireWraps()
    def nodeNeedsWire(node: Data, errorLine: StackTraceElement) {
      // Add this node to the list of nodes for this line
      if (!wireWrapLineToNode.contains(errorLine)) {
        wireWrapLineToNode(errorLine) = ArrayBuffer[Data]()
      }
      wireWrapLineToNode(errorLine) += node
    }
    for ((node, assignmentLine) <- assignments) {
      // Is the node type-only (no data) and isn't io and hasn't been Wire() wrapped?
      if (node.isTypeOnly && !(node.isWired || node.isIo)) {
        // Do we have line numbers?
        val errorLine = if (node.line != null) {
          node.line
        } else {
          assignmentLine
        }
        nodeNeedsWire(node, errorLine)
      }
    }
    wireWrapLineToNode
  }
  
  /** reportWireWrap (Chisel3) - report type-only nodes requiring Wire() wrapping.
    *  @param - HashMap of source lines (and associated nodes) requiring Wire() wrapping.
    */
  private[Chisel] def reportWireWrap(lineNodes: neededWireWraps) {
    // For extra credit, sort the map keys (file and line number)
    for ((errorLine, nodes) <- lineNodes) {
      val nodeNames = nodes.map(_.name).mkString(", ")
      val plural = if (nodes.length > 1) "s" else ""
      val errorString = "Chisel3 compatibility: node%s %s should be wrapped in a Wire()".format(plural, nodeNames)
      ChiselError.warning(errorString, errorLine)
    }
  }

  /** verify module.
    *  @return - true means there are no issues with the module, false means there are issues and they have been reported.
    */
  private[Chisel] def verify: Boolean = {
    var result = true
    // If we're verifying Chisel3 compatibility, verify Wire() wrapping.
    if (Driver.minimumCompatibility > "2") {
      val neededWireWraps = verifyWireWrap
      if (!neededWireWraps.isEmpty) {
        reportWireWrap(neededWireWraps)
        result = false
      }
    }
    result
  }
}
