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
      if (io.dir == null)
         ChiselErrors += new ChiselError(() => {"All IO's must be ports (dir set): " + io}, io.line)
      // else if (! io.isKnownWidth)
      //   ChiselErrors += new ChiselError(() => {"All IO's must have width set: " + io}, io.line)
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
  def current: Module = {
    val comp = getComponent
    if (comp == null) Driver.topComponent else comp
  }

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
         ( + ) overridden if Delay specifies its own clock
   ( + ) reset parameter
         ( + ) sets the default reset signal
         ( + ) overridden if Delay specifies its own clock w/ reset != implicitReset
*/
abstract class Module(var clock: Clock = null, private[Chisel] var _reset: Bool = null) {
  /** A backend(Backend.scala) might generate multiple module source code
    from one Module, based on the parameters to instantiate the component
    instance. Since we do not want to blindly generate one module per instance
    the backend will keep a cache of each module's implementation source code
    and discard textual duplicates. By walking the nodes from level zero
    (leafs) to level N (root), we are guaranteed to generate all
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

  val nodes = new LinkedHashSet[Node]
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

  //Parameter Stuff
  lazy val params = Module.params
  params.path = this.getClass :: params.path

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
    // io naming
    io nameIt ("io", true)
  }

  def findBinding(m: Node) = bindings find (_.inputs(0) == m)

  def io: Data

  def nextIndex : Int = { nindex = nindex + 1; nindex }

  // override def toString: String = name this one isn't really working...
  def wires: Array[(String, Bits)] = io.flatten

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
    // val wires = io.flatten;
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

  def addPin[T <: Data](pin: T, name: String = "") = {
    io match {
      case b: Bundle => {
        for ((n, io) <- pin.flatten) {
          io.component = this
          io.isIo = true
        }
        if (name != "") pin nameIt (name, true)
        b.elements += ((pin.name, pin))
      }
      case _ => // Is it possible?
    }
    pin
  }

  def addModule[T<:Module](c: =>T, f: PartialFunction[Any,Any]) = {
    Driver.modStackPushed = true
    Driver.modAdded = true
    val q = params.alterPartial(f)
    Driver.compStack.push(this)
    Driver.parStack.push(q)
    val res = init(c)
    Driver.parStack.pop
    Driver.compStack.pop
    res
  }
  def addModule[T <: Module](c: => T)(implicit p:Parameters = params) = {
    Driver.modStackPushed = true
    Driver.modAdded = true
    Driver.compStack.push(this)
    Driver.parStack.push(p.push)
    val res = init(c)
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
    if (!(defaultResetPin == null))
      queue enqueue defaultResetPin

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
    if (!(defaultResetPin == null))
      stack push defaultResetPin
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
    if (!(defaultResetPin == null)) {
      addResetPin(_reset)
      if (this != Driver.topComponent && hasExplicitReset)
        defaultResetPin.inputs += _reset
    }
  }

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
                 case _ => elm.getNode nameIt (backend.asValidName(name + "_" + i), false)
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
           comp.pathParent = this;
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
  def getPathName: String = {
    getPathName()
  }
  def getPathName(separator: String = "_"): String = {
    if ( parent == null ) name else parent.getPathName(separator) + separator + name;
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

