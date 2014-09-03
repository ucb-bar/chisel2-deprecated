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

import scala.collection.immutable.Vector
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import java.io.PrintStream

import Node._;
import ChiselError._;
import Width._;

import java.lang.Double.longBitsToDouble
import java.lang.Float.intBitsToFloat

import java.lang.Thread._

object Node {
  def sprintf(message: String, args: Node*): Bits = {
    val s = Bits().fromNode(new Sprintf(message, args))
    s.setIsTypeNode
    s
  }

  def fixWidth(w: => Int): (=> Node) => Width = { (n) => {
    if (n != null) {
      assert(w != -1, ChiselError.error("invalid width for fixWidth object"));
      Width(w)
    } else {
      Width()
    }
  }}

  def widthOf(i: => Int): (=> Node) => Width = { (m) => {
    val debug = false
    try {
      if (debug && i >= m.inputs.length ) {
        println("Bad i (" + i + " >= " + m.inputs.length + ")")
      }
      m.inputs(i).width
    } catch {
        case e: java.lang.IndexOutOfBoundsException => {
          val error = new ChiselError(() => {m + " is unconnected ("+ i + " of " + m.inputs.length + "). Ensure that it is assigned."}, m.line)
	  if (debug) {
            e.printStackTrace(System.err)
            var errorChild = ""
            try {
              if (m.inputs.length > i) {
                errorChild = m.inputs(i).toString
              } else {
                errorChild = m.inputs.length.toString + " <= " + i 
              }
            } catch {
              case ec: Throwable => {
                ec.printStackTrace(System.err)
              }
            } finally {
              println(error + " - " + errorChild)
            }
          }
          if (!ChiselErrors.contains(error) && !Driver.isInGetWidth) {
            ChiselErrors += error
          }
          Width()
        }
    }
  }}

  // Compute the maximum width required for a node,
  def maxWidth(m: => Node): Width = {
    var w = Width(0)
    for (i <- m.inputs)
      if (!(i == null || i == m)) {
        w = w.max(i.width)
      }
    w
  }

  def minWidth(m: => Node): Width = m.inputs.map(_.width).min

  def maxWidthPlusOne(m: => Node): Width = maxWidth(m) + 1

  def sumWidth(m: => Node): Width = {
    var res = Width(0);
    for (i <- m.inputs)
      res = res + i.width;
    res
  }

  def lshWidthOf(i: => Int, n: => Node): (=> Node) => (Width) = {
    (m) => {
      // n.width
      if (!n.isLit && !n.isKnownWidth) Width()
      else m.inputs(0).width + n.litValue((1 << n.needWidth)-1).toInt
    }
  }

  def rshWidthOf(i: => Int, n: => Node): (=> Node) => (Width) = {
    (m) => {
      val a = m.inputs(i).width
      val b = n.litValue(0).toInt
      val w = a - b
      val extra = ": " + m.inputs(i).toString
      println("rshWidthOf: " + a + " - " + b + " = " + w + "(" + i + ")" + extra)
      w
    }
  }
}

/** *Node* defines the root class of the class hierarchy for
  a [Composite Pattern](http://en.wikipedia.org/wiki/Composite_pattern).

  A digital logic graph is encoded as adjacency graph where instances
  of *Node* describe vertices and *inputs*, *consumers* member fields
  are used to traverse the directed graph respectively backward (from
  output to input) and forward (from input to output).
  */
abstract class Node extends nameable {
  var sccIndex = -1
  var sccLowlink = -1
  /* Assigned in Binding and Mod.reset */
  var component: Module = Module.getComponent
  var isTypeNode = false;
  var depth = 0;
  def componentOf: Module = if (Driver.backend.isEmittingComponents && component != null) component else Driver.topComponent
  // The semantics of width are sufficiently complicated that
  // it deserves its own class
  var width_ = Width()
  val consumers = new ArrayBuffer[Node]; // mods that consume one of my outputs
  val inputs = new ArrayBuffer[Node];
  def traceableNodes: Array[Node] = Array[Node]();
  var inferWidth: (=> Node) => Width = maxWidth

  var nameHolder: nameable = null;
  val line: StackTraceElement =
    if (Driver.getLineNumbers) {
      val trace = new Throwable().getStackTrace
      findFirstUserLine(trace) getOrElse trace(0)
    } else null
  var prune = false
  var driveRand = false
  var clock: Clock = null
  var CppVertex: CppVertex = null
  var counter: Bits = null
  var shadow: Bits = null
  var cntrIdx = -1

  val _id = Driver.nodes.length
  Driver.nodes += this

  def isByValue: Boolean = true;
  def width: Width = {
    if (Driver.isInGetWidth) inferWidth(this) else width_
  }

  /** Sets the width of a Node. */
  def width_=(w: Int) {
    width_.setWidth(w);
    inferWidth = fixWidth(w);
  }

  def width_=(w: Width) {
    width_ = w;
    // NOTE: This explicitly does not set inferWidth.
    // See the comments in infer
  }

  def nameIt (path: String, isNamingIo: Boolean) {
    try {
      if (!named && (!isIo || isNamingIo)) {
        /* If the name was set explicitely through *setName*,
         we don't override it. */
        name = path;
      }
      while (component.names.getOrElseUpdate(name, this) ne this)
        name += "_"
    } catch {
      case e:NullPointerException => {
        println("Node:nameIt() NullPointerException: name '" + name + "'")
        printTree(System.out, 1)
        throwException("NullPointer " + this.getClass + ":name " + name, e)
      }
    }
  }

  def setPseudoName(path: String, isNamingIo: Boolean) {
    if (!isIo || (isIo && isNamingIo))
      pName = path
  }

  lazy val chiselName = this match {
    case l: Literal => "";
    case any        =>
      if (name != "" && (name != "reset") && !(component == null)) 
        component.getPathName(".") + "." + name
      else
        ""
  }

  // TODO: REMOVE WHEN LOWEST DATA TYPE IS BITS
  def ##(b: Node): Node  = Op("##", sumWidth _,  this, b );
  final def isLit: Boolean = litOf ne null
  def litOf: Literal = if (getNode != this) getNode.litOf else null
  def litValue(default: BigInt = BigInt(-1)): BigInt =
    if (isLit) litOf.value
    else default
  def floLitValue: Float = intBitsToFloat(litValue().toInt)
  def dblLitValue: Double = longBitsToDouble(litValue().toLong)
  // TODO: MOVE TO WIRE
  def assign(src: Node): Unit = throw new Exception("unimplemented assign")
  def <>(src: Node): Unit = throw new Exception("unimplemented <>")
  def ^^(src: Node): Unit = src <> this

  def getLit: Literal = this.asInstanceOf[Literal]
  private var _isIo = false
  def isIo = _isIo
  def isIo_=(isIo: Boolean) = _isIo = isIo
  def isReg: Boolean = false
  def isUsedByClockHi: Boolean = consumers.exists(_.usesInClockHi(this))
  def usesInClockHi(i: Node): Boolean = false
  def initOf (n: String, widthfunc: (=> Node) => Width, ins: Iterable[Node]): Node = {
    name = n;
    inferWidth = widthfunc;
    inputs ++= ins
    this
  }
  def init (n: String, widthFunc: (=> Node) => Width, ins: Node*): Node = {
    initOf(n, widthFunc, ins.toList);
  }
  def init (n: String, w: Int, ins: Node*): Node = {
    width_ = Width(w)
    initOf(n, fixWidth(w), ins.toList)
  }
  
  // Called while we're walking the graph inferring the width of nodes.
  // We return true if we should continue to walk the graph,
  // either because there's a node whose width we don't know,
  // or because we updated a node's width.
  def infer: Boolean = {
    val res = inferWidth(this);
    if (! res.isKnown) {
      true
    } else if (res != width) {
      // NOTE: This should NOT stop us using inferWidth, since the value
      // we set here may not be correct.
      width_ = res
      true
    } else {
      false
    }
  }
  lazy val isInObject: Boolean =
    (isIo && (Driver.isIoDebug || component == Driver.topComponent)) ||
    Driver.topComponent.debugs.contains(this) ||
    isReg || isUsedByClockHi || Driver.isDebug && !name.isEmpty ||
    Driver.emitTempNodes

  lazy val isInVCD: Boolean = name != "reset" && needWidth() > 0 &&
     (!name.isEmpty || Driver.emitTempNodes) &&
     ((isIo && isInObject) || isReg || Driver.isDebug)

  /** Prints all members of a node and recursively its inputs up to a certain
    depth level. This method is purely used for debugging. */
  def printTree(writer: PrintStream, depth: Int = 4, indent: String = ""): Unit = {
    if (depth < 1) return;
    writer.println(indent + getClass + " width=" + getWidth + " #inputs=" + inputs.length);
    this match {
      case fix: SInt => {
        if (!(fix.comp == null)) {
          writer.println(indent + "  (has comp " + fix.comp + " of type " + fix.comp.getClass + ")");
        }
      }
      case bits: UInt => {
        if (!(bits.comp == null)) {
          writer.println(indent + "(has comp " + bits.comp + ")");
        }
      }
      case any => writer.println(indent + this)
    }
    writer.println("sccIndex: " + sccIndex)
    writer.println("sccLowlink: " + sccLowlink)
    writer.println("component: " + component)
    writer.println("isTypeNode: " + isTypeNode)
    writer.println("depth: " + depth)
    writer.println("width: " + width_)
    writer.println("index: " + emitIndex)
    writer.println("consumers.length: " + consumers.length)
    writer.println("nameHolder: " + nameHolder)
    writer.println("line: " + line)
    for (in <- inputs) {
      if (in == null) {
        writer.println("null");
      } else {
        in.printTree(writer, depth-1, indent + "  ");
      }
    }
  }

  def traceNode(c: Module, stack: Stack[() => Any]): Any = {
    // pushes and pops components as necessary in order to later mark the parent of nodes
    val (comp, nextComp) =
      this match {
        case io: Bits => {
          if(io.isIo && (io.dir == INPUT || io.dir == OUTPUT)) {
            (io.component, if (io.dir == OUTPUT) io.component else io.component.parent)
          } else {
            (c, c)
          }
        }
        case any    => (c, c);
      }

    assert( comp != null );
    if (comp != null && !comp.isWalked.contains(this)) {
      comp.isWalked += this;
      for (node <- traceableNodes) {
        if (node != null) {
          stack.push(() => node.traceNode(nextComp, stack));
        }
      }
      var i = 0;
      for (node <- inputs) {
        if (node != null) {
           //tmp fix, what happens if multiple componenets reference static nodes?
          if (node.component == null || !Driver.components.contains(node.component)) {
            /* If Backend.collectNodesIntoComp does not resolve the component
             field for all components, we will most likely end-up here. */
            assert( node.component == nextComp,
              ChiselError.error((if(node.name != null && !node.name.isEmpty)
                node.name else "?")
                + "[" + node.getClass.getName
                + "] has no match between component "
                + (if( node.component == null ) "(null)" else node.component)
                + " and '" + nextComp + "' input of "
                + (if(this.name != null && !this.name.isEmpty)
                this.name else "?")))
          }
          if (!Driver.backend.isInstanceOf[VerilogBackend] || !node.isIo) {
            stack.push(() => node.traceNode(nextComp, stack));
          }
          val j = i;
          val n = node;
          stack.push(() => {
            /* This code finds an output binding for a node.
             We search for a binding only if the io is an output
             and the logic's grandfather component is not the same
             as the io's component and the logic's component is not
             same as output's component unless the logic is an input */
            n match {
              case io: Bits =>
                if (io.isIo && io.dir == OUTPUT && !io.isTypeNode &&
                    (!(component.parent == io.component) &&
                     !(component == io.component &&
                       !(this.isInstanceOf[Bits]
                         && this.asInstanceOf[Bits].dir == INPUT)))) {
                  val c = n.component.parent;
                  val b = Binding(n, c, io.component);
                  inputs(j) = b;
                  if (!c.isWalked.contains(b)) {
                    c.mods += b;  c.isWalked += b;
                  }
                  // In this case, we are trying to use the input of a submodule
                  // as part of the logic outside of the submodule.
                  // If the logic is outside the submodule, we do not use
                  // the input name. Instead, we use whatever is driving
                  // the input. In other words, we do not use the Input name,
                  // if the component of the logic is the part of Input's
                  // component. We also do the same when assigning
                  // to the output if the output is the parent
                  // of the subcomponent.
                } else if (io.isIo && io.dir == INPUT &&
                           ((!this.isIo
                             && this.component == io.component.parent)
                             || (this.isInstanceOf[Bits]
                               && this.asInstanceOf[Bits].dir == OUTPUT &&
                               this.component == io.component.parent))) {
                  if (io.inputs.length > 0) inputs(j) = io.inputs(0);
                }
              case any =>
            };
          });
        }
        i += 1;
      }
      comp.mods += this;
    }
  }

  def forceMatchingWidths { }

  def matchWidth(w: Width): Node = {
    val this_width = this.width
    if (w.isKnown && this_width.isKnown) {
      val my_width = this_width.needWidth()
      val match_width = w.needWidth()
      if (match_width > my_width) {
        val zero = Literal(0, match_width - my_width); zero.infer
        val res = Concatenate(zero, this); res.infer
        res
      } else if (match_width < my_width) {
        val res = NodeExtract(this, match_width-1,0); res.infer
        res
      } else {
        this
      }
    } else {
      ChiselError.error("Node.matchWidth with unknown width: " + w + ", node " + this)
      this
    }
  }

  def setName(n: String) {
    name = n
    named = true;
  }

  var isWidthWalked = false;

  def getWidth(): Int = {
    val oldDriverisInGetWidth = Driver.isInGetWidth
    Driver.isInGetWidth = true
    val w = width
    Driver.isInGetWidth = oldDriverisInGetWidth
    if (w.isKnown)
      w.needWidth()
    else
      throwException("Node.getWidth() for node " + this + " returns unknown width")
  }

  def getWWidth(): Width = {
    val oldDriverisInGetWidth = Driver.isInGetWidth
    Driver.isInGetWidth = true
    val w = width
    Driver.isInGetWidth = oldDriverisInGetWidth
    w
  }

  def removeTypeNodes() {
    for(i <- 0 until inputs.length) {
      if(inputs(i) == null){
        val error = new ChiselError(() => {"NULL Input for " + this.getClass + " " + this + " in Module " + component}, this.line);
        if (!ChiselErrors.contains(error)) {
          ChiselErrors += error
        }
      }
      else if(inputs(i).isTypeNode) {
        inputs(i) = inputs(i).getNode;
      }
    }
  }

  def getNode: Node =
    if (!isTypeNode || inputs.isEmpty) this
    else inputs(0).getNode

  def toBits(): UInt = chiselCast(this){UInt()}

  def toNode: Node = this

  def addConsumers() {
    for ((i, off) <- inputs.zipWithIndex) {
      /* By construction we should not end-up with null inputs. */
      assert(i != null, ChiselError.error("input " + off
        + " of " + inputs.length + " for node " + this + " is null"))
      if(!i.consumers.contains(this)) {
        i.consumers += this;
      }
    }
  }

  def extract (widths: Array[Int]): List[UInt] = {
    var res: List[UInt] = Nil;
    var off = 0;
    for (w <- widths) {
      res  = this.asInstanceOf[UInt](off + w - 1, off) :: res;
      off += w;
    }
    res.reverse
  }
  def extract (b: Bundle): List[Node] = {
    var res: List[Node] = Nil;
    var off = 0;
    for ((n, io) <- b.flatten) {
      if (io.dir == OUTPUT) {
        val w = io.needWidth();
        res  = NodeExtract(this,off + w - 1, off) :: res;
        off += w;
      }
    }
    res.reverse
  }
  def maybeFlatten: Seq[Node] = {
    this match {
      case b: Bundle =>
        val buf = ArrayBuffer[Node]();
        for ((n, e) <- b.flatten) buf += e.getNode;
        buf
      case o        =>
        Array[Node](getNode);
    }
  }

  lazy val emitIndex: Int = componentOf.nextIndex

  override def hashCode: Int = _id
  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  def canCSE: Boolean = false
  def hashCodeForCSE: Int = inputs.head.hashCode
  def equalsForCSE(x: Node): Boolean = false

  def _isComplementOf(x: Node): Boolean = {
    def checkOne(x: Node, y: Node) = x.getNode match {
      case op: Op => op.op == "^" && op.inputs(0).getNode == y.getNode && op.inputs(1).litValue() == 1
      case _ => false
    }
    checkOne(this, x) || checkOne(x, this)
  }
  def setWidth(w: Int) = {
    width_.setWidth(w)
    inferWidth = fixWidth(w);
  }
  // Return a value or raise an exception.
  def needWidth(): Int = width.needWidth()

  // Return true if the width of this node is known (set).
  def isKnownWidth: Boolean = width.isKnown
}
