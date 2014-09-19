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
    try {
      m.inputs(i).widthW
    } catch {
        case e: java.lang.IndexOutOfBoundsException => {
          val error = new ChiselError(() => {m + " is unconnected ("+ i + " of " + m.inputs.length + "). Ensure that it is assigned."}, m.line)
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
        w = w.max(i.widthW)
      }
    w
  }

  def minWidth(m: => Node): Width = m.inputs.map(_.widthW).min

  def maxWidthPlusOne(m: => Node): Width = maxWidth(m) + 1

  def sumWidth(m: => Node): Width = {
    var res = Width(0);
    for (i <- m.inputs)
      res = res + i.widthW;
    res
  }

  def lshWidthOf(i: => Int, n: => Node): (=> Node) => (Width) = {
    (m) => {
      val w0 = m.inputs(0).widthW
      var w = n.widthW
      // If we don't know the width, try inferring it.
      if (!w.isKnown) {
        w = n.inferWidth(n)
      }
      // If we still don't know it, return unknown.
      if (!w.isKnown) {
//        ChiselError.warning("lshWidthOf: width not set - " + n)
        Width()
      } else if (!w0.isKnown) {
//        ChiselError.warning("lshWidthOf: child width not set - " + m.inputs(0))
        Width()
      } else {
        w0 + n.litValue((1 << w.needWidth)-1).toInt
      }
    }
  }

  def rshWidthOf(i: => Int, n: => Node): (=> Node) => (Width) = {
    (m) => {
      val a = m.inputs(i).widthW
      val b = n.litValue(0).toInt
      val w = a - b
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
  var cntrIdx = -1

  val _id = Driver.nodes.length
  Driver.nodes += this

  def isByValue: Boolean = true;
  private[Chisel] def width: Int = {
    val w = widthW
    if (w.isKnown)
      w.needWidth()
    else
      throwException("Node.width for node " + this + " returns unknown width")
  }
  private[Chisel] def widthW: Width = {
    if (Driver.isInGetWidth) inferWidth(this) else width_
  }

  /** Sets the width of a Node. */
  private[Chisel] def width_=(w: Int) {
    width_.setWidth(w);
    inferWidth = fixWidth(w);
  }

  private[Chisel] def width_=(w: Width) {
    width_ = w;
    // NOTE: This explicitly does not set inferWidth.
    // See the comments in infer
  }

  def nameIt (path: String, isNamingIo: Boolean) {
    try {
      if (!named && (!isIo || isNamingIo)) {
        /* If the name was set explicitely through *setName*,
         we don't override it. */
        setName(path)
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

  lazy val chiselName = this match {
    case l: Literal => "";
    case any        =>
      if (named && (name != "reset") && !(component == null)) 
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
    } else if (res != widthW) {
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

  def forceMatchingWidths { }

  def matchWidth(w: Width): Node = {
    val this_width = this.widthW
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

  def getWidthW(): Width = {
    val oldDriverisInGetWidth = Driver.isInGetWidth
    Driver.isInGetWidth = true
    val w = widthW
    Driver.isInGetWidth = oldDriverisInGetWidth
    w
  }

  def getWidth(): Int = {
    val w = getWidthW()
    if (w.isKnown)
      w.needWidth()
    else
      throwException("Node.getWidth() for node " + this + " returns unknown width")
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
  def needWidth(): Int = {
    val w = widthW
    w.needWidth()
  }

  // Return true if the width of this node is known (set).
  def isKnownWidth: Boolean = {
    val w = widthW
    w.isKnown
  }
}
