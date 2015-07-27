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

import scala.collection.mutable.{ArrayBuffer, Stack, LinkedHashSet}

object Node {
  def sprintf(message: String, args: Node*): Bits = {
    val s = Bits().fromNode(new Sprintf(message, args))
    s.setIsTypeNode
    s
  }

  def fixWidth(w: => Int): (=> Node) => Width = { n => {
    assert(w != -1, ChiselError.error({"invalid width for fixWidth object"}))
    Width(w)
  }}

  def widthOf(i: => Int): (=> Node) => Width = { (m) => {
    try {
      m.inputs(i).widthW
    } catch {
        case e: java.lang.IndexOutOfBoundsException => {
          val error = new ChiselError(() => {m + " is unconnected ("+ i + " of " + m.inputs.length + "). Ensure that it is assigned."}, m.line)
          if (!ChiselError.contains(error) && !Driver.isInGetWidth) {
            ChiselError.error(error)
          }
          Width()
       }
    }
  }}

  // Compute the maximum width required for a node,
  def maxWidth(m: => Node): Width = {
    var w = Width(0)
    for (i <- m.inputs if i != m)
      w = w.max(i.widthW)
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
  private[Chisel] var sccIndex = -1
  private[Chisel] var sccLowlink = -1
  private[Chisel] var depth = 0
  var prune = false
  var driveRand = false
  var isTypeNode = false
  /* Assigned in Binding and Mod.reset */
  private[Chisel] var compOpt: Option[Module] = Module.getComponent
  def component: Module = compOpt getOrElse { throwException("< " + this + " > doesn't have its component, yet.") } 
  def componentOf = if (Driver.backend.isEmittingComponents && compOpt != None) component else Module.topMod
  // The semantics of width are sufficiently complicated that
  // it deserves its own class
  private[Chisel] var width_ = Width()
  private[Chisel] var inferWidth: (=> Node) => Width = Node.maxWidth
  var clock: Option[Clock] = None
  val inputs = ArrayBuffer[Node]()
  val consumers = LinkedHashSet[Node]() // nodes that consume one of my outputs
  val line: StackTraceElement =
    if (Driver.getLineNumbers) {
      val trace = new Throwable().getStackTrace
      ChiselError.findFirstUserLine(trace) getOrElse trace(0)
    } else null

  val _id = Driver.nodes.length
  Driver.nodes += this

  private[Chisel] def width: Int = {
    val w = widthW
    if (w.isKnown)
      w.needWidth()
    else
      throwException("Node.width for node " + this + " returns unknown width")
  }
  private[Chisel] def widthW: Width = {
    val selfresult = if (Driver.isInGetWidth) inferWidth(this) else width_
    if(!selfresult.isKnown && isTypeNode && !inputs.isEmpty) getNode.widthW
    else selfresult
  }

  /** Sets the width of a Node. */
  private[Chisel] def width_=(w: Int) {
    width_.setWidth(w);
    inferWidth = Node.fixWidth(w);
  }

  private[Chisel] def width_=(w: Width) {
    width_ = w;
    // NOTE: This explicitly does not set inferWidth.
    // See the comments in infer
  }

  def setName(n: String) { name = n ; named = true }

  def nameIt (path: String, isNamingIo: Boolean) {
    try {
      if (!named && (!isIo || isNamingIo)) {
        /* If the name was set explicitly through *setName*,
         we don't override it. */
        setName(path)
      }
      // Don't bother trying to make unnamed nodes unambiguous.
      // TODO - We might want to distinguish generated versus user-specified names.
      //  and only disambiguate the generated names.
      //  Ambiguous user-specified names should trigger an error.
      if (name != "") {
        while (component.names.getOrElseUpdate(name, this) ne this) {
          name += "_"
        }
      }
    } catch {
      case e:NullPointerException => {
        println("Node:nameIt() NullPointerException: name '" + name + "'")
        printTree(System.out, 1)
        throwException("NullPointer " + this.getClass + ":name " + name, e)
      }
    }
  }

  lazy val chiselName = this match {
    case l: Literal => ""
    case _ if named && name != "reset" && compOpt != None =>
        component.getPathName(".") + "." + name
    case _ => ""
  }

  // TODO: REMOVE WHEN LOWEST DATA TYPE IS BITS
  def ##(b: Node): Node  = Op("##", Node.sumWidth _,  this, b );
  final def isLit: Boolean = litOpt ne None
  private[Chisel] def litOpt: Option[Literal] = if (getNode != this) getNode.litOpt else None
  def litOf: Literal = litOpt match { case Some(l) => l case None => throwException("no lit value for this node") }
  def litValue(default: BigInt = BigInt(-1)): BigInt = litOpt match {
    case None => default case Some(l) => l.value }
  def floLitValue: Float = java.lang.Float.intBitsToFloat(litValue().toInt)
  def dblLitValue: Double = java.lang.Double.longBitsToDouble(litValue().toLong)
  // TODO: MOVE TO WIRE
  def assign(src: Node): Unit = throw new Exception("unimplemented assign")
  def <>(src: Node): Unit = throw new Exception("unimplemented <>")
  def ^^(src: Node): Unit = src <> this

  private var _isIo = false
  def isIo = _isIo
  def isIo_=(isIo: Boolean) = _isIo = isIo
  def isReg: Boolean = false
  def isUsedByClockHi: Boolean = consumers.exists(_.usesInClockHi(this))
  def usesInClockHi(i: Node): Boolean = false
  def initOf (n: String, widthfunc: (=> Node) => Width, ins: Iterable[Node]): Node = {
    name = n
    inferWidth = widthfunc
    inputs ++= ins
    this
  }
  def init (n: String, widthFunc: (=> Node) => Width, ins: Node*): Node = {
    initOf(n, widthFunc, ins.toList);
  }
  def init (n: String, w: Int, ins: Node*): Node = {
    width_ = Width(w)
    initOf(n, Node.fixWidth(w), ins.toList)
  }

  // Called while we're walking the graph inferring the width of nodes.
  // We return true if we should continue to walk the graph,
  // either because there's a node whose width we don't know,
  // or because we updated a node's width.
  def infer: Boolean = {
    val res = inferWidth(this)
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
  
  def isTopLevelIO = isIo && component == Module.topMod

  private[Chisel] lazy val isInObject =
    (isIo && (Driver.isIoDebug || component == Module.topMod)) || 
    (Module.topMod.debugs contains this) || (Driver.backend isInObject this) ||
    isReg || isUsedByClockHi || Driver.isDebug && named || Driver.emitTempNodes

  private[Chisel] lazy val isInVCD = Driver.isVCD && name != "reset" && needWidth() > 0 &&
     (named || Driver.emitTempNodes) &&
     ((isIo && isInObject) || isReg || Driver.isDebug)

  /** Prints all members of a node and recursively its inputs up to a certain
    depth level. This method is purely used for debugging. */
  def printTree(writer: java.io.PrintStream, depth: Int = 4, indent: String = ""): Unit = {
    if (depth < 1) return
    writer.println(indent + getClass + " width=" + getWidth + " #inputs=" + inputs.length)
    this match {
      case fix: SInt => fix.comp match {
        case None =>
        case Some(p) =>
          writer.println(indent + "  (has comp " + p + " of type " + p.getClass + ")")
      }
      case bits: UInt => bits.comp match {
        case None =>
        case Some(p) =>
          writer.println(indent + "(has comp " + p + ")")
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
    writer.println("consumers.size: " + consumers.size)
    writer.println("line: " + line)
    for (in <- inputs) {
      in.printTree(writer, depth-1, indent + "  ");
    }
  }

  private[Chisel] def forceMatchingWidths { }

  private[Chisel] def matchWidth(w: Width): Node = {
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

  private[Chisel] def getWidthW(): Width = {
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

  private[Chisel] def removeTypeNodes() {
    for ((input, i) <- inputs.zipWithIndex) {
      inputs(i) = input.getNode
    }
  }

  def getNode: Node =
    if (!isTypeNode || inputs.isEmpty) this
    else inputs(0).getNode

  def toBits(): UInt = chiselCast(this){UInt()}

  def toNode: Node = this

  private[Chisel] def addConsumers() {
    for ((i, off) <- inputs.zipWithIndex) {
      /* By construction we should not end-up with null inputs. */
      assert(i != null, ChiselError.error("input " + off
        + " of " + inputs.length + " for node " + this + " is null"))
      i.consumers += this
    }
  }

  // TODO: Only used in old testers
  def maybeFlatten: Seq[Node] = {
    this match {
      case b: Bundle => b.flatten.unzip._2 map (_.getNode)
      case _         => Array[Node](getNode)
    }
  }

  private[Chisel] lazy val emitIndex: Int = componentOf.nextIndex

  override def hashCode: Int = _id
  override def equals(that: Any): Boolean = that match {
    case n: Node => this eq n
    case _ => ChiselError.error("can't compare Node " + this + " and non-Node " + that); false
  }

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
    inferWidth = Node.fixWidth(w)
  }
  // Return a value or raise an exception.
  def needWidth(): Int = widthW.needWidth 
  // Return true if the width of this node is known (set).
  private[Chisel] def isKnownWidth: Boolean = widthW.isKnown

  // The following are used for optimizations, notably, dealing with zero-width wires.
  /* If we've updated this node since our last visit. */
  var modified = false
  // Eliminate any zero-width wires attached to this node.
  // Return true if we modified the node.
  def W0Wtransform() {
    // If we're just a type node, we're now a zero-width type node.
    if (isTypeNode) {
      setWidth(0)
      modified = true
    }
  }

  // Review a node for optimization possibilities if its children have been updated.
  def review() { }

  // Replace the subtree starting from this node with the indicated replacement.
  def replaceTree(newNode: Node) {
    val oldNode = this

    /* We are no longer anyone's parent. */
    for (c <- inputs) {
      c.consumers -= oldNode
    }

    /* Replace our role as input in our parent nodes with the replacement node. */
    for (p <- oldNode.consumers; i <- 0 until p.inputs.length if p.inputs(i) == oldNode) {
      newNode.consumers += p
      p.inputs(i) = newNode
    }

    oldNode.inputs.clear()
    oldNode.consumers.clear()
  }

  // Chisel3 - type-only nodes (no data - no initialization or assignment)
  // This is used to determine which nodes must be Wire() wrapped,
  //  and whether Wire() wrapping of the node is legal or not.
  protected[Chisel] def isTypeOnly: Boolean = {
    // If we're the last node in a type chain, the chain is type only.
    // Nodes with real data will override this definition.
    // NOTE: We don't look at this node's inputs, since if this is an assignment,
    //  they will be the source of the assignment, and they will most likely be data carrying nodes.
    val gNode = getNode
    if (gNode == this) {
      true
    } else {
      gNode.isTypeOnly
    }
  }
}
