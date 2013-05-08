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
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import java.io.PrintStream

import Node._;
import Component._;
import ChiselError._;

object Node {
  //implicits
  implicit def convBitsToBool(x: Bits): Bool = {
    if(x.getWidth > 1) {
      throw new Exception("multi bit signal " + x + " converted to Bool");
    }
    if(x.getWidth == -1) {
      throw new Exception("unable to automatically convert " + x + " to Bool, convert manually instead");
    }
    x.toBool
  }

  implicit def convBitsToUFix(x: Bits): UFix = x.toUFix;

  var isCoercingArgs = true;
  val conds = new Stack[Bool]();
  conds.push(Bool(true));
  val keys  = new Stack[Bits]();

  var isInGetWidth = false

  def fixWidth(w: Int) = { Predef.assert(w != -1, {println("invalid width for fixWidth object")});
                          (m: Node) => {m.isFixedWidth = true; w} };

  def widthOf(i: Int) = { (m: Node) => {
    try {
      m.inputs(i).width
    } catch {
        case e: java.lang.IndexOutOfBoundsException => {
          val error = new ChiselError(() => {m + " in " + m.component + " is unconnected. Ensure that is assigned."}, m.line)
          if (!ChiselErrors.contains(error) && !isInGetWidth) {
            ChiselErrors += error
          }
          -1
        }
    }}}

  def maxWidth(m: Node): Int = {
    var w = 0
    for (i <- m.inputs)
      if (!(i == null || i == m)) {
        w = w.max(i.width)
      }
    w
  }

  def minWidth(m: Node): Int = m.inputs.map(_.width).min

  def maxWidthPlusOne(m: Node): Int = maxWidth(m) + 1;

  def sumWidth(m: Node): Int = {
    var res = 0;
    for (i <- m.inputs)
      res = res + i.width;
    res
  }

  def lshWidthOf(i: Int, n: Node) = {
    (m: Node) => {
      val res = m.inputs(0).width + n.maxNum.toInt;
      res
    }
  }

  def rshWidthOf(i: Int, n: Node) = { (m: Node) => m.inputs(i).width - n.minNum.toInt }

  var clk: Node = Bits(INPUT, 1)
  clk.setName("clk")

}

abstract class Node extends nameable {
  var sccIndex = -1
  var sccLowlink = -1
  var walked = false;
  val staticComp: Component = getComponent();
  var component: Component = null;
  var flattened = false;
  var isTypeNode = false;
  var depth = 0;
  def componentOf: Component = if (isEmittingComponents && component != null) component else topComponent
  var isSigned = false;
  var width_ = -1;
  var index = -1;
  var isFixedWidth = false;
  var consumers = new ArrayBuffer[Node]; // mods that consume one of my outputs
  var inputs = new ArrayBuffer[Node];
  def traceableNodes = Array[Node]();
  var outputs = new ArrayBuffer[Node];
  var inferWidth: (Node) => Int = maxWidth;
  var nameHolder: nameable = null;
  var isClkInput = false;
  var inferCount = 0;
  var genError = false;
  var stack: Array[StackTraceElement] = null;
  var line: StackTraceElement = findFirstUserLine(Thread.currentThread().getStackTrace)
  var isScanArg = false
  var isPrintArg = false
  def isMemOutput = false

  nodes += this

  def setIsSigned = isSigned = true
  def isByValue: Boolean = true;
  def width: Int = if(isInGetWidth) inferWidth(this) else width_;
  def width_=(w: Int) = { isFixedWidth = true; width_ = width; inferWidth = fixWidth(w); }

  def nameIt (path: String) {
    if( !named ) {
      /* If the name was set explicitely through *setName*,
       we don't override it. */
      name = path;
    }
  }

  // TODO: REMOVE WHEN LOWEST DATA TYPE IS BITS
  def ##(b: Node): Node  = Op("##", 2, sumWidth _,  this, b );
  def maxNum: BigInt = if(litOf != null) litOf.value else ((1 << (if(width < 0) inferWidth(this) else width))-1);
  def minNum: BigInt = BigInt(0);
  // TODO: SHOULD BE GENERALIZED TO DIG FOR LIT AS litOf DOES
  def isLit = false;
  def clearlyEquals(x: Node): Boolean = this == x
  // TODO: SHOULD AGREE WITH isLit
  def litOf: Literal = {
    if(inputs.length == 0) {
      if (isLit) this.asInstanceOf[Literal] else null
    } else if(inputs.length == 1 && isInstanceOf[Bits] && inputs(0) != null) {
      inputs(0).litOf
    } else {
      null
    }
  }
  def litValue(default: BigInt = BigInt(-1)): BigInt = {
    val lit = litOf
    if (lit == null) default else lit.value
  }
  def value = BigInt(-1);
  def signed: this.type = {
    val res = Fix()
    res := this.asInstanceOf[Fix];
    res.isSigned = true;
    res.asInstanceOf[this.type]
  }
  def bitSet(off: UFix, dat: Bits): Bits = {
    val bit = Bits(1, 1) << off;
    (this.asInstanceOf[Bits] & ~bit) | (dat << off);
  }
  // TODO: MOVE TO WIRE
  def assign(src: Node) = {
    if (inputs.length > 0) {
      inputs(0) = src;
    } else {
      inputs += src;
    }
  }
  def <>(src: Node) = {
    this assign src
  }
  def ^^(src: Node) = {
    println("NODE ^^ " + this.getClass + " " + src);
    src assign this;
  }
  def getLit = this.asInstanceOf[Literal]
  def isIo = false;
  def isReg = false;
  def isUsedByRam: Boolean = {
    for (c <- consumers)
      if (c.isRamWriteInput(this)) {
        return true;
      }
    return false;
  }
  def isRamWriteInput(i: Node) = false;
  def initOf (n: String, width: (Node) => Int, ins: List[Node]): Node = {
    name = n;
    inferWidth = width;
    for (i <- ins)
      inputs += i;
    this
  }
  def init (n: String, width: (Node) => Int, ins: Node*): Node = {
    initOf(n, width, ins.toList);
  }
  def init (n: String, w: Int, ins: Node*): Node = {
    isFixedWidth = true;
    width_ = w;
    initOf(n, fixWidth(w), ins.toList)
  }
  def infer: Boolean = {
    val res = inferWidth(this);
    if(inferCount > 1000000) {
      if(genError) {
        val error = new ChiselError(() => {"Unable to infer width of " + this}, this.line);
        if (!ChiselErrors.contains(error)) {
          ChiselErrors += error
        }
      } else {
        genError = true;
      }
      return false;
    }
    if(res == -1) {
      return true
    } else if (res != width) {
      width_ = res;
      return true;
    } else{
      return false;
    }
  }
  def isInObject =
    (isIo && (isIoDebug || component == topComponent)) ||
    (topComponent.debugs.contains(this) && !name.isEmpty) ||
    isReg || isUsedByRam || isDebug || isPrintArg || isScanArg;
  def isInVCD = (isIo && isInObject) || isReg || (isDebug && !name.isEmpty);
  def dotName = { val name = this.getClass.getName; name.substring(7, name.size) };

  /** Prints all members of a node and recursively its inputs up to a certain
    depth level. This method is purely used for debugging. */
  def printTree(writer: PrintStream, depth: Int = 4, indent: String = ""): Unit = {
    if (depth < 1) return;
    writer.println(indent + getClass + " width=" + getWidth + " #inputs=" + inputs.length);
    this match {
      case fix: Fix => {
        if (!(fix.comp == null)) {
          writer.println(indent + "  (has comp " + fix.comp + " of type " + fix.comp.getClass + ")");
        }
      }
      case ufix: UFix => {
        if (!(ufix.comp == null)) {
          writer.println(indent + "(has comp " + ufix.comp + ")");
        }
      }
      case bits: Bits => {
        if (!(bits.comp == null)) {
          writer.println(indent + "(has comp " + bits.comp + ")");
        }
      }
      case any =>
    }
    writer.println("sccIndex: " + sccIndex)
    writer.println("sccLowlink: " + sccLowlink)
    writer.println("walked: " + walked)
    writer.println("component: " + component)
    writer.println("flattened: " + flattened)
    writer.println("isTypeNode: " + isTypeNode)
    writer.println("depth: " + depth)
    writer.println("isSigned: " + isSigned)
    writer.println("width_: " + width_)
    writer.println("index: " + index)
    writer.println("isFixedWidth: " + isFixedWidth)
    writer.println("consumers.length: " + consumers.length)
    writer.println("outputs.length: " + outputs.length)
    writer.println("nameHolder: " + nameHolder)
    writer.println("nameHolder: " + nameHolder)
    writer.println("isClkInput: " + isClkInput)
    writer.println("inferCount: " + inferCount)
    writer.println("genError: " + genError)
    writer.println("stack.length: " + (if(stack != null) { stack.length } else { 0 }))
    writer.println("line: " + line)
    writer.println("isScanArg: " + isScanArg)
    writer.println("isPrintArg: " + isPrintArg)
    writer.println("isMemOutput: " + isMemOutput)
    for (in <- inputs) {
      if (in == null) {
        writer.println("null");
      } else {
        in.printTree(writer, depth-1, indent + "  ");
      }
    }
  }

  def traceNode(c: Component, stack: Stack[() => Any]): Any = {
    if(this.isTypeNode) println("found " + this)
    // determine whether or not the component needs a clock input
    if ((isReg || isClkInput) && !(component == null)) {
      component.containsReg = true
    }

    // pushes and pops components as necessary in order to later mark the parent of nodes
    val (comp, nextComp) =
      this match {
        case io: Bits => {
          if(io.dir == INPUT || io.dir == OUTPUT) {
            (io.component, if (io.dir == OUTPUT) io.component else io.component.parent)
          } else {
            (c, c)
          }
        }
        case any    => (c, c);
      }

    // give the components reset signal to the current node
    if(this.isInstanceOf[Reg]) {
      val reg = this.asInstanceOf[Reg]
      if(reg.isReset) reg.inputs += reg.component.reset
      reg.hasResetSignal = true
    }

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
          if (node.component == null || !components.contains(node.component)) {
            node.component = nextComp;
          }
          if (!backend.isInstanceOf[VerilogBackend] || !node.isIo) {
            stack.push(() => node.traceNode(nextComp, stack));
          }
          val j = i;
          val n = node;
          stack.push(() => {
            // This code finds an output binding for a node. We search for a binding only if the io is an output
            // and the logic's grandfather component is not the same as the io's component and
            // the logic's component is not same as output's component unless the logic is an input
            n match {
              case io: Bits =>
                if (io.dir == OUTPUT && !io.isTypeNode &&
                    (!(component.parent == io.component) &&
                     !(component == io.component &&
                       !(this.isInstanceOf[Bits] && this.asInstanceOf[Bits].dir == INPUT)))) {
                  val c = n.component.parent;
                  val b = Binding(n, c, io.component);
                  inputs(j) = b;
                  if (!c.isWalked.contains(b)) {
                    c.mods += b;  c.isWalked += b;
                  }
                  // In this case, we are trying to use the input of a submodule
                  // as part of the logic outside of the submodule.
                  // If the logic is outside the submodule, we do not use the input
                  // name. Instead, we use whatever is driving the input. In other
                  // words, we do not use the Input name, if the component of the
                  // logic is the part of Input's component.
                  // We also do the same when assigning to the output if the output
                  // is the parent of the subcomponent;
                } else if (io.dir == INPUT &&
                           ((!this.isIo && this.component == io.component.parent) ||
                            (this.isInstanceOf[Bits] && this.asInstanceOf[Bits].dir == OUTPUT &&
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

  def matchWidth(w: Int): Node = {
    if (w > this.width) {
      val topBit = if (isSigned) NodeExtract(this, this.width-1) else Literal(0,1)
      topBit.infer
      val fill = NodeFill(w - this.width, topBit); fill.infer
      val res = Concatenate(fill, this); res.infer
      res
    } else if (w < this.width) {
      val res = NodeExtract(this, w-1,0); res.infer
      res
    } else {
      this
    }
  }

  def setName(n: String) {
    name = n
    named = true;
  }

  def setIsClkInput {};

  var isWidthWalked = false;

  def getWidth(): Int = {
    isInGetWidth = true
    val w = width
    isInGetWidth = false
    w
  }

  def setTypeNodeNoAssign[T <: Data](typeNode: T): T = {
    typeNode.setIsTypeNode
    if(!isInstanceOf[Literal]) nameHolder = typeNode
    typeNode
  }
  def setTypeNode[T <: Data](typeNode: T): T = {
    typeNode assign this
    setTypeNodeNoAssign(typeNode)
    typeNode
  }

  def removeTypeNodes() {
    for(i <- 0 until inputs.length) {
      if(inputs(i) == null){
        val error = new ChiselError(() => {"NULL Input for " + this.getClass + " " + this + " in Component " + component}, this.line);
        if (!ChiselErrors.contains(error)) {
          ChiselErrors += error
        }
      }
      else if(inputs(i).isTypeNode) {
        inputs(i) = inputs(i).getNode;
      }
    }
  }
  def getNode(): Node = {
    if(!isTypeNode || inputs.length == 0) {
      this
    } else {
      inputs(0).getNode
    }
  }

  def addConsumers(): Boolean = {
    var off = 0;
    for (i <- inputs) {
      if (i == null) {
        println(this + " " + inputs + " HAS NULL INPUT " + off + "/" + inputs.length + " IN " + component);
        inputs = ArrayBuffer(inputs(0));
        return false;
      } else if(!i.consumers.contains(this)) {
        i.consumers += this;
      }
      off += 1;
    }
    true;
  }

  def extract (widths: Array[Int]): List[Fix] = {
    var res: List[Fix] = Nil;
    var off = 0;
    for (w <- widths) {
      res  = this.asInstanceOf[Fix](off + w - 1, off) :: res;
      off += w;
    }
    res.reverse
  }
  def extract (b: Bundle): List[Node] = {
    var res: List[Node] = Nil;
    var off = 0;
    for ((n, io) <- b.flatten) {
      if (io.dir == OUTPUT) {
        val w = io.width;
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
  def emitIndex(): Int = {
    if (index == -1) {
      index = componentOf.nextIndex;
    }
    index
  }

}
