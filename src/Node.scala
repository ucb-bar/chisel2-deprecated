package Chisel
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Queue._
import scala.collection.mutable.Stack

import scala.math.max;
import Node._;
import Component._;
import ChiselError._;

object Node {
  //implicits
  implicit def convBitsToBool(x: Bits): Bool = {
      if(x.getWidth > 1)
      throw new Exception("multi bit signal " + x + " converted to Bool");
    if(x.getWidth == -1)
      throw new Exception("unable to automatically convert " + x + " to Bool, convert manually instead");
    x.toBool
  }

  implicit def convBitsToUFix(x: Bits): UFix = x.toUFix;

  var isHiC = false;
  var isCoercingArgs = true;
  var conds = new Stack[Bool]();
  conds.push(Bool(true));
  var keys  = new Stack[Bits]();
  def fixWidth(w: Int) = { (m: Node) => {m.isFixedWidth = true; w} };
  def widthOf(i: Int) = { (m: Node) => { 
    try { 
      m.inputs(i).getWidth 
    } catch { 
        case e: java.lang.IndexOutOfBoundsException => {
          val error = ChiselError({m + " in " + m.component + " is unconnected. Ensure that is assigned."}, m)
          if (!ChiselErrors.contains(error))
            ChiselErrors += error
          -1
        }
    }}}
  def maxWidth(m: Node): Int = {
    var res = 0;
    for (i <- m.inputs)
      if(!(i == null) && !(i == m)){
	res = max(res, i.getWidth);
      }
    res
  }
  def maxWidthPlusOne(m: Node): Int = maxWidth(m) + 1;
  def sumWidth(m: Node): Int = {
    var res = 0;
    for (i <- m.inputs)
      res = res + i.getWidth;
    res
  }
  def lshWidthOf(i: Int, n: Node) = { 
    (m: Node) => {
      val mwidth = m.inputs(i).getWidth;
      val nMax = n.maxNum;
      val res = m.inputs(i).getWidth + n.maxNum.toInt;
      res
    } 
  }
  def rshWidthOf(i: Int, n: Node) = { (m: Node) => m.inputs(i).getWidth - n.minNum.toInt }

  var clk: Node = Bits(1, INPUT)
  clk.setName("clk")

  var stop = true;
  
}

abstract class Node extends nameable{
  var sccIndex = -1
  var sccLowlink = -1
  var walked = false;
  var staticComp: Component = getComponent();
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
  var line: Array[StackTraceElement] = Thread.currentThread().getStackTrace
  var memSource: MemAccess = null
  var isScanArg = false
  var isPrintArg = false
  def isMemOutput = false
  
  def isByValue: Boolean = true;
  def width: Int = width_;
  def width_=(w: Int) = { isFixedWidth = true; width_ = width; inferWidth = fixWidth(w); }
  def name_it (path: String, setNamed: Boolean = true) = { name = path; named = setNamed}
  // TODO: REMOVE WHEN LOWEST DATA TYPE IS BITS
  def ##(b: Node): Node  = Op("##", 2, sumWidth _,  this, b ); 
  def maxNum: BigInt = (1 << (if(width < 0) inferWidth(this) else width))-1;
  def minNum: BigInt = BigInt(0);
  // TODO: SHOULD BE GENERALIZED TO DIG FOR LIT AS litOf DOES
  def isLit = false;
  // TODO: SHOULD AGREE WITH isLit
  def litOf: Literal = {
    if(inputs.length == 0)
      if (isLit) this.asInstanceOf[Literal] else null
    else if(inputs.length == 1 && isTypeNode)
      inputs(0).litOf
    else
      null
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
    if (inputs.length > 0) 
      inputs(0) = src; 
    else 
      inputs += src; 
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
  var isRegOut = false;
  def isUsedByRam: Boolean = {
    for (c <- consumers) 
      if (c.isRamWriteInput(this))
	return true;
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
	val error = ChiselError({"Unable to infer width of " + this}, this);
        if (!ChiselErrors.contains(error))
          ChiselErrors += error
      } else
	genError = true;
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
    (topComponent.debugs.contains(this) && named) || 
    isReg || isUsedByRam || isDebug || isPrintArg || isScanArg;
  def isInVCD = (isIo && isInObject) || isReg || (isDebug && named);
  def dotName = { val name = this.getClass.getName; name.substring(7, name.size) };

  def printTree(depth: Int = 4, indent: String = ""): Unit = {
    if (depth < 1) return;
    println(indent+getClass+" width="+getWidth+" #inputs="+inputs.length);
    this match {
      case fix: Fix => {
        if (!(fix.comp == null)) {
          println(indent+"  (has comp "+fix.comp+" of type "+fix.comp.getClass+")");
        }
      }
      case ufix: UFix => {
        if (!(ufix.comp == null)) {
          println(indent+"(has comp "+ufix.comp+")");
        }
      }
      case bits: Bits => {
        if (!(bits.comp == null)) {
          println(indent+"(has comp "+bits.comp+")");
        }
      }
      case any =>
    }
    for (in <- inputs) {
      if (in == null) {
        println("null");
      } else {
        in.printTree(depth-1, indent+"  ");
      }
    }
  }

  def traceNode(c: Component, stack: Stack[() => Any]): Any = {
    if(this.isTypeNode) println("found")
    // determine whether or not the component needs a clock input
    if ((isReg || isRegOut || isClkInput) && !(component == null))
        component.containsReg = true

    // pushes and pops components as necessary in order to later mark the parent of nodes
    val (comp, nextComp) = 
      this match {
        case io: Bits => {
          if(io.dir == INPUT || io.dir == OUTPUT)
            (io.component, if (io.dir == OUTPUT) io.component else io.component.parent)
          else
            (c, c)
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
          if (node.component == null || !components.contains(node.component))
            node.component = nextComp;
          stack.push(() => node.traceNode(nextComp, stack));
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

  def forceMatchingWidths = { }

  def matchWidth(w: Int): Node = {
    if (w > this.width) {
      val fill = NodeFill(w - this.width, Literal(0,1)); fill.infer
      val res = Concatenate(fill, this); res.infer
      res
    } else if (w < this.width) {
      val res = NodeExtract(this, w-1,0); res.infer
      res
    } else {
      this
    }
  }

  def fixName() = {
    if(nameHolder != null && !named && !isInstanceOf[Literal]){
      name = nameHolder.name;
      nameHolder.name = "";
      named = nameHolder.named;
    }
  }

  def setName(n: String) = {
    name = n
    named = true;
  }

  def setIsClkInput = {};

  var isWidthWalked = false;

  def getWidth(): Int = {
    if(width > 0)
      width
    else if(isTypeNode) {
      if(inputs.length == 0) -1 else inputs(0).getWidth
    } else if(isInstanceOf[Reg] && !isWidthWalked){
      isWidthWalked = true;
      inferWidth(this)
    }else if(inputs.length >= 1 && !isInstanceOf[Reg])
      inferWidth(this)
    else
      -1
  }
  
  def setTypeNodeNoAssign[T <: Data](typeNode: T): T = {
    typeNode.setIsTypeNode 
    if(!isInstanceOf[Literal]) nameHolder = typeNode
    typeNode
  }
  def setTypeNode[T <: Data](typeNode: T): T = {
    setTypeNodeNoAssign(typeNode)
    typeNode assign this
    typeNode
  }

  def removeTypeNodes() {
    for(i <- 0 until inputs.length) {
      if(inputs(i) == null){
        val error = ChiselError({"NULL Input for " + this.getClass + " " + this + " in Component " + component}, this);
        if (!ChiselErrors.contains(error))
          ChiselErrors += error
      }
      else if(inputs(i).isTypeNode) {
	inputs(i) = inputs(i).getNode;
      }
    }
  }
  def getNode(): Node = {
    if(!isTypeNode || inputs.length == 0)
      this
    else 
      inputs(0).getNode
  }

  def addConsumers(): Boolean = {
    var off = 0;
    for (i <- inputs) {
      if (i == null) {
        println(this + " " + inputs + " HAS NULL INPUT " + off + "/" + inputs.length + " IN " + component);
        inputs = ArrayBuffer(inputs(0));
        return false;
      } else if(!i.consumers.contains(this))
        i.consumers += this;
      off += 1;
    }
    true;
  }

  def extract (widths: Array[Int]): List[Fix] = {
    var res: List[Fix] = Nil;
    var off = 0;
    for (w <- widths) {
      res  = this.asInstanceOf[Fix](off+w-1, off) :: res;
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
        res  = NodeExtract(this,off+w-1, off) :: res;
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
    if (index == -1) 
      index = componentOf.nextIndex; 
    index 
  }

}
