// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Queue._;
import scala.collection.mutable.Stack

import scala.math.max;
import Node._;
import Component._;
import IOdir._;
import ChiselError._;

object Node {
  // var cond = new Stack[Node];
  var isCoercingArgs = true;
  var conds = new Stack[Bool]();
  var keys  = new Stack[Bits]();
  def fixWidth(w: Int) = { (m: Node) => w };
  def widthOf(i: Int) = { (m: Node) => { 
    try { 
      m.inputs(i).getWidth 
    } catch { 
        case e: java.lang.IndexOutOfBoundsException => {
          val error = IllegalState(m + " in " + m.component + " is unconnected. Ensure that is assigned.", 0)
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
      val res = m.inputs(i).getWidth + n.maxNum;
      res
    } 
  }
  def rshWidthOf(i: Int, n: Node) = { (m: Node) => m.inputs(i).getWidth - n.minNum }
  var reset: Fix = Input("reset", 1);
  var resets = Queue[Fix]();
  var clk: Node = Input("clk", 1);
  def pushReset(r: Fix) { resets.enqueue(reset); reset = r }
  def popReset() { reset = resets.dequeue() }
  def withReset(r: Fix)(block: => Node) = {
    val resBak = reset; reset = r; 
    val res = block; 
    reset = resBak;
    res
  }
  def ListLookup(addr: Node, default: List[Node], mapping: Array[(Node, List[Node])]): List[UFix] = {
    val ll = new ListLookup(mapping, default);
    ll.init("", widthOf(1), addr);
    for (w <- ll.wires)
      w.lookup = ll;
    for (e <- default)
      ll.inputs += e;
    for ((addr, data) <- mapping){
      for (e <- data){
        ll.inputs += e;
      }
    }
    ll.wires.map(x => {
      val res = UFix('output);
      res.setIsCellIO;
      x.nameHolder = res;
      res assign x;
      res
    })
  }
  
  var stop = true;
  
}

abstract class Node extends nameable{
  var walked = false;
  var component: Component = null;
  var flattened = false;
  var isCellIO = false;
  var depth = 0;
  def componentOf: Component = if (isEmittingComponents && component != null) component else topComponent
  var isSigned = false;
  var width_ = -1;
  var index = -1;
  var isFixedWidth = false;
  var consumers = new ArrayBuffer[Node]; // mods that consume one of my outputs
  var inputs = new ArrayBuffer[Node];
  var outputs = new ArrayBuffer[Node];
  var inferWidth: (Node) => Int = maxWidth;
  var nameHolder: nameable = null;
  var isClkInput = false;
  var inferCount = 0;
  var genError = false;
  var stack: Array[StackTraceElement] = null;
  def width: Int = width_;
  def width_=(w: Int) = { isFixedWidth = true; width_ = width; inferWidth = fixWidth(w); }
  def name_it (path: String, setNamed: Boolean = true) = { name = path; named = setNamed}
  /*
  def unary_-(): Node    = Op("-",  1, widthOf(0), this);
  def unary_~(): Node    = Op("~",  1, widthOf(0), this);
  def unary_!(): Node    = Op("!",  1, fixWidth(1), this);
  def andR(): Node       = Op("&",  1, fixWidth(1), this);
  def orR(): Node        = Op("|",  1, fixWidth(1), this);
  def xorR(): Node       = Op("^",  1, fixWidth(1), this);
  def <<(b: Node): Node  = Op("<<", 0, lshWidthOf(0, b),  this, b );
  def >>(b: Node): Node  = Op(">>", 0, rshWidthOf(0, b),  this, b );
  def >>>(b: Node): Node = Op(">>", 0, rshWidthOf(0, b),  this, b );
  def +(b: Node): Node   = Op("+",  2, maxWidth _,  this, b );
  def *(b: Node): Node   = Op("*",  0, sumWidth _,  this, b );
  def ^(b: Node): Node   = Op("^",  2, maxWidth _,  this, b );
  def ?(b: Node): Node   = Multiplex(this, b, null);
  def -(b: Node): Node   = Op("-",  2, maxWidth _,  this, b );
  def ===(b: Node): Node = Op("==", 2, fixWidth(1), this, b );
  def !=(b: Node): Node  = Op("!=", 2, fixWidth(1), this, b );
  def >(b: Node): Node   = Op(">",  2, fixWidth(1), this, b );
  def <(b: Node): Node   = Op("<",  2, fixWidth(1), this, b );
  def <=(b: Node): Node  = Op("<=", 2, fixWidth(1), this, b );
  def >=(b: Node): Node  = Op(">=", 2, fixWidth(1), this, b );
  def &&(b: Node): Node  = Op("&&", 2, fixWidth(1), this, b );
  def ||(b: Node): Node  = Op("||", 2, fixWidth(1), this, b );
  def &(b: Node): Node   = Op("&",  2, maxWidth _, this, b );
  def |(b: Node): Node   = Op("|",  2, maxWidth _, this, b );
  * */
  def ##(b: Node): Node  = Op("##", 2, sumWidth _,  this, b );
  def maxNum: Int = (1 << (if(width < 0) inferWidth(this) else width))-1;
  def minNum: Int = 0;
  def isLit = false;
  def value = -1;
  def signed: this.type = { 
    val res = Wire(){Fix()};
    res <== this.asInstanceOf[Fix];
    res.isSigned = true; 
    res.asInstanceOf[this.type]
  }
  def bitSet(off: UFix, dat: Bits): Bits = { 
    val bit = Bits(1, 1) << off;
    (this.asInstanceOf[Bits] & ~bit) | (dat << off);
  }
  // TODO: MOVE TO WIRE
  def assign(src: Node) = { if (inputs.length > 0) inputs(0) = src; else inputs += src; }
  def <>(src: Node) = { 
    // println("M <>'ing " + this + " & " + src);
    this assign src 
  }
  def ><(src: Node) = {
    src match {
      case b: Bundle =>
        var off = 0;
        for ((n, io) <- b.flatten) {
          if (io.dir == INPUT) {
            io assign NodeExtract(this,off+io.width-1,off);
            off += io.width;
          }
        }
      case n =>
    }
    this
  }
  def ^^(src: Node) = { 
    // println("^^ " + this + " & " + src);
    //this := src 
    println("NODE ^^ " + this.getClass + " " + src);
    src assign this;
  }
  // def apply(bit: Int): Node = { Extract(this, bit) };
  // def apply(hi: Int, lo: Int): Node = { Extract(this, hi, lo) };
  // def apply(bit: Literal): Node = { apply(bit.value) };
  // def apply(hi: Literal, lo: Literal): Node = { apply(hi.value, lo.value) };
  // def apply(bit: Node): Node = { Extract(this, bit); }
  // def apply(hi: Node, lo: Node): Node = { Extract(this, hi, lo) };
  def getLit = this.asInstanceOf[Literal]
  def isIo = false;
  def isReg = false;
  var isRegOut = false;
  def isProbe = false;
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
    // if (name == "") index = component.nextIndex;
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
    if(inferCount > 100) {
      if(genError) {
	    val error = IllegalState("Unable to infer width of " + this, 0);
        if (!ChiselErrors.contains(error))
          ChiselErrors += error
      } else
	    genError = true;
      return false;
    }
    // println("INFER " + this + " -> " + res);
    if(res == -1) {
      inferCount += 1;
      return true
    } else if (res != width) {
      width_ = res;
      inferCount += 1;
      return true;
    } else{
      return false;
    }
  }
  def emitIndex: Int = { if (index == -1) index = componentOf.nextIndex; index }
  def isInObject = isIo || isReg || isUsedByRam || isProbe || (isDebug && named);
  def emitTmp: String = 
    if (isEmittingC) {
      if (isInObject)
        emitRef
      else
        "dat_t<" + width + "> " + emitRef
    } else
      emitRef
  def emitRefVCD: String = emitRef;
  def emitRef: String = if (isEmittingC) emitRefC else emitRefV;
  //def emitRefV: String = if (name == "") "T" + emitIndex else name
  def emitRefV = if(name == "" || !named) "T" + emitIndex else if(!named) name + "_" + emitIndex else name
  // def emitRef: String = "T" + emitIndex;
  def emitDef: String = ""
  def emitReg: String = ""
  def emitWidth: String = "[" + (width-1) + ":0]"
  def emitDec: String = "  wire" + (if (isSigned) " signed " else "") + emitWidth + " " + emitRef + ";\n";
  // C backend
  def emitDecC: String = "  dat_t<" + width + "> " + emitRef + ";\n";
  def emitDefLoC: String = ""
  def emitInitC: String = ""
  def emitDefHiC: String = ""
  def emitRefC: String = emitRefV;
  def depthString(depth: Int): String = {
    var res = "";
    for (i <- 0 until depth)
      res += "  ";
    res
  }
  def visitNode(newDepth: Int): Unit = {
    val comp = componentOf;
    depth = max(depth, newDepth);
    // println("THINKING MOD(" + depth + ") " + comp.name + ": " + this.name);
    if (!comp.isWalked.contains(this)) {
      // println(depthString(depth) + "FiND MODS " + this + " IN " + comp.name);
      comp.isWalked += this;
      this.walked = true;
      for (i <- inputs) {
        if (i != null) {
          i match {
            case d: Delay => 
            case o => {
	      i.visitNode(newDepth+1);
	    }
          }
        }
      }
      comp.omods += this;
    }
  }
  def visitNodeRev(newDepth: Int): Unit = {
    val comp = componentOf;
    depth = max(depth, newDepth);
    if (!comp.isWalked.contains(this)) {
      // println(depthString(depth) + "FiND MODS " + this + " IN " + comp.name);
      comp.isWalked += this;
      for (c <- consumers) {
        if (c != null) {
          c match {
            case d: Delay => 
            case o => c.visitNodeRev(newDepth+1);
          }
        }
      }
      // println("ADDING MOD " + this);
      comp.gmods += this;
    }
  }

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
  def findAssignNode(depth: Int = 5): Node = {
    if (depth < 1) return null;
    this match {
      case assign: Assign[_] => {
        // println("[info] Found Assign Node at depth "+depth);
        return assign;
      }
      case any =>
    }
    if (inputs.length > 0) {
      val in = inputs(0);
      if (!(in == null)) return in.findAssignNode(depth-1);
    }
    null;
  }
  def findNodes(depth: Int, c: Component): Unit = {
    // untraced or same component?
    if(isCellIO) println("found " + this);
    fixName();
    if ((isReg || isRegOut || isClkInput) && !(component == null))
        component.containsReg = true
    val (comp, nextComp, markComp) = 
      this match {
        case io: IO => {
          val ioComp = io.component;
          val nxtComp = if (io.dir == OUTPUT) ioComp else ioComp.parent;
          if (io.dir == OUTPUT && ioComp.parent == null && !(ioComp == topComponent)) {
            ioComp.parent = c;
            c.children += ioComp;
	    if(isEmittingComponents) c.nameChild(ioComp);
            // println("PARENTING " + c.name + "(" + c.children.length + ") CHILD " + ioComp);
          }
          (ioComp,  // if (isEmittingComponents) ioComp else topComponent, 
           nxtComp, // if (isEmittingComponents) nxtComp else topComponent, 
           nxtComp);
        }
        case any => (c, c, c);
      }
    if (comp == null) {
      if (this != reset){
        println("NULL COMPONENT FOR " + this);
      }
    } else if (!comp.isWalked.contains(this)) {
      // println(depthString(depth) + "FiND MODS " + name + " IN " + comp.name);
      // println("FiND MODS(" + depth + ") " + name + " IN " + comp.name);
      comp.isWalked += this;
      var i = 0;
      // if (component == null) 
      //   component = markComp;
      for (node <- inputs) {
        if (node != null) {
	  node.removeCellIOs;
          // println(depthString(depth+1) + "INPUT " + node);
          if (node.component == null) // unmarked input
            node.component = markComp;
          node.findNodes(depth + 2, nextComp);
	  //This code finds a binding for a node
	  //We search for a binding only if it is an output
	  //and the logic's grandfather component is not the same as the 
	  //io's component
	  //and the logic's component is not the same as the output's component unless the logic is an input
          node match { 
            case io: IO => 
	      if (io.dir == OUTPUT && (!(component.parent == io.component) && !(component == io.component && !(this.isInstanceOf[IO] && this.asInstanceOf[IO].dir == INPUT)))) {
		// && !(component == io.component && !this.isInstanceOf[IO])
                val c = node.component.parent;
                // println("BINDING " + node + " I " + i + " NODE-PARENT " + node.component.parent + " -> " + this + " PARENT " + component.parent);
                if (c == null) {
		  println(component + " " + io.component + " ")
                  println("UNKNOWN COMPONENT FOR " + node);
                }
                val b = Binding(node, c, io.component);
                inputs(i) = b;
                if (!c.isWalked.contains(b)) {
                  c.mods += b;  c.isWalked += b;
                  // println("OUTPUT " + io + " BINDING " + inputs(n) + " INPUT " + this);
                }
              }  
            case any => 
          }
        }
        i += 1;
      }
      comp.mods += this;
    }
  }

  def fixName() = {
    if(nameHolder != null && !named){
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
    else if(isCellIO)
      inputs(0).getWidth
    else if(isInstanceOf[Reg] && !isWidthWalked){
      isWidthWalked = true;
      inferWidth(this)
    }else if(inputs.length >= 1 && !isInstanceOf[Reg] && (isInstanceOf[IO] || !isInstanceOf[Wire]))
      inferWidth(this)
    else
      -1
  }
  
  def removeCellIOs() {
    for(i <- 0 until inputs.length)
      if(inputs(i) == null){
        val error = IllegalState("NULL Input for " + this.getClass + " " + this + " in Component " + component, 0);
        if (!ChiselErrors.contains(error))
          ChiselErrors += error
      }
      else if(inputs(i).isCellIO)
	inputs(i) = inputs(i).getNode;
  }
  def getNode(): Node = {
    if(!isCellIO || inputs.length == 0)
      this
    else 
      inputs(0).getNode
  }
  def getCell(): Cell = null;
  def addConsumers(): Boolean = {
    /*
    this match {
      case o: IO => 
        if (o.dir == INPUT) {
          println("ADDING CONSUMERS " + this);
          for (i <- inputs)
            println("  INPUT " + i);
        }
      case any       =>
    }
    */
    var off = 0;
    for (i <- inputs) {
      if (i == null) {
        println(this + " " + inputs + " HAS NULL INPUT " + off + "/" + inputs.length + " IN " + component);
        // TODO: HACK
        inputs = ArrayBuffer(inputs(0));
        return false;
      } else
        i.consumers += this;
      // println("ADDING " + this + " AS CONSUMER OF " + i + " " + i.consumers.length + " CONSUMERS");
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
  def Match(mods: Array[Node]) {
    var off = 0;
    for (m <- mods.reverse) {
      val res = Extract(this.asInstanceOf[Bits], off+m.getWidth-1, off){Bits()};
      m match {
        case r: Reg => r procAssign res;
	case i: Bits => i <== res;
        case o      => o assign res;
      }
      off += m.getWidth;
    }
  }
}

}
