// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack

import scala.math.max;
import Node._;
import Component._;
import IOdir._;

object Node {
  var cond = new Stack[Node];
  var ruleStack = new Stack[Rule];
  var isCoercingArgs = true;
  def fixWidth(w: Int) = { (m: Node) => w };
  def widthOf(i: Int) = { (m: Node) => {if(m.inputs(i).width < 0) m.inputs(i).inferWidth(m.inputs(i)) else m.inputs(i).width }}
  def maxWidth(m: Node): Int = {
    var res = 0;
    for (i <- m.inputs)
      res = max(res, i.width);
    res
  }
  def maxWidthPlusOne(m: Node): Int = maxWidth(m) + 1;
  def sumWidth(m: Node): Int = {
    var res = 0;
    for (i <- m.inputs)
      res = res + i.width;
    res
  }
  def lshWidthOf(i: Int, n: Node) = { 
    (m: Node) => {
      val mwidth = m.inputs(i).width;
      val nMax = n.maxNum;
      val res = m.inputs(i).width + n.maxNum;
      res
    } 
  }
  def rshWidthOf(i: Int, n: Node) = { (m: Node) => m.inputs(i).width - n.minNum }
  var reset: int_t = Input("reset", 1);
  var resets = Queue[int_t]();
  var clk: Node = Input("clk", 1);
  def pushReset(r: int_t) { resets.enqueue(reset); reset = r }
  def popReset() { reset = resets.dequeue() }
  def withReset(r: int_t)(block: => Node) = {
    val resBak = reset; reset = r; 
    val res = block; 
    reset = resBak;
    res
  }
  def ListLookup(addr: Node, default: List[Node], mapping: Array[(Node, List[Node])]): List[int_t] = {
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
      val res = int_t(OUTPUT);
      res.setIsCellIO;
      x.nameHolder = res;
      res := x;
      res
    })
  }
  
  var stop = true;
  
}

abstract class Node extends nameable{
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
  def width: Int = width_;
  def width_=(w: Int) = { isFixedWidth = true; width_ = width; inferWidth = fixWidth(w); }
  def name_it (path: String, setNamed: Boolean = true) = { name = path; named = setNamed}
  def unary_-(): Node    = Op("-",  1, widthOf(0), this);
  def unary_~(): Node    = Op("~",  1, widthOf(0), this);
  def unary_!(): Node    = Op("!",  1, fixWidth(1), this);
  def <<(b: Node): Node  = Op("<<", 0, lshWidthOf(0, b),  this, b );
  def >>(b: Node): Node  = Op(">>", 0, rshWidthOf(0, b),  this, b );
  def >>>(b: Node): Node = Op(">>", 0, rshWidthOf(0, b),  this, b );
  def +(b: Node): Node   = Op("+",  2, maxWidth _,  this, b );
  def *(b: Node): Node   = Op("*",  0, sumWidth _,  this, b );
  def ^(b: Node): Node   = Op("^",  2, maxWidth _,  this, b );
  def ?(b: Node): Node   = Multiplex(this, b, null);
  def -(b: Node): Node   = Op("-",  2, maxWidth _,  this, b );
  def ##(b: Node): Node  = Op("##", 2, sumWidth _,  this, b );
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
  def maxNum: Int = (1 << (if(width < 0) inferWidth(this) else width))-1;
  def minNum: Int = 0;
  def isLit = false;
  def value = -1;
  def signed: this.type = { 
    val res = Junction[int_t]();
    res <== this;
    res.isSigned = true; 
    res.asInstanceOf[this.type]
  }
  def bitSet(off: int_t, dat: int_t): int_t = { 
    val bit = Lit(1, 1) << off;
    (this.asInstanceOf[int_t] & ~bit) | (dat << off);
  }
  // TODO: MOVE TO WIRE
  def :=(src: Node) = { if (inputs.length > 0) inputs(0) = src; else inputs += src; }
  def <>(src: Node) = { 
    // println("M <>'ing " + this + " & " + src);
    this := src 
  }
  def ><(src: Node) = {
    src match {
      case b: bundle_t =>
        var off = 0;
        for ((n, io) <- b.flatten) {
          if (io.dir == INPUT) {
            io := this(off+io.width-1,off);
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
    src := this;
  }
  def apply(bit: Int): Node = { Bits(this, bit) };
  def apply(hi: Int, lo: Int): Node = { Bits(this, hi, lo) };
  def apply(bit: Literal): Node = { apply(bit.value) };
  def apply(hi: Literal, lo: Literal): Node = { apply(hi.value, lo.value) };
  def apply(bit: Node): Node = { Bits(this, bit); }
  def apply(hi: Node, lo: Node): Node = { Bits(this, hi, lo) };
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
    // println("INFER " + this + " -> " + res);
    if (res != width) {
      width_ = res;
      return true;
    } else
      return false;
  }
  def emitIndex: Int = { if (index == -1) index = componentOf.nextIndex; index }
  def isInObject = isIo || isReg || isUsedByRam || isProbe;
  def emitTmp: String = 
    if (isEmittingC) {
      if (isInObject)
        emitRef
      else
        "dat_t<" + width + "> " + emitRef
    } else
      emitRef
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
  def findNodes(depth: Int, c: Component): Unit = {
    // untraced or same component?
    if(isCellIO) println("found");
    fixName();
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
      if (this != reset)
        println("NULL COMPONENT FOR " + this);
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
          node match { 
            case io: IO => 
              if (io.dir == OUTPUT && !(component == io.component)) {
                val c = node.component.parent;
                // println("BINDING " + node + " I " + i + " NODE-PARENT " + node.component.parent + " -> " + this + " PARENT " + component.parent);
                if (c == null) {
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

  def getWidth(): Int = {
    if(width > 0)
      width
    else if(inputs.length > 1)
      inferWidth(this)
    else if(inputs.length == 1)
      inputs(0).getWidth
    else
      -1
  }

  def removeCellIOs() {
    for(i <- 0 until inputs.length)
      inputs(i) = inputs(i).getNode;
  }
  def getNode(): Node = {
    if(inputs.length == 0 || !isCellIO)
      this
    else
      inputs(0).getNode
  }
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
  def extract (widths: Array[Int]): List[int_t] = {
    var res: List[int_t] = Nil;
    var off = 0;
    for (w <- widths) {
      res  = this.asInstanceOf[int_t](off+w-1, off) :: res;
      off += w;
    }
    res.reverse
  }
  def extract (b: bundle_t): List[Node] = {
    var res: List[Node] = Nil;
    var off = 0;
    for ((n, io) <- b.flatten) {
      if (io.dir == OUTPUT) {
        val w = io.width;
        res  = this(off+w-1, off) :: res;
        off += w;
      }
    }
    res.reverse
  }
  def Match(mods: Array[Node]) {
    var off = 0;
    for (m <- mods.reverse) {
      val res = Bits(this, off+m.getWidth-1, off);
      m match {
        case r: Reg => r <== res;
	case i: int_t => i <== res;
        case o      => o := res;
      }
      off += m.getWidth;
    }
  }
}

}
