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
  implicit def intToNode(x: Int): Node = Lit(x);
  var isCoercingArgs = true;
  def fixWidth(w: Int) = { (m: Node) => w };
  def widthOf(i: Int) = { (m: Node) => m.inputs(i).width }
  def maxWidth(m: Node): Int = {
    var res = 0;
    for (i <- m.inputs)
      res = max(res, i.width);
    res
  }
  def sumWidth(m: Node): Int = {
    var res = 0;
    for (i <- m.inputs)
      res = res + i.width;
    res
  }
  def lshWidthOf(i: Int, n: Node) = { (m: Node) => m.inputs(i).width + n.maxNum }
  def rshWidthOf(i: Int, n: Node) = { (m: Node) => m.inputs(i).width - n.minNum }
  var reset: Node = Input("reset", 1);
  var resets = Queue[Node]();
  var clk: Node = Input("clk", 1);
  def pushReset(r: Node) { resets.enqueue(reset); reset = r }
  def popReset() { reset = resets.dequeue() }
  def withReset(r: Node)(block: => Node) = {
    val resBak = reset; reset = r; 
    val res = block; 
    reset = resBak;
    res
  }
  def ListLookup (addr: Node, default: List[Node], mapping: Array[(Lit, List[Node])]): List[Node] = {
    val ll = new ListLookup(mapping, default);
    ll.init("", widthOf(1), addr); 
    for (w <- ll.wires)
      w.lookup = ll;
    for (e <- default) 
      ll.inputs += e;
    for ((addr, data) <- mapping)
      for (e <- data)
        ll.inputs += e;
    ll.wires
  }

  
}

abstract class Node {
  var component: Component = null;
  var depth = 0;
  def componentOf: Component = if (isEmittingComponents) component else topComponent
  var isSigned = false;
  var width_ = -1;
  var index = -1;
  var isFixedWidth = false;
  var name: String = "";
  var consumers = new ArrayBuffer[Node]; // mods that consume one of my outputs
  var inputs = new ArrayBuffer[Node];
  var outputs = new ArrayBuffer[Node];
  var inferWidth: (Node) => Int = maxWidth;
  def width: Int = width_;
  def width_=(w: Int) = { isFixedWidth = true; width_ = width; inferWidth = fixWidth(w); }
  def name_it (path: String) = { name = path; }
  def unary_-(): Node    = Op("-",  1, widthOf(0), this);
  def unary_~(): Node    = Op("~",  1, widthOf(0), this);
  def unary_!(): Node    = Op("!",  1, fixWidth(1), this);
  def <<(b: Node): Node  = Op("<<", 0, lshWidthOf(0, b),  this, b );
  def >>(b: Node): Node  = Op(">>", 0, rshWidthOf(0, b),  this, b );
  def >>>(b: Node): Node = Op(">>", 0, rshWidthOf(0, b),  this, b );
  def +(b: Node): Node   = Op("+",  2, maxWidth _,  this, b );
  def *(b: Node): Node   = Op("*",  0, sumWidth _,  this, b );
  def ^(b: Node): Node   = Op("^",  2, maxWidth _,  this, b );
  def ?(b: Node): Node   = Mux(this, b, null);
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
  def maxNum: Int = (1 << width)-1;
  def minNum: Int = 0;
  def isLit = false;
  def value = -1;
  def signed: Node = { 
    val res = Wire();
    res := this;
    res.isSigned = true; 
    res
  }
  def bitSet(off: Node, dat: Node): Node = { 
    val bit = Lit(1, 1) << off;
    (this & ~bit) | (dat << off);
  }
  // TODO: MOVE TO WIRE
  def :=(src: Node) = { if (inputs.length > 0) inputs(0) = src; else inputs += src; }
  def <>(src: Node) = { 
    // println("M <>'ing " + this + " & " + src);
    this := src 
  }
  def ><(src: Node) = {
    src match {
      case b: Bundle =>
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
    this := src 
  }
  def apply(bit: Int): Node = { Bits(this, bit) };
  def apply(hi: Int, lo: Int): Node = { Bits(this, hi, lo) };
  def apply(bit: Lit): Node = { apply(bit.value) };
  def apply(hi: Lit, lo: Lit): Node = { apply(hi.value, lo.value) };
  def apply(bit: Node): Node = { Bits(this, bit); }
  def apply(hi: Node, lo: Node): Node = { Bits(this, hi, lo) };
  def isIo = false;
  def isReg = false;
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
  def emitRefV: String = if (name == "") "T" + emitIndex else name
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
            case o => i.visitNode(newDepth+1);
          }
        }
      }
      // println("ADDING MOD " + this.name);
      if (this != reset)
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
        println(this + " HAS NULL INPUT " + off + "/" + inputs.length + " IN " + component);
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
  def extract (widths: Array[Int]): List[Node] = {
    var res: List[Node] = Nil;
    var off = 0;
    for (w <- widths) {
      res  = this(off+w-1, off) :: res;
      off += w.value;
    }
    res.reverse
  }
  def extract (b: Bundle): List[Node] = {
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
      val res = this(off+m.width-1, off);
      m match {
        case r: Reg => r <== res;
        case o      => o := res;
      }
      off += m.width;
    }
  }
}

}
