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
          val error = ChiselError(m + " in " + m.component + " is unconnected. Ensure that is assigned.", m)
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
  //var reset: Fix = Fix(1, INPUT);//Input("reset", 1);
  //reset.setName("reset");
  //var resets = Queue[Fix]();

  var clk: Node = Bits(1, INPUT);//Input("clk", 1);
  clk.setName("clk");

  /*
  def pushReset(r: Fix) { resets.enqueue(reset); reset = r }
  def popReset() { reset = resets.dequeue() }
  def withReset(r: Fix)(block: => Node) = {
    val resBak = reset; reset = r; 
    val res = block; 
    reset = resBak;
    res
  }
  * */
  // TODO: WHY IS THIS HERE?
/*
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
      val res = UFix(OUTPUT);
      res.setIsCellIO;
      x.nameHolder = res;
      res assign x;
      res
    })
  }

* */

  var stop = true;
  
}

abstract class Node extends nameable{
  var sccIndex = -1
  var sccLowlink = -1
  var walked = false;
  var staticComp: Component = getComponent();
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
  def traceableNodes = Array[Node]();
  var outputs = new ArrayBuffer[Node];
  var inferWidth: (Node) => Int = maxWidth;
  var nameHolder: nameable = null;
  var isClkInput = false;
  var inferCount = 0;
  var genError = false;
  var stack: Array[StackTraceElement] = null;
  var line: Array[StackTraceElement] = Thread.currentThread().getStackTrace
  
  def isByValue: Boolean = true;
  def isTerminator = false;
  def isTerminated = (inputs.size == 1 && inputs(0).isTerminator);
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
    else if(inputs.length == 1 && (isCellIO || this.isInstanceOf[Wire]))
      inputs(0).litOf
    else
      null
  }
  def value = BigInt(-1);
  def signed: this.type = { 
    val res = Wire(){Fix()};
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
    if (!src.isTerminated) {
      if (inputs.length > 0) 
        inputs(0) = src; 
      else 
        inputs += src; 
    }
  }
  def <>(src: Node) = { 
    // println("M <>'ing " + this + " & " + src);
    this assign src 
  }
  def ^^(src: Node) = { 
    // println("^^ " + this + " & " + src);
    //this := src 
    println("NODE ^^ " + this.getClass + " " + src);
    src assign this;
  }
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
    if(inferCount > 1000000) {
      if(genError) {
	val error = ChiselError("Unable to infer width of " + this, this);
        if (!ChiselErrors.contains(error))
          ChiselErrors += error
      } else
	genError = true;
      return false;
    }
    // println("INFER " + this + " -> " + res);
    if(res == -1) {
      //inferCount += 1;
      return true
    } else if (res != width) {
      width_ = res;
      //inferCount += 1;
      return true;
    } else{
      return false;
    }
  }
  def emitIndex: Int = { if (index == -1) index = componentOf.nextIndex; index }
  // TODO: SUBCLASS FROM SOMETHING INSTEAD OR OVERRIDE METHOD
  // TODO: RENAME METHOD TO ISVOLATILE
  def isInObject = 
    (isIo && (isIoDebug || component == topComponent)) || 
    (topComponent.debugs.contains(this) && named) || 
    isReg || isUsedByRam || isProbe || (isDebug && named);
  def isInVCD = isIo || isReg || isProbe || (isDebug && named);
  def emitTmp: String = 
    if (backendName == "c") {
      if (isInObject)
        emitRef
      else
        "dat_t<" + width + "> " + emitRef
    } else
      emitRef
  def emitRefVCD: String = emitRef;
  def emitRef: String = if (backendName == "c") emitRefC else emitRefV;
  //def emitRefV: String = if (name == "") "T" + emitIndex else name
  def emitRefV = if(name == "" || !named) "T" + emitIndex else if(!named) name + "_" + emitIndex else name
  def dotName = { val name = this.getClass.getName; name.substring(7, name.size) };
  def emitRefDot: String = emitRef;
  // def emitRef: String = "T" + emitIndex;
  def emitDef: String = ""
  def emitReg: String = ""
  def emitWidth: String = if(width == 1) "" else "[" + (width-1) + ":0]"
  def emitDec: String = "  wire" + (if (isSigned) " signed " else "") + emitWidth + " " + emitRef + ";\n";
  // C backend
  def emitDecVCD: String = if (isVCD && !isLit) "  dat_t<" + width + "> " +emitRef + "__prev" + ";\n" else "";
  def emitDecC: String = "  dat_t<" + width + "> " + emitRef + ";\n";
  def emitDefLoC: String = ""
  def emitInitC: String = ""
  def emitDefHiC: String = ""
  def emitInitHiC: String = ""
  def emitDefVCD(vcdname: String) = {
    "  if (t == 0 || (" + emitRef + " != " + emitRef + "__prev).to_bool())\n" +
    "    dat_dump(f, " + emitRef + ", \"" + vcdname + "\");\n" +
    "  " + emitRef + "__prev = " + emitRef + ";\n"
  }
  def emitRefC: String = emitRefV;
  def depthString(depth: Int): String = {
    var res = "";
    for (i <- 0 until depth)
      res += "  ";
    res
  }

  /*
  def visitNode(newDepth: Int, stack: Stack[(Int, Node)]): Unit = {
    val comp = componentOf;
    // println("VISIT NODE(" + newDepth + ") " + comp.name + ": " + this.name);
    if (newDepth == -1) 
      comp.omods += this;
    else {
      depth = max(depth, newDepth);
      //println("THINKING MOD(" + depth + ") " + comp.name + ": " + this.name);
      if (!comp.isWalked.contains(this)) {
        //println(depthString(depth) + "FiND MODS " + this + " IN " + comp.name);
        comp.isWalked += this;
        this.walked = true;
        stack.push((-1, this));
        for (i <- inputs) {
          if (i != null) {
            i match {
              case m: MemRef[ _ ] => if(!m.isReg) stack.push((newDepth+1, i));
              case d: Delay       => 
              case o              => stack.push((newDepth+1, o)); 
            }
          }
        }
        // println("VISITING MOD " + this + " DEPTH " + depth);
      }
    }
  }

  def visitNodeRev(newDepth: Int, stack: Stack[(Int, Node)]): Unit = {
    val comp = componentOf;
    if (newDepth == -1)
      comp.gmods += this;
    else {
      depth = max(depth, newDepth);
      if (!comp.isWalked.contains(this)) {
        // println(depthString(depth) + "FiND MODS " + this + " IN " + comp.name);
        comp.isWalked += this;
        stack.push((-1, this));
        for (c <- consumers) {
          if (c != null) {
            c match {
              case m: MemRef[ _ ] => if(!m.isReg) stack.push((newDepth+1, m));
              case d: Delay       => 
              case o              => stack.push((newDepth+1, o));
            }
          }
        }
      }
    }
  }
  */

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

  def traceNode(c: Component, stack: Stack[() => Any]): Any = {
    if(this.isCellIO) println("found")
    // determine whether or not the component needs a clock input
    if ((isReg || isRegOut || isClkInput) && !(component == null))
        component.containsReg = true

    // pushes and pops components as necessary in order to later mark the parent of nodes
    val (comp, nextComp) = 
      this match {
        case io: IO => {
          //assert(io.dir == OUTPUT || io.dir == INPUT, 
                 //{println(" IO w/o direction " + io + " name: " + io.name + " " + io.inputs + " in comp: " + c + " of class: " + io.getClass + " on line " + findFirstUserLine(io.line))})
          (io.component, if (io.dir == OUTPUT) io.component else io.component.parent);
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
	    // This code finds a binding for a node. We search for a binding only if it is an output
	    // and the logic's grandfather component is not the same as the io's component and
	    // the logic's component is not same as output's component unless the logic is an input
            n match { 
              case io: IO => 
                if (io.dir == OUTPUT && !io.isCellIO &&
                    (!(component.parent == io.component) && 
                     !(component == io.component && 
                       !(this.isInstanceOf[IO] && this.asInstanceOf[IO].dir == INPUT)))) {
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
                           ((!this.isInstanceOf[IO] && this.component == io.component.parent) || 
                            (this.isInstanceOf[IO] && this.asInstanceOf[IO].dir == OUTPUT && 
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
    else if(isCellIO) {
      if(inputs.length == 0) -1 else inputs(0).getWidth
    } else if(isInstanceOf[Reg] && !isWidthWalked){
      isWidthWalked = true;
      inferWidth(this)
    }else if(inputs.length >= 1 && !isInstanceOf[Reg] && (isInstanceOf[IO] || !isInstanceOf[Wire]))
      inferWidth(this)
    else
      -1
  }
  
  def removeCellIOs() {
    for(i <- 0 until inputs.length) {
      if(inputs(i) == null){
        val error = ChiselError("NULL Input for " + this.getClass + " " + this + " in Component " + component, this);
        if (!ChiselErrors.contains(error))
          ChiselErrors += error
      }
      else if(inputs(i).isCellIO) {
	inputs(i) = inputs(i).getNode;
      }
    }
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
      } else if(!i.consumers.contains(this))
        i.consumers += this;
      // println("ADDING " + this + " AS CONSUMER OF " + i + " " + i.consumers.length + " CONSUMERS");
      off += 1;
    }
    true;
  }
  // TODO: SUPERCEDED BY toBits with bundle
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
  // TODO: SUPERCEDED BY fromBits with bundle
  def Match(mods: Array[Node]) {
    var off = 0;
    for (m <- mods.reverse) {
      val res = Extract(this.asInstanceOf[Bits], off+m.getWidth-1, off){Bits()};
      m match {
        case r: Reg  => r procAssign res;
	case i: Bits => i := res;
        case o       => o assign res;
      }
      off += m.getWidth;
    }
  }
}

}
