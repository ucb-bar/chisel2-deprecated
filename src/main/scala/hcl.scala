// author: jonathan bachrach

package Chisel {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import java.lang.reflect.Modifier._;

import scala.math.log;
import scala.math.abs;
import scala.math.ceil;
import scala.math.max;
import scala.math.min;
import Node._;
import Wire._;
import Lit._;
import Op._;
import Reg._;
import Component._;
import Bundle._;
import IOdir._;
import IO._;

object Node {
  var cond = new Stack[Node];
  implicit def intToNode(x: Int): Node = Lit(x);
  var compStack = new Stack[Component]();
  var currentComp: Component = null;
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
  def when(c: Node)(block: => Unit) = {
    cond.push(c); 
    // println("WHEN " + c + " {");
    block; 
    cond.pop();
    // println("} ");
  }
  def ifelse(c: Node, con_block: () => Unit, alt_block: () => Unit) = {
    val tt = c;
    cond.push(tt);  
    // println("  IF " + tt + " {");
    con_block(); 
    // println("  }");
    cond.pop();
    val et = !c;
    cond.push(et); 
    // println("  ELSE IF " + et + " {");
    alt_block(); 
    cond.pop();
    // println("  }");
  }
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
  def pushComponent(c: Component) = {
    compStack.push(c);
    currentComp = c;
    c.name_it();
    c.ownIo();
  }
  def popComponent(c: Component) = {
    val parent = if (compStack.length > 1) compStack(1); else null;
    if (!(parent == null) && c.parent == null) {
      c.parent = parent;
      parent.children += c;
    }
    currentComp = parent;
  }
  def withComponent(c: Component, block: Component => Unit) = {
    pushComponent(c);
    block(c);
    popComponent(c);
  }
  def withSimpleComponent(block: Component => Node) = {
    withComponent(Component("MAIN"), c => {
      val res = block(c);
      c.ioVal = Output("res", res);
    });
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
  def unary_~(): Node = Op("~", widthOf(0), this);
  def unary_!(): Node = Op("!", fixWidth(1), this);
  def <<(b: Node): Node = Op("<<",   maxWidth _,  this, b );
  def >>(b: Node): Node = Op(">>",   maxWidth _,  this, b );
  def >>>(b: Node): Node = Op(">>", maxWidth _,  this, b );
  def +(b: Node): Node = Op("+",   maxWidth _,  this, b );
  def ^(b: Node): Node = Op("^",   maxWidth _,  this, b );
  def ?(b: Node): Node = Mux(this, b, null);
  def -(b: Node): Node = Op("-",   maxWidth _,  this, b );
  def >(b: Node): Node = Op(">",   fixWidth(1), this, b );
  def ===(b: Node): Node = Op("==", fixWidth(1), this, b );
  def !=(b: Node): Node = Op("!=", fixWidth(1), this, b );
  def <=(b: Node): Node = Op("<=", fixWidth(1), this, b );
  def <(b: Node): Node = Op("<", fixWidth(1), this, b );
  def &&(b: Node): Node = Op("&&", fixWidth(1), this, b );
  def ||(b: Node): Node = Op("||", fixWidth(1), this, b );
  def &(b: Node): Node = Op("&", maxWidth _, this, b );
  def |(b: Node): Node = Op("|", maxWidth _, this, b );
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
  def findBinding(): Binding = 
    componentOf.findBinding(this);
  def binding(): Node = {
    if (isEmittingComponents) {
      val b = findBinding();
      if (b == null) {
        println("// UNABLE TO FIND BINDING FOR " + this);
        this
      } else
        b
    } else
      this
  }
  def initOf (n: String, width: (Node) => Int, ins: List[Node]): Node = { 
    component = currentComp;
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
  def emitTmp: String = emitRef;
  def emitRef: String = if (isEmittingC) emitRefC else emitRefV;
  def emitRefV: String = if (name == "") "T" + emitIndex else name;
  // def emitRef: String = "T" + emitIndex;
  def emitDef: String = ""
  def emitReg: String = ""
  def emitWidth: String = "[" + (width-1) + ":0]"
  def emitDec: String = "  wire" + (if (isSigned) " signed " else "") + emitWidth + " " + emitRef + ";\n";
  // C backend
  def emitDecC: String = "  dat_t<" + width + "> " + emitRef + ";\n";
  def emitDefLoC: String = ""
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
      // println("ADDING MOD " + this);
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
  def findNodes(depth: Int): Unit = {
    // untraced or same component?
    val comp = componentOf;
    if (comp == null) {
      if (name != "reset")
        println("NULL COMPONENT FOR " + this);
    } else if (!comp.isWalked.contains(this)) {
      // println(depthString(depth) + "FiND MODS " + this + " IN " + comp.name);
      comp.isWalked += this;
      var i = 0;
      for (node <- inputs) {
        if (node != null) {
          // println(depthString(depth+1) + "INPUT " + node);
          if (node.component == null) // unmarked input
            node.component = component;
          node.findNodes(depth + 2);
          node match { 
            case io: IO => 
              if (io.dir == OUTPUT) {
                val c = node.component.parent;
                // println("BINDING " + node + " I " + i + " NODE-PARENT " + node.component.parent + " -> " + this + " PARENT " + component.parent);
                val b = Binding(node, c);
                inputs(i) = b;
                c.mods += b;  c.isWalked += b;
                // println("OUTPUT " + io + " BINDING " + inputs(n) + " INPUT " + this);
              }
            case any => 
          }
        }
        i += 1;
      }
      // println("ADDING MOD " + this);
      comp.mods += this;
      // this match {
      //   case o: Output => ;
      //   case _ => 
      // }
    }
    // else if (component != null) {
    // if (!c.children.contains(component)) {
    //   println("ADDING CHILD " + component.name + " TO " + c.name);
    //   c.children += component;
    // }
    // }
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

object Component {
  var compIndex = -1;
  var isEmittingComponents = false;
  var isEmittingC = false;
  var topComponent: Component = null;
  def nextCompIndex : Int = { compIndex = compIndex + 1; compIndex }
  def apply (name: String, i: Interface): Component = {
    val res = new Component();
    res.name = name + "_" + nextCompIndex;
    res.ioVal = i;
    res.parent = currentComp;
    if (res.parent != null)
      res.parent.children += res;
    val wires = res.io.flatten;
    for ((n, w) <- wires) {
      w.component = res;
      w match {
        case io: IO => 
          if (io.dir == INPUT)
            res.inputs  += io; 
          else
            res.outputs += io; 
      };
    }
    res
  }
  def apply (name: String): Component = Component(name, nullBundle);
  def chisel_main(args: Array[String], gen: () => Component) = {
    val isEmitV = args.length > 0 && args(0) == "--v";
    isEmittingComponents = isEmitV;
    println("EMITTING COMPONENTS = " + isEmittingComponents);
    val c = gen();
    if (isEmitV)
      c.compileV();
    else
      c.compileC();
  }
}
class Component extends Node {
  var ioVal: Interface = null;
  var bindings = new ArrayBuffer[Binding];
  var wiresCache: Array[(String, IO)] = null;
  var parent: Component = null;
  var children = new ArrayBuffer[Component];

  var mods  = new ArrayBuffer[Node];
  var omods = new ArrayBuffer[Node];
  var gmods = new ArrayBuffer[Node];
  var regs = new ArrayBuffer[Node];
  var nexts = new Queue[Node];
  var nindex = -1;
  var defaultWidth = 32;
  def ownIo() = {
    val wires = io.flatten;
    for ((n, w) <- wires) {
      // println(">>> " + w + " IN " + this);
      w.component = this;
    }
  }
  def name_it() = {
    if (name == "") { 
      compIndex += 1;
      val cname  = getClass().getName(); 
      val dotPos = cname.lastIndexOf('.');
      name = if (dotPos >= 0) cname.substring(dotPos+1) else cname;
      name = name + "_" + compIndex;
    }
  }
  def findBinding(m: Node): Binding = {
    // println("FINDING BINDING " + m + " OUT OF " + bindings.length);
    for (b <- bindings) {
      // println("LOOKING AT " + b);
      if (b.inputs(0) == m)
        return b
    }
    // println("UNABLE TO FIND BINDING FOR " + m);
    return null
  }
  def io: Interface = ioVal;
  def nextIndex : Int = { nindex = nindex + 1; nindex }
  def genName (name: String): String = 
    if (name == null || name.length() == 0) "" else name + "_" + nextIndex;
  var isWalking = new HashSet[Node];
  var isWalked = new HashSet[Node];
  override def toString: String = name
  def wires: Array[(String, IO)] = {
    if (wiresCache == null)
      wiresCache = io.flatten;
    wiresCache
  }
  def <>(src: Component) = io <> src.io;
  def apply(name: String): Interface = io(name);
  // COMPILATION OF REFERENCE
  override def emitDec: String = {
    var res = "";
    val wires = io.flatten;
    for ((n, w) <- wires) 
      res += w.emitDec;
    res
  }
  override def emitRefV: String = name;
  override def emitDef: String = {
    var res = "  " + emitRef + " " + emitRef + "(.clk(clk), .reset(reset)";
    var isFirst = true;
    for ((n, w) <- wires) {
      res += ",\n       ." + n + "( ";
      w match {
        case io: IO  => 
          if (io.dir == INPUT) {
            if (io.inputs.length == 0)
              println("// " + io + " UNCONNECTED IN " + io.component); 
            else if (io.inputs.length > 1) 
              println("// " + io + " CONNECTED TOO MUCH " + io.inputs.length); 
            else 
              res += io.inputs(0).emitRef;
          } else {
            if (io.consumers.length == 0) 
              println("// " + io + " UNCONNECTED IN " + io.component + " BINDING " + findBinding(io)); 
            else {
              var consumer: Node = parent.findBinding(io);
              if (consumer == null) 
                println("// " + io + "(" + io.component + ") OUTPUT UNCONNECTED (" + io.consumers.length + ") IN " + parent); 
              else 
                res += consumer.emitRef; // TODO: FIX THIS?
            }
          }
      };
      res += " )";
    }
    res += ");\n";
    res
  }
  override def emitDefLoC: String = {
    var res = "";
    for ((n, w) <- wires) {
      w match {
        case io: IO  => 
          if (io.dir == INPUT)
            res += "    " + emitRef + "->" + n + " = " + io.inputs(0).emitRef + ";\n";
      };
    }
    res += emitRef + "->clock_lo(reset);\n";
    for ((n, w) <- wires) {
      w match {
        case io: IO => 
          if (io.dir == OUTPUT)
            res += "    " + io.consumers(0).emitRef + " = " + emitRef + "->" + n + ";\n";
      };
    }
    res
  }
  override def emitDefHiC: String = {
    var res = emitRef + "->clock_hi(reset);\n";
    res
  }
  // COMPILATION OF BODY
  def emitDefs: String = {
    var res = "";
    for (m <- mods) 
      res += m.emitDef;
    for (c <- children) 
      res += c.emitDef;
    res
  }
  def emitRegs: String = {
    var res = "  always @(posedge clk) begin\n";
    for (m <- mods) 
      res += m.emitReg;
    res += "  end\n";
    res
  }
  def emitDecs: String = {
    var res = "";
    for (m <- mods) 
      res += m.emitDec;
    res
  }
  def isInferenceTerminal(m: Node): Boolean = {
    m.isFixedWidth || (
      m match { 
        case io: IO => true; 
        case b: Binding => true; 
        case _ => false }
    )
    /*
    var isAllKnown = true;
    for (i <- m.inputs) {
      if (i.width == -1)
        isAllKnown = false;
    }
    isAllKnown
    */
  }
  def initInference() = {
    for (m <- mods) {
      // if (isInferenceTerminal(m)) {
        // println("ENQUEUE " + m);
      // println("INIT " + m);
        nexts.enqueue(m);
      // }
    }
  }
  def inferAll() = {
    initInference;
    while (!nexts.isEmpty) {
      val next = nexts.dequeue();
      if (next.infer) {
        nexts.enqueue(next);
        for (c <- next.consumers) {
          // println("ENQUEUING " + c);
          nexts.enqueue(c);
        }
        for (i <- next.inputs)
          nexts.enqueue(i);
      }
    }
  }
  def findConsumers() = {
    for (m <- mods) {
      m.addConsumers;
    }
  }
  def findRoots(): ArrayBuffer[Node] = {
    val roots = new ArrayBuffer[Node];
    for (m <- mods) {
      m match {
        case io: IO => if (io.dir == OUTPUT) { if (io.consumers.length == 0) roots += m; }
        case d: Delay  => roots += m;
        case any       =>
      }
    }
    roots
  }
  def findLeaves(): ArrayBuffer[Node] = {
    val leaves = new ArrayBuffer[Node];
    for (m <- mods) {
      m match {
        case io: IO    => if (io.dir == INPUT) { if (io.inputs.length == 0) leaves += m; }
        case l: Lit    => leaves += m;
        case d: Delay  => leaves += m;
        case any       =>
      }
    }
    leaves
  }
  def findOrdering() = {
    val roots = findRoots();
    isWalked.clear();
    for (r <- roots)
      r.visitNode(0);
  }
  def findGraph() = {
    val leaves = findLeaves();
    isWalked.clear();
    for (r <- leaves)
      r.visitNodeRev(0);
  }
  def findGraphDims(): (Int, Int, Int) = {
    var maxDepth = 0;
    val imods = new ArrayBuffer[Node]();
    for (m <- mods) {
      m match {
        case o: IO  =>
        case l: Lit =>
        case i      => imods += m;
      }
    }
    val whist = new HashMap[Int, Int]();
    for (m <- imods) {
      val w = m.width;
      if (whist.contains(w))
        whist(w) = whist(w) + 1;
      else
        whist(w) = 1;
    }
    val hist = new HashMap[String, Int]();
    for (m <- imods) {
      var name = m.getClass().getName();
      m match {
        case m: Mux => name = "Mux";
        case op: Op => name = op.op;
        case o      => name = name.substring(name.indexOf('.')+1);
      }
      if (hist.contains(name))
        hist(name) = hist(name) + 1;
      else
        hist(name) = 1;
    }
    for (m <- imods) 
      maxDepth = max(m.depth, maxDepth);
    // for ((n, c) <- hist) 
    println("%6s: %s".format("name", "count"));
    for (n <- hist.keys.toList.sortWith((a, b) => a < b)) 
      println("%6s: %4d".format(n, hist(n)));
    println("%6s: %s".format("width", "count"));
    for (w <- whist.keys.toList.sortWith((a, b) => a < b)) 
      println("%3d: %4d".format(w, whist(w)));
    var widths = new Array[Int](maxDepth+1);
    for (i <- 0 until maxDepth+1)
      widths(i) = 0;
    for (m <- imods) 
      widths(m.depth) = widths(m.depth) + 1;
    var numNodes = 0;
    for (m <- imods) 
      numNodes += 1;
    var maxWidth = 0;
    for (i <- 0 until maxDepth+1)
      maxWidth = max(maxWidth, widths(i));
    (numNodes, maxWidth, maxDepth)
  }
  def collectNodes() = {
    for (m <- mods) {
      m match {
        case io: IO  => 
          if (io.dir == INPUT) 
            inputs += m;
          else
            outputs += m;
        case r: Reg    => regs += m;
        case other     =>
      }
    }
  }
  override def findNodes(depth: Int): Unit = {
    io.findNodes(depth);
  }
  def doCompileV(out: java.io.FileWriter, depth: Int): Unit = {
    name_it();
    io.name_it("");
    // println("COMPILING COMP " + name);
    println("// " + depthString(depth) + "COMPILING " + this + " " + children.length + " CHILDREN");
    if (isEmittingComponents) {
      for (top <- children)
        top.doCompileV(out, depth+1);
    } else
      topComponent = this;
    // isWalked.clear();
    findNodes(0);
    findConsumers();
    inferAll();
    collectNodes();
    // for (m <- mods) {
    //   println("// " + depthString(depth+1) + " MOD " + m);
    // }
    out.write("module " + name + "(input clk, input reset");
    for ((n, w) <- wires) {
      w match {
        case io: IO => {
          if (io.dir == INPUT) {
            out.write(",\n    input " + io.emitWidth + " " + io.emitRef);
          } else {
            out.write(",\n    output" + io.emitWidth + " " + io.emitRef);
          }
        }
      };
    }
    out.write(");\n\n");
    out.write(emitDecs)
    out.write("\n");
    out.write(emitDefs)
    // for (o <- outputs)
    //   out.writeln("  assign " + o.emitRef + " = " + o.inputs(0).emitRef + ";");
    if (regs.size > 0) {
      out.write("\n");
      out.write(emitRegs)
    }
    out.write("endmodule\n\n");
    // println("// " + depthString(depth) + "DONE");
  }
  def markComponents(the_parent: Component): Unit = {
    name_it();
    ownIo();
    if (the_parent != null) {
      parent = the_parent;
      parent.children += this;
    }
    // println("WALKING " + name + " PARENT " + parent);
    val c = getClass();
    for (m <- c.getDeclaredMethods) {
      val name = m.getName();
      // println("LOOKING FOR " + name);
      val types = m.getParameterTypes();
      if (types.length == 0) {
        val o = m.invoke(this);
        o match { 
          case child: Component => child.markComponents(this);
          case node: Node => if (node.name == "" || node.name == null) node.name = name;
          case any =>
        }
      }
    }
  }
  def compileV(): Unit = {
    markComponents(null);
    val out = new java.io.FileWriter("../" + name + ".v");
    currentComp = this;
    doCompileV(out, 0);
    out.close();
  }
  def nameAllIO(): Unit = {
    // println("NAMING " + this);
    io.name_it("");
    for (child <- children) 
      child.nameAllIO();
  }
  def compileC(): Unit = {
    markComponents(null);
    val out_h = new java.io.FileWriter("../" + name + ".h");
    val out_c = new java.io.FileWriter("../" + name + ".cpp");
    isEmittingC = true;
    println("// COMPILING " + this + " NC = " + children.length);
    if (isEmittingComponents) {
      io.name_it("");
      for (top <- children)
        top.compileC();
    } else {
      nameAllIO();
      topComponent = this;
    }
    // isWalked.clear();
    findNodes(0);
    findConsumers();
    inferAll();
    collectNodes();
    findOrdering(); // search from roots  -- create omods
    findGraph();    // search from leaves -- create gmods
    val (numNodes, maxWidth, maxDepth) = findGraphDims();
    println("NUM " + numNodes + " MAX-WIDTH " + maxWidth + " MAX-DEPTH " + maxDepth);
    for (m <- omods) {
      m match {
        case l: Lit => ;
        case any    => 
          if (m.name != "" && !(m.component == null)) 
            m.name = m.component.name + "_" + m.name;
      }
      // println(">> " + m.name);
    }
    // for (m <- omods)
    //   println("MOD " + m + " IN " + m.component.name);
    out_h.write("class " + name + "_t : public mod_t {\n");
    out_h.write(" public:\n");
    if (isEmittingComponents) {
      for ((n, w) <- wires) 
        out_h.write("  dat_t<" + w.width + "> " + w.emitRef + ";\n");
    }
    for (m <- omods) 
      out_h.write(m.emitDecC);
    if (isEmittingComponents) {
      for (c <- children) 
        out_h.write("  " + c.emitRef + "_t* " + c.emitRef + ";\n");
    }
    out_h.write("\n");
    out_h.write("  void init ( void );\n");
    out_h.write("  void clock_lo ( dat_t<1> reset );\n");
    out_h.write("  void clock_hi ( dat_t<1> reset );\n");
    out_h.write("};\n");
    out_h.close();

    out_c.write("#include \"emulator.h\"\n");
    out_c.write("#include \"" + name + ".h\"\n");
    out_c.write("\n");
    out_c.write("void " + name + "_t::init ( void ) {\n");
    if (isEmittingComponents) {
      for (c <- children) {
        out_c.write("  " + c.emitRef + " = new " + c.emitRef + "_t();\n");
        out_c.write("  " + c.emitRef + "->init();\n");
      }
    }
    out_c.write("}\n");
    out_c.write("void " + name + "_t::clock_lo ( dat_t<1> reset ) {\n");
    for (m <- omods) 
      out_c.write(m.emitDefLoC);
    // for (c <- children) 
    //   out_c.write("    " + c.emitRef + "->clock_lo(reset);\n");
    out_c.write("}\n");
    out_c.write("void " + name + "_t::clock_hi ( dat_t<1> reset ) {\n");
    for (m <- omods) 
      out_c.write(m.emitDefHiC);
    // for (c <- children) 
    //   out_c.write("    " + c.emitRef + "->clock_hi(reset);\n");
    out_c.write("}\n");
    out_c.close();
  }
};

abstract class Interface extends Node {
  def apply(name: String): Interface = null
  def flatten = Array[(String, IO)]();
  def flip(): this.type = this;
}

object Bundle {
  def nullBundle = Bundle(Map[String, Interface]());
  def apply (elts: Map[String, Interface]): Bundle = {
    val res = new Bundle();
    // println("NEW BUNDLE");
    res.elementsCache = elts; // TODO: REMOVE REDUNDANT CREATION
    for ((n, i) <- elts) {
      i.name = n;
      // println("  ELT " + n + " " + i);
    }
    res
  }
}
class Bundle(view_arg: Seq[String] = null) extends Interface {
  var view = view_arg;
  var elementsCache: Map[String, Interface] = null;
  def calcElements(view: Seq[String]): Map[String, Interface] = {
    val c      = getClass();
    var elts   = Map[String, Interface]();
    var isCollecting = true;
    // println("COLLECTING " + c + " IN VIEW " + view);
    for (m <- c.getMethods) {
      val name = m.getName();
      if (isCollecting) {
        val modifiers = m.getModifiers();
        // println("  CHECKING " + name + " MODS " + modifiers);
        val types = m.getParameterTypes();
        val rtype = m.getReturnType();
        var isFound = false;
        var isInterface = false;
        var c = rtype;
        val sc = Class.forName("Chisel.Interface");
        do {
          if (c == sc) {
            isFound = true; isInterface = true;
          } else if (c == null || c == Class.forName("java.lang.Object")) {
            isFound = true; isInterface = false;
          } else 
            c = c.getSuperclass();
        } while (!isFound);
        if (types.length == 0 && !isStatic(modifiers) && isInterface &&
            name != "elements" && name != "flip" && name != "toString" && name != "flatten" && name != "binding" &&
            (view == null || view.contains(name))) {
          val o = m.invoke(this);
          o match { 
            case i: Interface => elts += ((name, i)); i.name = name; 
              // println("    ADDING " + name + " -> " + o);
            case any =>
              // println("    FOUND " + o);
          }
        }
      } else if (name == "elementsCache") 
        // println("IS-COLLECTING");
        isCollecting = true;
    }
    // println("END ->>>>");
    elts
  }
  def elements: Map[String, Interface] = {
    if (elementsCache == null) {
      elementsCache = calcElements(view);
    }
    elementsCache
  }
  override def toString: String = {
    val init = "BUNDLE(";
    var res = init;
    for ((n, i) <- elements) {
      if (res.length() > init.length()) res += ", ";
      res += n + " => " + i;
    }
    res += ")";
    res
  }
  def view (elts: Map[String, Interface]): Bundle = { 
    elementsCache = elts; this 
  }
  override def name_it (path: String) = {
    for ((n, i) <- elements) {
      i.name = (if (path.length > 0) path + "_" else "") + n;
      i.name_it(i.name);
      // println("  ELT " + n + " " + i);
    }
  }
  def +(other: Bundle): Bundle = {
    var elts = Map[String, Interface]();
    for ((n, i) <- elements) 
      elts += ((n, i));
    for ((n, i) <- other.elements) 
      elts += ((n, i));
    Bundle(elts)
  }
  override def flip(): this.type = {
    for ((n, i) <- elements) 
      i.flip()
    this
  }
  override def findNodes(depth: Int): Unit = {
    for ((n, elt) <- elements) 
      elt.findNodes(depth);
    /*
    val elts = flatten;
    println(depthString(depth) + "BUNDLE " + this + " => " + elts);
    for ((n, elt) <- elts) {
      println(depthString(depth+1) + "I: " + elt);
      elt match { case i: Input => i.findNodes(depth); case o => ; }
    }
    for ((n, elt) <- elts) {
      println(depthString(depth+1) + "O: " + elt);
      elt match { case o: Output => o.findNodes(depth); case i => ; }
    }
    */
  }
  override def apply(name: String): Interface = elements(name)
  override def <>(src: Node) = { 
    // println("B <>'ing " + this + " & " + src);
    src match {
      case other: Bundle => {
        for ((n, i) <- elements) {
          if (other.elements.contains(n))
            i <> other(n);
          else
            println("// UNABLE TO FIND " + n + " IN " + other.component);
        }
      }
      case default =>
        println("// TRYING TO CONNECT BUNDLE TO NON BUNDLE " + default);
    }
  }
  override def ^^(src: Node) = { 
    // println("B <>'ing " + this + " & " + src);
    src match {
      case other: Bundle =>
        for ((n, i) <- elements) {
          // println(" := ELT " + i + " & " + other(n));
          i ^^ other(n);
        }
    }
  }
  override def flatten: Array[(String, IO)] = {
    var res = ArrayBuffer[(String, IO)]();
    for ((n, i) <- elements)
      res = res ++ i.flatten
    res.toArray
  }
}

object Wire {
  def apply(): Wire = Wire("")
  def apply(name: String): Wire = {
    val res = new Wire();
    res.init(name, widthOf(0), null);
    res
  }
  def apply(name: String, width: Int): Wire = {
    val res = new Wire();
    res.init(name, width, null);
    res
  }
  def apply(width: Int): Wire = apply("", width);
}
class Wire extends Interface {
  // override def toString: String = "W(" + name + ")"
  override def toString: String = name
  override def emitDef: String = { 
    if (inputs.length == 0) {
      println("// UNCONNECTED " + this + " IN " + component); ""
    } else if (inputs(0) == null) {
      println("// UNCONNECTED WIRE " + this + " IN " + component); ""
    } else
      "  assign " + emitTmp + " = " + inputs(0).emitRef + ";\n" }
  override def emitDefLoC: String = 
    // TODO: NEED THIS TO BE A CHECK
    if (inputs.length == 1)
      "    " + emitTmp + " = " + inputs(0).emitRef + ";\n"
    else
      ""
    // "    " + emitTmp + " = " + inputs(0).emitRef + ";\n"
}

// used for component to component connections
object Binding {
  def apply(m: Node, c: Component): Node = {
    // println("BINDING " + m);
    if (isEmittingComponents) {
      val res = m.findBinding();
      if (res == null) {
        val res = new Binding();
        res.component = c;
        res.init("", widthOf(0), m);
        res.name = c.genName(m.name); // TODO: NAME
        // println("ADDING NEW BINDING " + m);
        // println("ADDING BINDING " + res + " TO " + res.component.name);
        // res.component.bindings += res;
        c.bindings += res;
        res
      } else {
        // println("FOUND BINDING " + res);
        res;
      }
    } else
      m
  }
}
class Binding extends Node {
  // override def emitDec: String = "";
  override def emitDef: String = "";
  override def toString: String = "BINDING(" + inputs(0) + ")";
  override def emitDecC: String = 
    if (isEmittingComponents) "  dat_t " + emitRef + ";\n"; else "";
  override def emitDefLoC: String = ""
  override def emitDefHiC: String = ""
  override def emitRefC: String = 
    if (isEmittingComponents) emitRefV; else inputs(0).emitRefC;

}

object IOdir {
  val INPUT  = new IOdir(0)
  val OUTPUT = new IOdir(1)
}
class IOdir (idi: Int) {
  val id = idi
  override def toString: String = if (id == 0) "INPUT-DIR" else "OUTPUT-DIR";
}

object IO {
  def Input(width: Int): IO = Input("", width)
  def Input(name: String): IO = Input(name, widthOf(0))
  def Input(name: String, width: Int): IO = 
    { val res = new IO(); res.dir = INPUT; res.init(name, width); res }
  def Input(name: String, inferWidth: (Node) => Int): IO = 
    { val res = new IO(); res.dir = INPUT; res.init(name, inferWidth); res }

  def Output(width: Int): IO = Output("", width);
  def Output(name: String): IO = Output(name, widthOf(0), null)
  def Output(name: String, inferWidth: (Node) => Int, x: Node): IO = 
    { val res = new IO(); res.dir = OUTPUT; res.init(name, inferWidth, x); res }
  def Output(name: String, width: Int): IO = 
    { val res = new IO(); res.dir = OUTPUT; res.init(name, width); res }
  def Output(name: String, x: Node): IO = Output(name, maxWidth _, x); 
  def Output(x: Node): IO = Output("", x);
}
class IO extends Wire { 
  var dir: IOdir = null;
  override def emitDef: String = {
    if (dir == INPUT)
      ""
    else
      super.emitDef;
  }
  override def emitDec: String = "";
  override def emitDecC: String = if (isEmittingComponents) ""; else super.emitDecC;
  // override def emitDef: String = "";
  override def apply(name: String): Interface = this
  override def flatten = Array((name, this));
  override def emitDefLoC: String = {
    if (dir == INPUT) {
      // TODO: HACK
      if (inputs.length == 1)
        "    " + emitTmp + " = " + inputs(0).emitRef + ";\n"
      // else if (consumers.length == 1)
      //   "    " + consumers(0).emitRef + " = " + emitTmp + ";// CONSUMER=1 \n"
      else
        ""
    } else
      super.emitDefLoC
  }
  override def toString: String = {
    if (dir == INPUT)
      "INPUT(" + name + "." + component + ")";
    else 
      "OUTPUT(" + name + "." + component + ")";
  }
  override def flip(): this.type = {
    if (dir == INPUT) {
      dir = OUTPUT;
    } else {
      dir = INPUT
    }
    this
  }
  override def <>(src: Node) = { 
    if (dir == INPUT) {
      // println("<>'ing " + this + " <-- " + src);
      src match { 
      case other: IO => 
        if (other.dir == OUTPUT) {
          this := other;
        } else {
          println("Connecting Input " + this + " to Input " + other);
        }
      case default => println("Connecting Input " + this + " to Node " + default);
      }
    } else { // DIR == OUTPUT
      // println("<>'ing " + this + " --> " + src);
      src match { 
        case other: IO  => 
          if (other.dir == INPUT) {
            other := this;
          } else {
            println("Connecting Output " + this + " to Output " + other);
          }
        case default => 
          println("Connecting Output " + this + " to Node " + default);
      }
    } 
  }
  override def ^^(parent: Node) = { 
    if (dir == INPUT) {
      // println("^^ " + this + " COMP " + component + " & " + src + " SRC COMP " + src.component);
      parent match { 
        case other: IO => 
          if (other.dir == INPUT) {
            this := other;
          } else 
            println("// ^^ing Input " + this + " to Output " + other);
        case default => 
          println("// ^^ing Input " + this + " to Node " + default);
      }
    } else { // dir == OUTPUT
      // println("^^ing " + this + " COMP " + component + " & " + src + " SRC COMP " + src.component + " CHILD? " + isChild);
      parent match { 
        case other: IO  => 
          if (other.dir == OUTPUT) {
            other := this;
          } else
            println("// ^^ing Output " + this + " to Input " + other);
        case default => 
          println("// ^^ing Output " + this + " to Node " + default);
      } 
    }
  }
};

object Lit {
  implicit def intToLit (x: Int) = Lit(x);
  def sizeof(x: Int): Int = { 
    val y = max(1, abs(x)).toDouble;
    val res = max(1, (ceil(log(y+1)/log(2.0))).toInt);
    // println("SIZEOF " + y + " LOG2 " + (log(y)/log(2.0)) + " IS " + res);
    res
  }
  val hexNibbles = "0123456789abcdef";
  def toHexNibble(x: String, off: Int): Char = {
    var res = 0;
    // println("OFF = " + off);
    for (i <- 0 until 4) {
      val idx = off+i;
      val c   = if (idx < 0) '0' else x(idx);
      res     = 2 * res + (if (c == '1') 1 else 0);
    }
    hexNibbles(res)
  }
  val pads = Vector(0, 3, 2, 1);
  def toHex(x: String): String = {
    var res = "";
    val numNibbles = (x.length-1) / 4 + 1;
    val pad = pads(x.length % 4);
    // println("X = " + x + " NN = " + numNibbles + " PAD = " + pad);
    for (i <- 0 until numNibbles) {
      res += toHexNibble(x, i*4 - pad);
    }
    res
  }
  def parseLit(x: String): (String, String, Int) = {
    var bits = "";
    var mask = "";
    var width = 0;
    for (d <- x) {
      if (d != '_') {
        width += 1;
        mask   = mask + (if (d == '?') "0" else "1");
        bits   = bits + (if (d == '?') "0" else d.toString);
      }
    }
    (bits, mask, width)
  }
  def apply(x: Int): Lit = { val res = new Lit(); res.init(x.toString, sizeof(x)); res }
  def apply(x: Int, width: Int): Lit = { val res = new Lit(); res.init(x.toString, width); res }
  def apply(n: String): Lit = { 
    val (bits, mask, width) = parseLit(n);  apply(n, width);
  }
  def apply(n: String, width: Int): Lit = { 
    val (bits, mask, swidth) = parseLit(n);
    val isZ = (n.contains('?'));
    val res = new Lit(); res.init(n, width); res.isZ = isZ; res.isBinary = true; res 
  }
}
class Lit extends Node {
  var isZ = false;
  var isBinary = false;
  // override def toString: String = "LIT(" + name + ")"
  override def findNodes(depth: Int): Unit = { }
  def value: Int = name.toInt;
  override def toString: String = name;
  override def emitDecC: String = "";
  override def emitRefC: String = 
    if (isBinary) { 
      var (bits, mask, width) = parseLit(name);
      if (isZ) {
        ("LITZ<" + width + ">(0x" + toHex(bits) + ", 0x" + toHex(mask) + ")")
      } else
        ("LIT<" + width + ">(0x" + toHex(bits) + ")")
    } else 
      ("LIT<" + width + ">(" + name + ")")
  override def emitDec: String = "";
  override def emitRefV: String = 
    if (width == -1) name 
    else if (isBinary) ("" + width + "'b" + name)
    else ("" + width + "'d" + name);
  def d (x: Int): Lit = Lit(x, value)
}

class Delay extends Node {
}


object Mem {
  def apply (n: Int, isEnable: Node, wrAddr: Node, wrData: Node): Mem = {
    val res = new Mem();
    res.init("", widthOf(2), isEnable, wrAddr, wrData);
    res.n = n;
    res
  }
  def apply (n: Int, isEnable: Node, wrAddr: Node, wrData: Node, reset: Node): Mem = {
    val res = apply(n, isEnable, wrAddr, wrData);
    res.resetVal = reset;
    res.inputs  += res.resetVal;
    res
  }
}
class Mem extends Delay {
  var n = 0;
  var resetVal: Node = null;
  override def toString: String = "MEM(" + inputs(0) + " " + inputs(1) + " " + inputs(2) + ")";
  override def emitDef: String = {
    var res = 
      "  always @(posedge clk) begin\n" +
      "    if (" + inputs(0).emitRef + ")\n" +
      "      " + emitRef + "[" + inputs(1).emitRef + "] <= " + inputs(2).emitRef + ";\n";
    if (!(resetVal == null)) {
      res += "    else if (reset) begin\n";
      for (i <- 0 until n) 
        res += "      " + emitRef + "[" + i + "] <= " + resetVal.emitRef + ";\n";
      res += "    end\n";
    }
    res += "  end\n";
    res
  }
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + "[" + (n-1) + ":0];\n";
  override def emitDefHiC: String = {
    var res = 
      "    if (" + inputs(0).emitRef + ".to_bool())\n" +
      "      " + emitRef + ".put(" + inputs(1).emitRef + ", " + inputs(2).emitRef + ");\n";
    if (!(resetVal == null)) {
      res += "    if (reset.to_bool()) {\n";
      for (i <- 0 until n) 
        res += "      " + emitRef + ".put(" + i + ", " + resetVal.emitRef + ");\n";
      res += "    }\n";
    }
    res
  }
  override def emitDecC: String = 
    "  mem_t<" + width + "," + n + "> " + emitRef + ";\n";
  override def apply(addr: Node): Node = MemRef(this, addr);
}

object MemRef {
  def apply (mem: Node, addr: Node): Node = {
    val res = new MemRef();
    res.init("", widthOf(0), mem, addr);
    res
  }
}
class MemRef extends Node {
  override def toString: String = inputs(0) + "[" + inputs(1) + "]";
  override def emitDef: String = 
    "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + inputs(1).emitRef + "];\n"
  override def emitDefLoC: String = 
    "    " + emitTmp + " = " + inputs(0).emitRef + ".get(" + inputs(1).emitRef + ");\n"
}

object MuxLookup {
  def apply (key: Node, default: Node, mapping: Vector[(Node, Node)]): Node = {
    var res = default;
    for ((k, v) <- mapping.reverse)
      res = Mux(key === k, v, res);
    res
  }
}

object MuxCase {
  def apply (default: Node, mapping: Vector[(Node, Node)]): Node = {
    var res = default;
    for ((t, v) <- mapping.reverse)
      res = Mux(t, v, res);
    res
  }
}

object Lookup {
  def apply (addr: Node, default: Node, mapping: Map[Lit, Node]): Lookup = {
    val res = new Lookup();
    res.init("", widthOf(1), addr, default); 
    for ((addr, data) <- mapping)
      res.inputs += data;
    res.map = mapping;
    res
  }
}

class Lookup extends Node {
  var map: Map[Lit, Node] = null;
  override def toString: String = "LOOKUP(" + inputs(0) + ")";
  override def emitDef: String = {
    var res = 
      "  always @(*) begin\n" +
      "    " + emitRef + " = " + inputs(1).emitRef + ";\n" +
      "    casez (" + inputs(0).emitRef + ")" + "\n";
    
    for ((addr, data) <- map) 
      res = res +
        "      " + addr.emitRef + " : " + emitRef + " = " + data.emitRef + ";\n";
    res = res + 
      "    endcase\n" +
      "  end\n";
    res
  }
  override def emitDefLoC: String = {
    var res = "";
    for ((addr, data) <- map) 
      res = res +
        "    if ((" + addr.emitRef + " == " + inputs(0).emitRef + ").to_bool()) " + emitRef + " = " + data.emitRef + ";\n";
    res
  }
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";
}

object Op {
  def apply (name: String, widthInfer: (Node) => Int, a: Node, b: Node): Node = {
    val res = new Op();
    res.init("", widthInfer, a, b);
    res.op = name;
    res
  }
  def apply (name: String, widthInfer: (Node) => Int, a: Node): Node = {
    val res = new Op();
    res.init("", widthInfer, a);
    res.op = name;
    res
  }
}
class Op extends Node {
  var op: String = "";
  override def toString: String =
    if (inputs.length == 1)
      op + inputs(0)
    else
      inputs(0) + op + inputs(1)
  override def emitDef: String = 
    if (inputs.length == 1)
      "  assign " + emitTmp + " = " + op + " " + inputs(0).emitRef + ";\n"
    else
      "  assign " + emitTmp + " = " + inputs(0).emitRef + " " + op + " " + inputs(1).emitRef + ";\n"
  override def emitDefLoC: String = 
    if (inputs.length == 1)
      "    " + emitTmp + " = " + op + inputs(0).emitRef + ";\n"
    else
      "    " + emitTmp + " = " + inputs(0).emitRef + " " + op + " " + inputs(1).emitRef + ";\n"
}

object Probe {
  def apply(x: Node, name: String): Probe = { val res = new Probe(); res.init(name, widthOf(0), x); res }
}
class Probe extends Node {
  override def toString: String =
    "Probe(" + inputs(0) + ", " + name + ")"
  override def emitDef: String = 
    "  assign " + emitTmp + " = " + inputs(0).emitRef + ";\n"
  override def emitDefLoC: String = 
    "    " + "printf(\"" + name + ": %lx\\n\", " + inputs(0).emitRef + ".to_ulong());\n" +
    "    " + emitTmp + " = " + inputs(0).emitRef + ";\n";
}

object Fill {
  def fillWidthOf(i: Int, n: Int) = { (m: Node) => m.inputs(i).width * n }
  def apply (mod: Node, n: Int): Node = {
    val res = new Fill();
    res.init("", fillWidthOf(0, n), mod);
    res.n = n;
    res
  }
}
class Fill extends Node {
  var n: Int = 0;
  override def toString: String = "FILL(" + inputs(0) + ", " + n + ")";
  override def emitDef: String = 
    "  assign " + emitTmp + " = {" + n + "{" + inputs(0).emitRef + "}};\n";
  override def emitDefLoC: String = 
    "    " + emitTmp + " = " + inputs(0).emitRef + ".fill<" + width + "," + n + ">();\n";
}

object Bits {
  def apply (mod: Node, bit: Int): Node = 
    apply(mod, Lit(bit));
  def apply (mod: Node, hi: Int, lo: Int): Node = {
    val res = new Bits();
    res.hi = Lit(hi);
    res.lo = Lit(lo);
    res.init("", fixWidth(hi-lo+1), mod, res.hi, res.lo);
    res
  }
  def apply (mod: Node, bit: Node): Node = {
    val res = new Bits();
    res.init("", fixWidth(1), mod, bit);
    res.hi = bit; 
    res.lo = bit;
    res
  }
  def apply (mod: Node, hi: Node, lo: Node): Node = {
    val res = new Bits();
    res.init("", widthOf(0), mod, hi, lo);
    res.hi = hi;
    res.lo = lo;
    res
  }
}
class Bits extends Node {
  var lo: Node = null;
  var hi: Node = null;
  override def toString: String =
    if (hi == lo)
      "BITS(" + inputs(0) + ", " + lo + ")";
    else
      "BITS(" + inputs(0) + ", " + hi + ", " + lo + ")";
  override def emitDef: String = 
    if (hi == lo)
      "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + hi.emitRef + "];\n"
    else
      "  assign " + emitTmp + " = " + inputs(0).emitRef + "[" + hi.emitRef + ":" + lo.emitRef + "];\n"
  override def emitDefLoC: String = 
    if (hi == lo)
      "    " + emitTmp + " = " + inputs(0).emitRef + ".bit(" + hi.emitRef + ");\n"
    else
      "    " + emitTmp + " = " + inputs(0).emitRef + ".extract<" + width + ">(" + hi.emitRef + "," + lo.emitRef + ");\n"
}

object Cat {
  def apply (mods: Node*): Node = {
    val res = new Cat();
    res.init("", sumWidth _);
    for (m <- mods)
      res.inputs += m;
    res
  }
}
class Cat extends Node {
  override def toString: String = {
    var res = "Cat(";
    for (i <- inputs)
      res = res + i + " ";
    res + ")"
  }
  override def emitDef: String = {
    var res = "  assign " + emitTmp + " = {" 
    var is_first = true;
    for (i <- inputs) {
      if (!is_first) res = res + ", ";
      res = res + i.emitRef;
      is_first = false;
    }
    res + "};\n"
  }
  override def emitDefLoC: String = {
    var res = "    " + emitTmp + " = cat<" + width + ">(" 
    var is_first = true;
    for (i <- inputs) {
      if (!is_first) res = res + ", ";
      res = res + i.emitRef;
      is_first = false;
    }
    res + ");\n"
  }
}

object Mux {
  def apply (t: Node, c: Node, a: Node): Node = 
    new Mux().init("", maxWidth _, t, c, a);
}
class Mux extends Op {
  override def toString: String =
    inputs(0) + " ? " + inputs(1) + " : " + inputs(2)
  override def emitDef: String = 
    "  assign " + emitTmp + " = " + inputs(0).emitRef + " ? " + inputs(1).emitRef + " : " + inputs(2).emitRef + ";\n"
  override def emitDefLoC: String = 
    "    " + emitTmp + " = mux<" + width + ">(" + inputs(0).emitRef + ", " + inputs(1).emitRef + ", " + inputs(2).emitRef + ");\n"
  def ::(a: Node): Mux = { inputs(2) = a; this }
}

object Reg {
  def apply(n: String, u: Node): Reg = 
    new Reg().init(n, maxWidth _, null, u).asInstanceOf[Reg];
  def apply(u: Node): Reg = Reg("", u)
  def apply(n: String): Reg = Reg(n, null)
  def apply(n: String, w: Int): Reg = {
    val res = new Reg().init(n, fixWidth(w), Lit(0, w), null).asInstanceOf[Reg];
    res.width_ = w;
    res
  }
  def apply(w: Int): Reg = Reg("", w);
  def apply(): Reg = Reg("", null)
}
class Reg extends Delay {
  // var clauses = new ArrayBuffer[Pair[Node, Node]];
  def reset(init: Node): Reg = { inputs(0) = init; this }
  def <==(src: Node) = {
    if (cond.length == 0)
      inputs(1) = src;
    else if (inputs(1) == null) {
      var res = cond(0);
      for (i <- 1 until cond.length)
        res = cond(i) && res;
      // println(this.name + " <== " + res + " " + cond.length);
      // val res = cond.foldRight(Lit(1,1)){(a, b) => a&&b}
      inputs(1) = Mux(res, src, this)
    } else {
      var res = cond(0);
      for (i <- 1 until cond.length)
        res = cond(i) && res;
      // println(this.name + " <== " + res + " " + cond.length);
      inputs(1) = Mux(res, src, inputs(1))
    }
    // clauses += Pair(cond.head, src);
  }
  def nameOpt: String = if (name.length > 0) name else "REG"
  override def toString: String = {
    if (component.isWalking.contains(this)) 
      nameOpt
    else {
      component.isWalking += this;
      val res = 
      if (inputs(1) == null) 
        nameOpt + "(" + inputs(0) + ")"
      else
        nameOpt + "(" + inputs(0) + "," + inputs(1) + ")";
      component.isWalking -= this;
      res;
    }
  }
  override def emitRefV: String = if (name == "") "R" + emitIndex else name;
  override def emitDef: String = "";
  override def emitReg: String =
    "    " + emitRef + " <= " + 
    (if (inputs(0) == null) "" else "reset ? " + inputs(0).emitRef + " : ") + 
    inputs(1).emitRef + ";\n"
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";

  override def emitDefLoC: String = 
    "    " + emitRef + "_shadow = " + 
    (if (inputs(0) == null) "" else "mux<" + width + ">(reset, " + inputs(0).emitRef + ", ") + 
    inputs(1).emitRef + ");\n"
  override def emitDefHiC: String =
    "    " + emitRef + " = " + emitRef + "_shadow;\n";
  override def emitDecC: String = 
    "  dat_t<" + width + "> " + emitRef + ";\n" +
    "  dat_t<" + width + "> " + emitRef + "_shadow;\n";
}

}
