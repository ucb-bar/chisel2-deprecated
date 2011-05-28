// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import java.lang.reflect.Modifier._;
import java.io.File;

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

object Node {
  var cond = new Stack[Node];
  implicit def intToNode(x: Int): Node = Lit(x);
  var compStack = new Stack[Component]();
  var currentComp: Component = null;
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
object Enum {
  def apply(l:List[Symbol]) = (l zip (Range(0, l.length, 1).map(x => Lit(x)))).toMap;
}
object Cat {
  def apply (mod: Node, mods: Node*): Node = mods.foldLeft(mod){(a, b) => a ## b}
}
object when {
  def apply(c: Node)(block: => Unit) = {
    cond.push(c); 
    // println("WHEN " + c + " {");
    block; 
    cond.pop();
    // println("} ");
  }
}
object pmux {
  def apply(c: Node)(con_block: => Unit)(alt_block: => Unit) = {
    val tt = c;
    cond.push(tt);  
    // println("  IF " + tt + " {");
    con_block; 
    // println("  }");
    cond.pop();
    val et = !c;
    cond.push(et); 
    // println("  ELSE IF " + et + " {");
    alt_block; 
    cond.pop();
    // println("  }");
  }
}
object pcond {
  def apply(cases: Seq[(Node, () => Node)]) = {
    var tst: Node = Lit(1);
    for ((ctst, block) <- cases) {
      cond.push(tst && ctst);  
      block(); 
      cond.pop();
      tst = tst && !ctst;
    }
    this;
  }
}
object pcase {
  def apply(x: Node, cases: Seq[(Lit, () => Node)]) = 
    pcond(cases.map(tb => (tb._1 === x, tb._2)))
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
      if (isEmittingComponents) {
        this match {
          case io: IO => {
            val ioComp = io.component;
            val nxtComp = if (io.dir == OUTPUT) ioComp else ioComp.parent;
            (ioComp, nxtComp, nxtComp);
          }
          case any    => (c, c, c);
        }
      } else
        (c, c, component);
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
            node.component = markComp;
          node.findNodes(depth + 2, nextComp);
          node match { 
            case io: IO => 
              if (io.dir == OUTPUT) {
                val c = node.component.parent;
                // println("BINDING " + node + " I " + i + " NODE-PARENT " + node.component.parent + " -> " + this + " PARENT " + component.parent);
                if (c == null) {
                  println("UNKNOWN COMPONENT FOR " + node);
                }
                val b = Binding(node, c);
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

object Component {
  var isGenHarness = false;
  var isReportDims = false;
  var scanFormat = "";
  var scanArgs: List[String] = Nil;
  var printFormat = "";
  var printArgs: List[String] = Nil;
  var targetEmulatorRootDir: String = null;
  var targetVerilogRootDir: String = null;
  var targetDir: String = null;
  var compIndex = -1;
  var compIndices = HashMap.empty[String,Int];
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
  def splitArg (s: String) = s.split(' ').toList;
  def initChisel () = {
    cond = new Stack[Node];
    compStack = new Stack[Component]();
    isGenHarness = false;
    isReportDims = false;
    scanFormat = "";
    scanArgs = Nil;
    printFormat = "";
    printArgs = Nil;
    isCoercingArgs = true;
    targetEmulatorRootDir = System.getProperty("CHISEL_EMULATOR_ROOT");
    if (targetEmulatorRootDir == null) targetEmulatorRootDir = "../emulator";
    targetVerilogRootDir = System.getProperty("CHISEL_VERILOG_ROOT");
    if (targetVerilogRootDir == null) targetVerilogRootDir = "../verilog";
    targetDir = "";
    currentComp = null;
    compIndex = -1;
    compIndices.clear();
    isEmittingComponents = false;
    isEmittingC = false;
    topComponent = null;
  }
}
object chisel_main {
  def apply(args: Array[String], gen: () => Component) = {
    initChisel();
    var i = 0;
    while (i < args.length) {
      val arg = args(i);
      arg match {
        case "--gen-harness" => isGenHarness = true; 
        case "--v" => isEmittingComponents = true; isCoercingArgs = false;
        case "--target-dir" => targetDir = args(i+1); i += 1;
        case "--scan-format" => scanFormat = args(i+1); i += 1;
        case "--scan-args" => scanArgs = splitArg(args(i+1)); i += 1;
        case "--print-format" => printFormat = args(i+1); i += 1;
        case "--print-args" => printArgs = splitArg(args(i+1)); i += 1;
        // case "--is-coercing-args" => isCoercingArgs = true;
        case any => println("UNKNOWN ARG");
      }
      i += 1;
    }
    val c = gen();
    if (isEmittingComponents)
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
  var registered_children = new ArrayBuffer[Component];

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
      val cname  = getClass().getName(); 
      val dotPos = cname.lastIndexOf('.');
      name = if (dotPos >= 0) cname.substring(dotPos+1) else cname;
      if (compIndices contains name) {
        val compIndex = (compIndices(name) + 1);
        compIndices += (name -> compIndex);
        name = name + "_" + compIndex;
      } else
        compIndices += (name -> 0);
    }
  }
  def register_child(c: Component): Component = {
    registered_children += c;
    c
  }
  def findBinding(m: Node): Binding = {
    // println("FINDING BINDING " + m + " OUT OF " + bindings.length + " IN " + this);
    for (b <- bindings) {
      // println("LOOKING AT " + b + " INPUT " + b.inputs(0));
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
            res += "  " + emitRef + "->" + n + " = " + io.inputs(0).emitRef + ";\n";
      };
    }
    res += emitRef + "->clock_lo(reset);\n";
    for ((n, w) <- wires) {
      w match {
        case io: IO => 
          if (io.dir == OUTPUT)
            res += "  " + io.consumers(0).emitRef + " = " + emitRef + "->" + n + ";\n";
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
  override def findNodes(depth: Int, c: Component): Unit = {
    io.findNodes(depth, c);
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
    // TODO: NOT SURE EXACTLY WHY I NEED TO PRECOMPUTE TMPS HERE
    for (m <- mods)
      m.emitTmp;
    out.write(emitDecs);
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
    for (c <- registered_children)
      c.markComponents(this);
  }
  def ensure_dir(dir: String) = {
    val d = dir + (if (dir == "" || dir(dir.length-1) == '/') "" else "/");
    new File(d).mkdirs();
    d
  }
  def compileV(): Unit = {
    markComponents(null);
    findNodes(0, this);
    val base_name = ensure_dir(targetVerilogRootDir + "/" + targetDir);
    val out = new java.io.FileWriter(base_name + name + ".v");
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
  def genHarness(base_name: String, name: String) = {
    val makefile = new java.io.FileWriter(base_name + name + "-makefile");
    makefile.write("CPPFLAGS = -O2 -I../\n\n");
    makefile.write(name + ": " + name + ".o" + " " + name + "-emulator.o\n");
    makefile.write("\tg++ -o " + name + " " + name + ".o " + name + "-emulator.o\n\n");
    makefile.write(name + ".o: " + name + ".cpp " + name + ".h\n");
    makefile.write("\tg++ -c ${CPPFLAGS} " + name + ".cpp\n\n");
    makefile.write(name + "emulator.o: " + name + "-emulator.cpp " + name + ".h\n");
    makefile.write("\tg++ -c ${CPPFLAGS} " + name + "-emulator.cpp\n\n");
    makefile.close();
    val harness  = new java.io.FileWriter(base_name + name + "-emulator.cpp");
    harness.write("#include \"emulator.h\"\n");
    harness.write("#include \"" + name + ".h\"\n");
    harness.write("int main (int argc, char* argv[]) {\n");
    harness.write("  " + name + "_t* c = new " + name + "_t();\n");
    harness.write("  int lim = (argc > 1) ? atoi(argv[1]) : -1;\n");
    harness.write("  c->init();\n");
    harness.write("  for (int t = 0; lim < 0 || t < lim; t++) {\n");
    harness.write("    dat_t<1> reset = LIT<1>(t == 0);\n");
    harness.write("    c->scan(stdin);\n");
    harness.write("    c->clock_lo(reset);\n");
    harness.write("    c->clock_hi(reset);\n");
    harness.write("    c->print(stdout);\n");
    harness.write("  }\n");
    harness.write("}\n");
    harness.close();
  }
  def compileC(): Unit = {
    markComponents(null);
    val base_name = ensure_dir(targetEmulatorRootDir + "/" + targetDir);
    val out_h = new java.io.FileWriter(base_name + name + ".h");
    val out_c = new java.io.FileWriter(base_name + name + ".cpp");
    if (isGenHarness)
      genHarness(base_name, name);
    isEmittingC = true;
    println("// COMPILING " + this + "(" + children.length + ")");
    if (isEmittingComponents) {
      io.name_it("");
      for (top <- children)
        top.compileC();
    } else {
      nameAllIO();
      topComponent = this;
    }
    // isWalked.clear();
    findNodes(0, this);
    findConsumers();
    inferAll();
    collectNodes();
    findOrdering(); // search from roots  -- create omods
    findGraph();    // search from leaves -- create gmods
    for (m <- omods) {
      m match {
        case l: Lit => ;
        case any    => 
          if (m.name != "" && !(m.component == null)) 
            m.name = m.component.name + "_" + m.name;
      }
      // println(">> " + m.name);
    }
    if (isReportDims) {
    val (numNodes, maxWidth, maxDepth) = findGraphDims();
    println("NUM " + numNodes + " MAX-WIDTH " + maxWidth + " MAX-DEPTH " + maxDepth);
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
      if (m.isInObject)
        out_h.write(m.emitDecC);
    if (isEmittingComponents) {
      for (c <- children) 
        out_h.write("  " + c.emitRef + "_t* " + c.emitRef + ";\n");
    }
    out_h.write("\n");
    out_h.write("  void init ( void );\n");
    out_h.write("  void clock_lo ( dat_t<1> reset );\n");
    out_h.write("  void clock_hi ( dat_t<1> reset );\n");
    out_h.write("  void print ( FILE* f );\n");
    out_h.write("  void scan ( FILE* f );\n");
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
    for (m <- omods) {
      out_c.write(m.emitInitC);
    }
    out_c.write("}\n");
    out_c.write("void " + name + "_t::clock_lo ( dat_t<1> reset ) {\n");
    for (m <- omods) {
      out_c.write(m.emitDefLoC);
    }
    // for (c <- children) 
    //   out_c.write("    " + c.emitRef + "->clock_lo(reset);\n");
    out_c.write("}\n");
    out_c.write("void " + name + "_t::clock_hi ( dat_t<1> reset ) {\n");
    for (m <- omods) 
      out_c.write(m.emitDefHiC);
    // for (c <- children) 
    //   out_c.write("    " + c.emitRef + "->clock_hi(reset);\n");
    out_c.write("}\n");
    def splitPrintFormat(s: String) = {
      var off = 0;
      var res: List[String] = Nil;
      for (i <- 0 until s.length) {
        if (s(i) == '%') {
          if (off < i) 
            res = s.substring(off, i) :: res;
          res = "%" :: res;
          off = i + 1;
        }
      }
      if (off < (s.length-1))
        res = s.substring(off, s.length) :: res;
      res.reverse
    }
    out_c.write("void " + name + "_t::print ( FILE* f ) {\n");
    if (printArgs.length > 0) {
      val format =
        if (printFormat == "") printArgs.map(a => "%").reduceLeft((y,z) => z + " " + y) 
        else printFormat;
      val toks = splitPrintFormat(format);
      var i = 0;
      for (tok <- toks) {
        if (tok(0) == '%') {
          out_c.write("  fprintf(f, \"%s\", " + name + "_" + printArgs(i) + ".to_str().c_str());\n");
          i += 1;
        } else {
          out_c.write("  fprintf(f, \"%s\", \"" + tok + "\");\n");
        }
      }
      out_c.write("  fprintf(f, \"\\n\");\n");
    }
    out_c.write("}\n");
    def constantArgSplit(arg: String) = arg.split('=');
    def isConstantArg(arg: String) = constantArgSplit(arg).length == 2;
    out_c.write("void " + name + "_t::scan ( FILE* f ) {\n");
    if (scanArgs.length > 0) {
      val format = 
        if (scanFormat == "") {
          var res = "";
          for (arg <- scanArgs) {
            if (!isConstantArg(arg)) {
              if (res.length > 0) res = res + " ";
              res = res + "%llx";
            }
          }
          res
        } else 
          scanFormat;
      out_c.write("  fscanf(f, \"" + format + "\"");
      for (arg <- scanArgs) {
        if (!isConstantArg(arg))
          out_c.write(",  &" + name + "_" + arg + ".values[0]");
      }
      out_c.write(");\n");
      for (arg <- scanArgs) {
        val argParts = constantArgSplit(arg);
        if (argParts.length == 2)
          out_c.write("  " + name + "_" + argParts(0) + ".values[0] = " + 
                      argParts(1).substring(0, argParts(1).length) + ";\n");
      }
    }
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

class BlackBox extends Component {
  override def doCompileV(out: java.io.FileWriter, depth: Int): Unit = {
    name_it();
    io.name_it("");
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
  override def findNodes(depth: Int, c: Component): Unit = {
    for ((n, elt) <- elements) 
      elt.findNodes(depth, c);
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
  def apply(): Wire = apply("")
  def apply(name: String, x: Node): Wire = {
    val res = new Wire();
    res.init(name, widthOf(0), x);
    res
  }
  def apply(x: Node): Wire = 
    apply("", x);
  def apply(name: String): Wire = 
    apply(name, null);
  def apply(name: String, width: Int): Wire = {
    val res = new Wire();
    res.init(name, width, null);
    res
  }
  def apply(width: Int): Wire = apply("", width);
}
class Wire extends Interface {
  // override def toString: String = "W(" + name + ")"
  def <==(src: Node): Wire = {
    if (cond.length == 0)
      inputs(0) = src;
    else {
      var res = cond(0);
      for (i <- 1 until cond.length)
        res = cond(i) && res;
      inputs(0) = Mux(res, src, inputs(0))
    }
    this
  }
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
      "  " + emitTmp + " = " + inputs(0).emitRef + ";\n"
    else
      ""
    // "  " + emitTmp + " = " + inputs(0).emitRef + ";\n"
}

// used for component to component connections
object Binding {
  def apply(m: Node, c: Component): Node = {
    // println("BINDING " + m);
    if (isEmittingComponents) {
      val res = c.findBinding(m);
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

object Input {
  def apply(width: Int): IO = apply("", width)
  def apply(name: String): IO = apply(name, widthOf(0))
  def apply(name: String, width: Int): IO = 
    { val res = new IO(); res.dir = INPUT; res.init(name, width); res }
  def apply(name: String, inferWidth: (Node) => Int): IO = 
    { val res = new IO(); res.dir = INPUT; res.init(name, inferWidth); res }
}
object Output {
  def apply(width: Int): IO = apply("", width);
  def apply(name: String): IO = apply(name, widthOf(0), null)
  def apply(name: String, inferWidth: (Node) => Int, x: Node): IO = 
    { val res = new IO(); res.dir = OUTPUT; res.init(name, inferWidth, x); res }
  def apply(name: String, width: Int): IO = 
    { val res = new IO(); res.dir = OUTPUT; res.init(name, width); res }
  def apply(name: String, x: Node): IO = apply(name, maxWidth _, x); 
  def apply(x: Node): IO = apply("", x);
}
class IO extends Wire { 
  var dir: IOdir = null;
  override def isIo = true;
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
        "  " + emitTmp + " = " + inputs(0).emitRef + ";\n"
      // else if (consumers.length == 1)
      //   "  " + consumers(0).emitRef + " = " + emitTmp + ";// CONSUMER=1 \n"
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
  def toLitVal(x: String): Int = {
    var res = 0;
    for (c <- x.substring(2, x.length)) 
      res = res * 16 + c.asDigit;
    res
  }

  def toLitVal(x: String, shamt: Int): Int = {
    var res = 0;
    for(c <- x)
      res = res * shamt + c.asDigit;
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
  def stringToVal(base: Char, x: String): Int = {
    if(base == 'x')
      toLitVal(x);
    else if(base == 'd')
      x.toInt
    else if(base == 'h')
      toLitVal(x, 16)
    else if(base == 'b')
      toLitVal(x, 2)  
    else
      -1
  }

  def apply(x: Int): Lit = { val res = new Lit(); res.init("0x%x".format(x), sizeof(x)); res }
  def apply(x: Int, width: Int): Lit = { val res = new Lit(); res.init("0x%x".format(x), width); res }
  def apply(x: Long, width: Int): Lit = { val res = new Lit(); res.init("0x%x".format(x), width); res }
  // def apply(n: String): Lit = { 
  //   val (bits, mask, width) = parseLit(n);  apply(n, width);
  // }
  def apply(n: String, width: Int): Lit = 
    apply(width, n(0), n.substring(1, n.length));
  def apply(width: Int, base: Char, literal: String): Lit = {
    if (!"dhb".contains(base)) throw new IllegalArgumentException("invalid base");
    val res = new Lit();
    res.init(literal, width); res.base = base;
    if (base == 'b') {res.isZ = literal.contains('?'); res.isBinary = true;}
    res
  }
}
class Lit extends Node {
  //implicit def intToLit (x: Int) = Lit(x);
  var isZ = false;
  var isBinary = false;
  var base = 'x';
  // override def toString: String = "LIT(" + name + ")"
  override def findNodes(depth: Int, c: Component): Unit = { }
  override def value: Int = stringToVal(base, name);
  override def maxNum = value;
  override def minNum = value;
  override def isLit = true;
  override def toString: String = name;
  override def emitDecC: String = "";
  override def emitRefC: String = 
    if (isBinary) { 
      var (bits, mask, swidth) = parseLit(name);
      var bwidth = if(base == 'b') width else swidth;
      if (isZ) {
        ("LITZ<" + bwidth + ">(0x" + toHex(bits) + ", 0x" + toHex(mask) + ")")
      } else
        ("LIT<" + bwidth + ">(0x" + toHex(bits) + ")")
    } else if(base == 'd' || base == 'x'){
      ("LIT<" + width + ">(" + name + "L)")
    } else
      ("LIT<" + width + ">(0x" + name + "L)")
  

  override def emitDec: String = "";
  override def emitRefV: String = 
    if (width == -1) name 
    else if(isBinary) ("" + width + "'b" + name)
    else if(base == 'x') ("" + width + "'h" + name.substring(2, name.length))
    else if(base == 'd') ("" + width + "'d" + name)
    else if(base == 'h') ("" + width + "'h" + name)
    else "";
  def d (x: Int): Lit = Lit(x, value)
  //def ~(x: String): Lit = Lit(value, x(0), x.substring(1, x.length));
}

class Delay extends Node {
  override def isReg = true;
}

object Rom {
  def romWidth(data: Array[Lit]) = { 
    (m: Node) => { 
      var res = 0; 
      for (d <- data) 
        res = max(d.width, res); 
      res  }
  }
  def apply (data: Array[Lit]): Rom = {
    val res = new Rom(data);
    res.init("", romWidth(data));
    res
  }
}
class Rom(data_vals: Array[Lit]) extends Delay {
  val data = data_vals;
  override def toString: String = "ROM(" + data + ")";
  override def emitDef: String = {
    var res = "  initial begin\n";
    for (i <- 0 until data.length) 
      res += "    " + emitRef + "[" + i + "] = " + data(i).emitRef + ";\n";
    res += "  end\n";
    res
  }
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + "[" + (data.length-1) + ":0];\n";
  override def emitInitC: String = {
    var res = "";
    for (i <- 0 until data.length) 
      res += "  " + emitRef + ".put(" + i + ", " + data(i).emitRef + ");\n";
    res
  }
  override def emitDecC: String = 
    "  mem_t<" + width + "," + data.length + "> " + emitRef + ";\n";
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
    "  " + emitTmp + " = " + inputs(0).emitRef + ".get(" + inputs(1).emitRef + ");\n"
}

object Mem {
  val noResetVal = Lit(0);
  def apply (n: Int, isEnable: Node = Lit(0), wrAddr: Node = Lit(0), wrData: Node = Lit(0), resetVal: Node = noResetVal): Mem = {
    val res = new Mem(n);
    res.init("", widthOf(2), isEnable, wrAddr, wrData, resetVal);
    res.isResetVal = resetVal != noResetVal;
    res
  }
}
class Mem(n_val: Int) extends Delay {
  val n          = n_val;
  var isResetVal = false;
  def wrEnable   = inputs(0);
  def wrEnable_= (x: Node) = inputs(0) = x;
  def wrAddr     = inputs(1);
  def wrAddr_= (x: Node) = inputs(1) = x;
  def wrData     = inputs(2);
  def wrData_= (x: Node) = inputs(2) = x;
  def resetVal   = inputs(3);
  def resetVal_= (x: Node) = { isResetVal = true; inputs(3) = x; }
  override def isRamWriteInput(i: Node) = 
    i == wrEnable || i == wrAddr || i == wrData;
  override def toString: String = "MEM(" + wrEnable + " " + wrAddr + " " + wrData + ")";
  override def emitDef: String = {
    var res = 
      "  always @(posedge clk) begin\n" +
      "    if (" + wrEnable.emitRef + ")\n" +
      "      " + emitRef + "[" + wrAddr.emitRef + "] <= " + wrData.emitRef + ";\n";
    if (isResetVal) {
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
      "  if (" + wrEnable.emitRef + ".to_bool())\n" +
      "    " + emitRef + ".put(" + wrAddr.emitRef + ", " + wrData.emitRef + ");\n";
    if (isResetVal) {
      res += "  if (reset.to_bool()) {\n";
      for (i <- 0 until n) 
        res += "    " + emitRef + ".put(" + i + ", " + resetVal.emitRef + ");\n";
      res += "  }\n";
    }
    res
  }
  override def emitDecC: String = 
    "  mem_t<" + width + "," + n + "> " + emitRef + ";\n";
  override def apply(addr: Node): Node = MemRef(this, addr);
}

object MuxLookup {
  def apply (key: Node, default: Node, mapping: Seq[(Node, Node)]): Node = {
    var res = default;
    for ((k, v) <- mapping.reverse)
      res = Mux(key === k, v, res);
    res
  }
}

object MuxCase {
  def apply (default: Node, mapping: Seq[(Node, Node)]): Node = {
    var res = default;
    for ((t, v) <- mapping.reverse)
      res = Mux(t, v, res);
    res
  }
}

object Lookup {
  def apply (addr: Node, default: Node, mapping: Seq[(Lit, Node)]): Lookup = {
    val res = new Lookup();
    res.init("", widthOf(1), addr, default); 
    for ((addr, data) <- mapping)
      res.inputs += data;
    res.map = mapping;
    res
  }
}

class Lookup extends Delay {
  var map: Seq[(Lit, Node)] = null;
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
        "  if ((" + addr.emitRef + " == " + inputs(0).emitRef + ").to_bool()) " + emitRef + " = " + data.emitRef + ";\n";
    res
  }
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";
}

class ListLookup(mapping: Array[(Lit, List[Node])], defaultVal: List[Node]) extends Node {
  val map = mapping;
  val default = defaultVal;
  val wires = defaultVal.map(a => new ListLookupRef());
  override def toString: String = "LIST-LOOKUP(" + inputs(0) + ")";
  override def emitDef: String = {
    var res = 
      "  always @(*) begin\n" +
      "    " + emitRef + " = " + inputs(1).emitRef + ";\n" +
      "    casez (" + inputs(0).emitRef + ")" + "\n";
    
    for ((addr, data) <- map) {
      res = res + "      " + addr.emitRef + " : \n";
      for ((w, e) <- wires zip data) 
        res = res + "        " + w.emitRef + " = " + e.emitRef + ";\n";
    }
    res = res + 
      "    endcase\n" +
      "  end\n";
    res
  }
  override def emitDefLoC: String = {
    var res = "";
    for ((addr, data) <- map) {
      res = res + "  if ((" + addr.emitRef + " == " + inputs(0).emitRef + ").to_bool()) {\n";
      for ((w, e) <- wires zip data)
	if(w.component != null)
          res = res + "    " + w.emitRef + "/*" + w.component + "*/" +  " = " + e.emitRef + ";\n";
      res = res + "  }\n";
    }
    res
  }
}
class ListLookupRef extends Delay {
  def lookup = inputs(0);
  def lookup_= (ll: ListLookup) = { inputs(0) = ll; }
  inputs += null;
  inferWidth = widthOf(0); // TODO: PROBABLY NOT RIGHT
  // override def toString: String = "W(" + name + ")"
  override def toString: String = name
  override def emitDef = "";
  override def emitDefLoC = "";
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";
  override def emitDecC: String = 
    "  dat_t<" + width + "> " + emitRef + ";\n";
}

object Op {
  def apply (name: String, nGrow: Int, widthInfer: (Node) => Int, a: Node, b: Node): Node = {
    val res = new Op();
    res.init("", widthInfer, a, b);
    res.op = name;
    res.nGrow = nGrow;
    res
  }
  def apply (name: String, nGrow: Int, widthInfer: (Node) => Int, a: Node): Node = {
    val res = new Op();
    res.init("", widthInfer, a);
    res.op = name;
    res.nGrow = nGrow;
    res
  }
}
class Op extends Node {
  var op: String = "";
  var nGrow: Int = 0;
  override def toString: String =
    if (inputs.length == 1)
      op + inputs(0)
    else
      inputs(0) + op + inputs(1)
  def emitOpRef (k: Int): String = {
    if (op == "<<") {
      if (k == 0 && inputs(k).width < width)
	"/* C */DAT<" + width + ">(" + inputs(k).emitRef + ")"
      else
	inputs(k).emitRef
    } else if (op == "##" || op == ">>" || op == ">>>" || op == "*")
      inputs(k).emitRef
    else {
      var w = 0;
      for (i <- 0 until nGrow)
	w = max(w, inputs(i).width);
      if (isCoercingArgs && nGrow > 0 && k < nGrow && w > inputs(k).width)
	"/* C */DAT<" + w + ">(" + inputs(k).emitRef + ")"
      else
	inputs(k).emitRef
    }
  }
  override def emitDef: String = {
    "  assign " + emitTmp + " = " + 
      (if (op == "##") 
        "{" + inputs(0).emitRef + ", " + inputs(1).emitRef + "}"
       else if (inputs.length == 1)
         op + " " + inputs(0).emitRef
       else
         inputs(0).emitRef + " " + op + " " + inputs(1).emitRef
      ) + ";\n"
  }
  override def emitDefLoC: String = {
    "  " + emitTmp + " = " + 
      (if (op == "##") 
        "cat<" + width + ">(" + emitOpRef(0) + ", " + emitOpRef(1) + ")"
       else if (inputs.length == 1)
         op + inputs(0).emitRef
       else
         emitOpRef(0) + " " + op + " " + emitOpRef(1)) +
    ";\n"
  }
}

object Probe {
  def apply(x: Node, name: String): Probe = { val res = new Probe(); res.init(name, widthOf(0), x); res }
  def apply(x: Node): Probe = apply(x, "");
}
class Probe extends Node {
  def my_name: String = if (name == "") inputs(0).emitRefV else name
  override def isProbe = true;
  override def toString: String =
    "Probe(" + inputs(0) + ", " + my_name + ")"
  override def emitDef: String = 
    "  assign " + emitTmp + " = " + inputs(0).emitRef + ";\n"
  override def emitDefLoC: String = 
    "  " + "printf(\"DBG " + my_name + ": %s\\n\", " + inputs(0).emitRef + ".to_str().c_str());\n" +
    "  " + emitTmp + " = " + inputs(0).emitRef + ";\n";
}

object Fill {
  def fillWidthOf(i: Int, n: Node) = { (m: Node) => m.inputs(i).width * n.maxNum }
  def apply (mod: Node, n: Node): Node = {
    val res = new Fill();
    res.init("", fillWidthOf(0, n), mod, n);
    res.n = n;
    res
  }
}
class Fill extends Node {
  var n: Node = null;
  override def toString: String = "FILL(" + inputs(0) + ", " + n + ")";
  override def emitDef: String = 
    "  assign " + emitTmp + " = {" + n.emitRef + "{" + inputs(0).emitRef + "}};\n";
  override def emitDefLoC: String = 
    if (n.isLit)
      "  " + emitTmp + " = " + inputs(0).emitRef + ".fill<" + width + "," + n.value + ">();\n";
    else
      "  " + emitTmp + " = " + inputs(0).emitRef + ".fill<" + width + ">(" + n.emitRef + ");\n";
}

object Log2 {
  // def log2WidthOf() = { (m: Node, n: Int) => log2(m.inputs(0).width) }
  def apply (mod: Node, n: Int): Node = {
    if (isEmittingComponents) {
      var res: Node = Lit(n);
      for (i <- 1 to n) 
        res = Mux(mod(i), Lit(i, sizeof(n)), res);
      res
    } else {
      val res = new Log2();
      res.init("", fixWidth(sizeof(n)), mod);
      res
    }
  }
}
class Log2 extends Node {
  override def toString: String = "LOG2(" + inputs(0) + ")";
  override def emitDefLoC: String = 
    "  " + emitTmp + " = " + inputs(0).emitRef + ".log2<" + width + ">();\n";
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
      "  " + emitTmp + " = " + inputs(0).emitRef + ".bit(" + hi.emitRef + ");\n"
    else
      "  " + emitTmp + " = " + inputs(0).emitRef + ".extract<" + width + ">(" + hi.emitRef + "," + lo.emitRef + ");\n"
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
    "  " + emitTmp + " = mux<" + width + ">(" + inputs(0).emitRef + ", " + inputs(1).emitRef + ", " + inputs(2).emitRef + ");\n"
  def ::(a: Node): Mux = { inputs(2) = a; this }
}

object Reg {
  def apply(n: String, u: Node): Reg = 
    new Reg().init(n, maxWidth _, u).asInstanceOf[Reg];
  def apply(u: Node): Reg = Reg("", u)
  def apply(n: String): Reg = Reg(n, null)
  def apply(n: String, w: Int): Reg = {
    val res = new Reg().init(n, fixWidth(w), null).asInstanceOf[Reg];
    res.width_ = w;
    res
  }
  def apply(w: Int): Reg = Reg("", w);
  def apply(): Reg = Reg("", null)
}
class Reg extends Delay {
  def updateVal = inputs(0);
  def resetVal  = inputs(1);
  def isReset  = inputs.length == 2;
  def isUpdate = !(updateVal == null);
  def update (x: Node) = { inputs(0) = x };
  def reset(init: Node): Reg = { 
    if (isReset)
      inputs(1) = init; 
    else
      inputs += init;
    this 
  }
  def <==(src: Node): Reg = {
    if (cond.length == 0)
      update(src);
    else if (!isUpdate) {
      var res = cond(0);
      for (i <- 1 until cond.length)
        res = cond(i) && res;
      // println(this.name + " <== " + res + " " + cond.length);
      // val res = cond.foldRight(Lit(1,1)){(a, b) => a&&b}
      update(Mux(res, src, this))
    } else {
      var res = cond(0);
      for (i <- 1 until cond.length)
        res = cond(i) && res;
      // println(this.name + " <== " + res + " " + cond.length);
      update(Mux(res, src, updateVal))
    }
    this
    // clauses += Pair(cond.head, src);
  }
  def nameOpt: String = if (name.length > 0) name else "REG"
  override def toString: String = {
    if (component.isWalking.contains(this)) 
      nameOpt
    else {
      component.isWalking += this;
      var res = nameOpt + "(";
      if (isUpdate) res = res + " " + updateVal;
      if (isReset)  res = res + " " + resetVal;
      res += ")";
      component.isWalking -= this;
      res;
    }
  }
  override def emitRefV: String = if (name == "") "R" + emitIndex else name;
  override def emitDef: String = "";
  override def emitReg: String =
    "    " + emitRef + " <= " + 
    (if (isReset) "reset ? " + resetVal.emitRef + " : " else "" ) + 
    updateVal.emitRef + ";\n"
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";

  override def emitDefLoC: String = 
    "  " + emitRef + "_shadow = " + 
    (if (isReset) "mux<" + width + ">(reset, " + resetVal.emitRef + ", " else "") + 
    updateVal.emitRef + (if (isReset) ");\n" else ";\n");
  override def emitDefHiC: String =
    "  " + emitRef + " = " + emitRef + "_shadow;\n";
  override def emitDecC: String = 
    "  dat_t<" + width + "> " + emitRef + ";\n" +
    "  dat_t<" + width + "> " + emitRef + "_shadow;\n";
}

/*
object Nodes {
  def apply (nodes: Node*): Nodes = new Nodes(nodes.toList);
  def unapplySeq(nodes: List[Node]): Nodes = 
}
class Nodes extends Node {
}
*/

}
