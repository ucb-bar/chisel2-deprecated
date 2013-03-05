package Chisel
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.{Queue=>ScalaQueue}
import Component._
import Literal._
import Node._
import ChiselError._
  
object Enum {
  def apply(l: List[Symbol]) = (l zip (Range(0, l.length, 1).map(x => UFix(x, sizeof(l.length-1))))).toMap;
  def apply(l: Symbol *) = (l.toList zip (Range(0, l.length, 1).map(x => UFix(x, sizeof(l.length-1))))).toMap;
  def apply[T <: Bits](n: Int)(gen: => T) = (Range(0, n, 1).map(x => (Lit(x, sizeof(n-1))(gen)))).toList;
}

object when {
  def execWhen(cond: Bool)(block: => Unit) = {
    conds.push(conds.top && cond);
    block;
    conds.pop(); 
  }
  def apply(cond: Bool)(block: => Unit) = {
    execWhen(cond){ block }
    new when(cond);
  }
}
class when (prevCond: Bool) {
  def elsewhen (cond: Bool)(block: => Unit) = {
    when.execWhen(!prevCond && cond){ block }
    new when(prevCond || cond);
  }
  def otherwise (block: => Unit) = {
    when.execWhen(!prevCond){ block }
  }
}

object unless {
  def apply(c: Bool)(block: => Unit) = 
    when (!c) { block }
}

object otherwise {
  def apply(block: => Unit) = 
    when (Bool(true)) { block }
}
object switch {
  def apply(c: Bits)(block: => Unit) = {
    keys.push(c); 
    block; 
    keys.pop();
  }
}
object is {
  def apply(v: Bits, vs: Bits*)(block: => Unit) = {
    if (keys.length == 0) 
      println("NO KEY SPECIFIED");
    else {
      val ts = vs.map(keys(0) === _)
      val c = ts.foldLeft(keys(0) === v){ _ || _ };
      when (c) { block; }
    }
  }
}

class TestIO(val format: String, val args: Seq[Data] = null)

object Scanner {
  def apply (format: String, args: Data*) = 
    new TestIO(format, args.toList);
}
object Printer {
  def apply (format: String, args: Data*) = 
    new TestIO(format, args.toList);
}

object withFame1 {
  def fireChildren(isTop: Boolean, c: Component): Unit = {
    println("ADDING IS-FIRE\n")
    if (!isTop)
      c.isFire := c.parent.isFire
    c.io.elements
    c.io.elementsCache += (("is_fire", c.isFire))
    for (e <- c.children)
      fireChildren(false, e)
  }
  def apply()(f: => Component) = {
    val x = isFame1; 
    isFame1 = true; 
    val c = f; 
    fireChildren(true, c)
    isFame1 = x; 
    c
  }
}
 
object chiselMain {
  def readArgs(args: Array[String]) = {
    var i = 0;
    while (i < args.length) {
      val arg = args(i);
      arg match {
        case "--Wall" => {
          saveWidthWarnings = true
          saveConnectionWarnings = true
          saveComponentTrace = true
          isCheckingPorts = true
        }
        case "--Wwidth" => saveWidthWarnings = true
        case "--Wconnection" => saveConnectionWarnings = true
        case "--Wcomponent" => saveComponentTrace = true
        case "--noCombLoop" => dontFindCombLoop = true
        case "--genHarness" => isGenHarness = true; 
        case "--debug" => isDebug = true; 
        case "--ioDebug" => isIoDebug = true; 
        case "--noIoDebug" => isIoDebug = false; 
        case "--clockGatingUpdates" => isClockGatingUpdates = true; 
        case "--clockGatingUpdatesInline" => isClockGatingUpdatesInline = true; 
        case "--folding" => isFolding = true; 
        case "--noFolding" => isFolding = false; 
        case "--vcd" => isVCD = true;
        case "--v" => backend = new VerilogBackend
        case "--moduleNamePrefix" => moduleNamePrefix = args(i+1); i += 1
        case "--inlineMem" => isInlineMem = true;
        case "--noInlineMem" => isInlineMem = false;
        case "--backend" => {
          if (args(i+1) == "v")
            backend = new VerilogBackend
          else if (args(i+1) == "c")
            backend = new CppBackend
          else if (args(i+1) == "flo")
            backend = new FloBackend
          else if (args(i+1) == "fpga")
            backend = new FPGABackend
          else
            backend = Class.forName(args(i+1)).newInstance.asInstanceOf[Backend]
          i += 1
        }
        case "--compile" => isCompiling = true
        case "--test" => isTesting = true;
        case "--targetDir" => targetDir = args(i+1); i += 1;
        case "--include" => includeArgs = splitArg(args(i+1)); i += 1;
        case "--checkPorts" => isCheckingPorts = true
        case any => println("UNKNOWN CONSOLE ARG");
      }
      i += 1;
    }
  }

  def run[T <: Component] (args: Array[String], gen: () => T): T = apply(args, gen) // hack to avoid supplying default parameters manually for invocation in sbt

  def apply[T <: Component]
      (args: Array[String], gen: () => T, 
       scanner: T => TestIO = null, printer: T => TestIO = null, ftester: T => Tester[T] = null): T = {
    initChisel();
    readArgs(args)

    val c = gen();
    if (scanner != null) {
      val s = scanner(c);
      scanArgs  ++= s.args;
      for (a <- s.args) a.isScanArg = true
      scanFormat  = s.format;
    }
    if (printer != null) {
      val p = printer(c);
      printArgs   ++= p.args;
      for(a <- p.args) a.isPrintArg = true
      printFormat   = p.format;
    }
    if (ftester != null) {
      tester = ftester(c)
    }
    backend.elaborate(c)
    if (isCheckingPorts) backend.checkPorts(c)
    if (isCompiling && isGenHarness) backend.compile(c)
    if (isTesting) tester.tests()
    c
  }
}

object throwException {
  def apply(s: String) = {
    val xcpt = new Exception(s)
    val st = xcpt.getStackTrace
    val usrStart = findFirstUserInd(st)
    val usrEnd = if(usrStart == 0) st.length else usrStart + 1
    xcpt.setStackTrace(st.slice(usrStart, usrEnd))
    throw xcpt    
  }
}

object chiselMainTest {
  def apply[T <: Component](args: Array[String], gen: () => T)(tester: T => Tester[T]): T = 
    chiselMain(args, gen, null, null, tester)
}

trait proc extends Node {
  var isDefaultNeeded = true;
  var updates = new ScalaQueue[(Bool, Node)];
  def genCond() = conds.top
  def genDelayCond() = {
    val c = conds.top
    if (isFame1) fame1fire && c else c;
  }
  def genMuxes(default: Node, others: Seq[(Bool, Node)]): Unit = {
    val update = others.foldLeft(default)((v, u) => Multiplex(u._1, u._2, v))
    if (inputs.isEmpty) inputs += update else inputs(0) = update
  }
  def genMuxes(default: Node): Unit = {
    if (updates.length == 0) {
      if (inputs.length == 0 || inputs(0) == null)
        ChiselErrors += ChiselError({"NO UPDATES ON " + this}, this)
      return
    }
    val (lastCond, lastValue) = updates.head
    if (default == null && !lastCond.isTrue) {
      ChiselErrors += ChiselError({"NO DEFAULT SPECIFIED FOR WIRE: " + this}, this)
      return
    }
    if (default != null)
      genMuxes(default, updates)
    else
      genMuxes(lastValue, updates.toList.tail)
  }
  def procAssign(src: Node);
  procs += this;
}

trait nameable {
  var name: String = "";
  var named = false;
}

abstract class BlackBox extends Component {
  parent.blackboxes += this;
  var moduleNameSet = false;

  def setVerilogParameters(string: String) = 
    this.asInstanceOf[Component].verilog_parameters = string;

  override def name_it() = {
    if(!moduleNameSet) {
      val cname = getClass().getName();
      val dotPos = cname.lastIndexOf('.');
      moduleName = if (dotPos >= 0) cname.substring(dotPos+1) else cname;
    }
  }
  def setName(name: String) = {moduleName = name; moduleNameSet = true}
}


class Delay extends Node {
  override def isReg = true;
}

object Log2 {
  def apply (mod: UFix, n: Int): UFix = {
    backend match {
      case x: CppBackend => {
        val log2 = new Log2()
        log2.init("", fixWidth(sizeof(n-1)), mod)
        log2.setTypeNode(UFix())
      }
      case _ => {
        var res = UFix(0);
        for (i <- 1 until n)
          res = Mux(mod(i), UFix(i, sizeof(n-1)), res);
        res
      }
    }
  }
}

object Log2Node {
  def apply (mod: Node): Node = {
    val log2 = new Log2()
    val n = mod.width;
    log2.init("", sizeof(n-1), mod)
    log2
  }
}
class Log2 extends Node {
  override def toString: String = "LOG2(" + inputs(0) + ")";
  override def genSubNodes = {
    val zero_log2s = (backend.words(inputs(0))-1 to 1 by -1).
      map(i => (Op("!=", 1, inputs(0).getSubNode(i), Literal(0)), Log2Node(inputs(0).getSubNode(i))))
    setSubNode(0, zero_log2s.foldRight(Log2Node(inputs(0).getSubNode(0)))((zl, r) => Multiplex(zl._1, zl._2, r)));
  }
}
