// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue

import Component._;
import Literal._;
import Node._;
import ChiselError._;
  
object Enum {
  def apply(l: List[Symbol]) = (l zip (Range(0, l.length, 1).map(x => UFix(x, sizeof(l.length-1))))).toMap;
  def apply(l: Symbol *) = (l.toList zip (Range(0, l.length, 1).map(x => UFix(x, sizeof(l.length-1))))).toMap;
  def apply[T <: Bits](n: Int)(gen: => T) = (Range(0, n, 1).map(x => (Lit(x, sizeof(n-1))(gen)))).toList;
}

object fromNode {
  def apply[T <: Data: Manifest](data: Node): T = {
    val resT = Fab[T]();
    resT.fromNode(data);
  }
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
  def apply(v: Bits)(block: => Unit) = {
    if (keys.length == 0) 
      println("NO KEY SPECIFIED");
    else {
      val c = keys(0) === v;
      when (c) { block; }
    }
  }
}

class TestIO(val format: String, val args: Seq[Data] = null) { }

object Scanner {
  def apply (format: String, args: Data*) = 
    new TestIO(format, args.toList);
}
object Printer {
  def apply (format: String, args: Data*) = 
    new TestIO(format, args.toList);
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
        }
        case "--Wwidth" => saveWidthWarnings = true
        case "--Wconnection" => saveConnectionWarnings = true
        case "--Wcomponent" => saveComponentTrace = true
        case "--noCombLoop" => dontFindCombLoop = true
        case "--gen-harness" => isGenHarness = true; 
        case "--debug" => isDebug = true; 
        case "--ioDebug" => isIoDebug = true; 
        case "--noIoDebug" => isIoDebug = false; 
        case "--clockGatingUpdates" => isClockGatingUpdates = true; 
        case "--clockGatingUpdatesInline" => isClockGatingUpdatesInline = true; 
        case "--folding" => isFolding = true; 
        case "--vcd" => isVCD = true;
        case "--v" => backendName = "v"; isEmittingComponents = true; isCoercingArgs = false;
        case "--target-dir" => targetDir = args(i+1); i += 1;
	case "--include" => includeArgs = splitArg(args(i+1)); i += 1;
        case any => println("UNKNOWN ARG");
      }
      i += 1;
    }
  }

  def apply[T <: Component]
      (args: Array[String], gen: () => T, scanner: T => TestIO = null, printer: T => TestIO = null): T = {
    initChisel();
    readArgs(args)

    val c = gen();
    if (scanner != null) {
      val s = scanner(c);
      scanArgs   = s.args;
      scanFormat = s.format;
    }
    if (printer != null) {
      val p = printer(c);
      printArgs   ++= p.args;
      printFormat = p.format;
    }
    backendName match {
    case "v" => c.compileV();
    case "c" => c.compileC();
    }
    c
  }
}


abstract class Data extends Node with Cloneable{
  var comp: proc = null;
  def toFix(): Fix = chiselCast(this){Fix()};
  def toUFix(): UFix = chiselCast(this){UFix()};
  def toBits(): Bits = chiselCast(this){Bits()};
  def toBool(): Bool = chiselCast(this){Bool()};
  def setIsTypeNode = isTypeNode = true;
  def apply(name: String): Data = null
  def flatten = Array[(String, Bits)]();
  def terminate(): Unit = { }
  def flip(): this.type = this;
  def asInput(): this.type = this;
  def asOutput(): this.type = this;
  def toNode: Node = this;
  def fromNode(n: Node): this.type = this;
  def :=[T <: Data](data: T) = {
    if(this.getClass != data.getClass) println("Mismatched types: " + this.getClass + " " + data.getClass);
    comp procAssign data.toNode;
  }
  override def clone(): this.type = {
    val res = this.getClass.newInstance.asInstanceOf[this.type];
    res
  }
  override def name_it(path: String, setNamed: Boolean = false) = {
    if (isTypeNode && comp != null) 
      comp.name_it(path, setNamed)
    else
      super.name_it(path, setNamed);
  }
  def setWidth(w: Int) = this.width = w;
}

trait proc extends Node {
  var isDefaultNeeded = true;
  var updates = new Queue[(Bool, Node)];
  def genCond() = conds.top;
  def genMuxes(default: Node) = {
    if (updates.length == 0) {
      if (inputs.length == 0 || inputs(0) == null){

	ChiselErrors += ChiselError({"NO UPDATES ON " + this}, this); 
      }
    } else {
      val (lastCond, lastValue) = updates.front;
      if (isDefaultNeeded && default == null && !lastCond.isTrue) {
        ChiselErrors += ChiselError({"NO DEFAULT SPECIFIED FOR WIRE: " + this}, this)
      }
      val (start, firstValue) = 
        if (default != null) 
          (0, default)
        else 
          (1, lastValue)
      if(inputs.length > 0)
	inputs(0) = firstValue;
      else
	inputs   += firstValue;

      var startCond: Bool = null
      def isEquals(x: Node, y: Node): Boolean = {
        if(x.litOf != null && y.litOf != null)
          x.litOf.value == y.litOf.value
        else
          x.equals(y)
      }

      for (i <- start until updates.size) {
        val (cond, value) = updates(i);
        if(i == updates.size-1 || !isEquals(updates(i+1)._2, value)) {
          if(startCond == null) {
            inputs(0) = Multiplex(cond, value, inputs(0));
          } else {
            inputs(0) = Multiplex(startCond || cond, value, inputs(0))
            startCond = null
          }
        } else {
          if(startCond == null)
            startCond = cond
          else
            startCond = startCond || cond
        }
      }
    }
  }
  def procAssign(src: Node);
  procs += this;
}

trait nameable {
  var name: String = "";
  var named = false;
}

object nullADT extends Data;


abstract class BlackBox extends Component {
  parent.blackboxes += this;
  var moduleNameSet = false;
  override def doCompileV(out: java.io.FileWriter, depth: Int): Unit = {
    traceNodes();
  }

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




object MuxLookup {
  def apply[S <: Bits, T <: Data] (key: S, default: T, mapping: Seq[(S, T)]): T = {
    var res = default;
    for ((k, v) <- mapping.reverse)
      res = Mux(key === k, v, res);
    res
  }

}

object MuxCase {
  def apply[T <: Data] (default: T, mapping: Seq[(Bool, T)]): T = {
    var res = default;
    for ((t, v) <- mapping.reverse){
      res = Mux(t, v, res);
    }
    res
  }
}

object Log2 {
  def apply (mod: UFix, n: Int): UFix = {
    if (isEmittingComponents) {
      var res = UFix(0);
      for (i <- 1 to n)
        res = Mux(mod(i), UFix(i, sizeof(n)), res);
      res
    } else {
      val log2Cell = new Log2Cell(n);
      log2Cell.io.in := mod;
      log2Cell.io.out
    }
  }
}
class Log2 extends Node {
  override def toString: String = "LOG2(" + inputs(0) + ")";
  override def emitDefLoC: String =
    " " + emitTmp + " = " + inputs(0).emitRef + ".log2<" + width + ">();\n";
}
class Log2Cell(n: Int) extends Cell {
  val io = new Bundle{
    val in = UFix(INPUT);
    val out = UFix(OUTPUT);
  }
  io.setIsTypeNode;
  val primitiveNode = new Log2();
  primitiveNode.init("", fixWidth(sizeof(n)), io.in);
  io.out assign primitiveNode;
}

}

