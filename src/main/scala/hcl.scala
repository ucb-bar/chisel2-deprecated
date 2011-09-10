// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

import Component._;
import Literal._;
import Node._;
import IOdir._;
  
object Enum {
  def apply(l: List[Symbol]) = (l zip (Range(0, l.length, 1).map(x => Fix(x, sizeof(l.length-1))))).toMap;
  def apply(l: Symbol *) = (l.toList zip (Range(0, l.length, 1).map(x => Fix(x, sizeof(l.length-1))))).toMap;
  def apply(n: Int) = (Range(0, n, 1).map(x => Fix(x, sizeof(n-1)))).toList;
}

object fromNode {
  def apply[T <: dat_t: Manifest](data: Node): T = {
    val resT = Fab[T]();
    resT.fromNode(data);
  }
}

object when {
  def apply(c: Bool)(block: => Unit) = {
    conds.push(c); 
    block; 
    conds.pop();
  }
}
object unless {
  def apply(c: Bool)(block: => Unit) = 
    when (!c) { block }
}

// object pcond {
//   def apply(cases: Seq[(Fix, () => Any)]) = {
//     var tst: Fix = Fix(1);
//     for ((ctst, block) <- cases) {
//       cond.push(tst && ctst);  
//       block(); 
//       cond.pop();
//       tst = tst && !ctst;
//     }
//     this;
//   }
// }
// object pcase {
//   def apply(x: Fix, cases: Seq[(Fix, () => Any)]) = 
//     pcond(cases.map(tb => (tb._1 === x, tb._2)))
//   def apply(x: Fix, default: () => Any, cases: Seq[(Fix, () => Any)]) = {
//     val elts = cases.map(tb => (tb._1 === x, tb._2)).toList;
//     pcond(elts ::: List((Fix(1), default)))

object otherwise {
  def apply(block: => Unit) = 
    when (Bool(true)) { block }
}
object switch {
  def apply(c: Node)(block: => Unit) = {
    keys.push(c); 
    block; 
    keys.pop();
  }
}
object is {
  def apply(v: Node)(block: => Unit) = {
    if (keys.length == 0) 
      println("NO KEY SPECIFIED");
    else {
      val c = new Bool();
      c := keys(0) === v;
      when (c) { block; }
    }
  }
}
class TstObject(val scanFormat: String, 
     val scan_args: Seq[Node] = null,
     val printFormat: String,            
     val print_args: Seq[Node] = null) { }

object chisel_main {
  def apply[T <: Component]
    (args: Array[String], 
     gen: () => T,
     tst: T => TstObject = null) {
    initChisel();
    var i = 0;
    while (i < args.length) {
      val arg = args(i);
      arg match {
        case "--gen-harness" => isGenHarness = true; 
        case "--v" => isEmittingComponents = true; isCoercingArgs = false;
        case "--target-dir" => targetDir = args(i+1); i += 1;
        // case "--scan-format" => scanFormat = args(i+1); i += 1;
        // case "--print-format" => printFormat = args(i+1); i += 1;
	case "--include" => includeArgs = splitArg(args(i+1)); i += 1;
        // case "--is-coercing-args" => isCoercingArgs = true;
        case any => println("UNKNOWN ARG");
      }
      i += 1;
    }
    val c = gen();
    if (tst != null) {
      val tstObj = tst(c);
      scanArgs = tstObj.scan_args;
      printArgs = tstObj.print_args;
      scanFormat = tstObj.scanFormat;
      printFormat = tstObj.printFormat;
    }
    if (isEmittingComponents)
      c.compileV();
    else
      c.compileC();
  }
}


abstract class dat_t extends Node {
  var comp: proc = null;
  def toFix(): Fix = chiselCast(this){Fix()};
  def toUFix(): UFix = chiselCast(this){UFix()};
  def toBits(): Bits = chiselCast(this){Bits()};
  def toBool(): Bool = chiselCast(this){Bool()};
  def setIsCellIO = isCellIO = true;
  def apply(name: String): dat_t = null
  def flatten = Array[(String, IO)]();
  def flip(): this.type = this;
  def asInput(): this.type = this;
  def asOutput(): this.type = this;
  def toNode: Node = this;
  def fromNode(n: Node): this.type = this;
  def <==[T <: dat_t](data: T) = {
    data.setIsCellIO;
    comp <== data.toNode;
  }
  override def clone(): this.type = {
    val res = this.getClass.newInstance.asInstanceOf[this.type];
    res
  }
  override def name_it(path: String, setNamed: Boolean = false) = {
    if (isCellIO && comp != null) 
      comp.name_it(path, setNamed)
    else
      super.name_it(path, setNamed);
  }
  def setWidth(w: Int) = this.width = w;
  override def unary_-()= this;
  override def unary_~()= this;
  override def unary_!()= this;
  def << (b: dat_t) = this;
  def >> (b: dat_t) = this;
  def >>>(b: dat_t) = this;
  def +  (b: dat_t) = this;
  def *  (b: dat_t) = this;
  def ^  (b: dat_t) = this;
  def ?  (b: dat_t) = this;
  def -  (b: dat_t) = this;
  def ## (b: dat_t) = this;
  def ===(b: dat_t) = this;
  def != (b: dat_t) = this;
  def >  (b: dat_t) = this;
  def <  (b: dat_t) = this;
  def <= (b: dat_t) = this;
  def >= (b: dat_t) = this;
  def && (b: dat_t) = this;
  def || (b: dat_t) = this;
  def &  (b: dat_t) = this;
  def |  (b: dat_t) = this;
}

trait proc extends Node {
  var updates = new Stack[(Bool, Node)];
  def genMuxes(default: Node) = {
    if (updates.length == 0) {
      if (inputs.length == 0 || inputs(0) == null)
        println("NO UPDATES SPECIFIED"); // error();
    } else {
      val (lastCond, lastValue) = updates.pop();
      val backstop: Node =
        if (lastCond.isTrue) {
          if (lastValue == null)
            println("NO CONDITIONAL UPDATE DEFAULT"); // error();
          lastValue
        } else if (lastValue == null) {
	  println("here2");
          if (default == null)
            println("NO CONDITIONAL UPDATE DEFAULT"); // error();
          default
        } else {
          lastValue
	}
      updates.push((lastCond, backstop));
      inputs(0) = if(default != null) default else backstop;
      for ((cond, value) <- updates) {
        inputs(0) = Multiplex(cond, value, inputs(0));
      }
    }
  }
  def <==(src: Node);
  procs += this;
}

trait nameable {
  var name: String = "";
  var named = false;
}

object nullADT extends dat_t;


abstract class BlackBox extends Component {
  override def doCompileV(out: java.io.FileWriter, depth: Int): Unit = {
    findNodes(depth, this);
  }
  override def name_it() = {
    val cname = getClass().getName();
    val dotPos = cname.lastIndexOf('.');
    moduleName = if (dotPos >= 0) cname.substring(dotPos+1) else cname;
  }
}


class Delay extends Node {
  override def isReg = true;
}




object MuxLookup {
/*
  def apply (key: Node, default: Node, mapping: Seq[(Node, Node)]): Node = {
    var res = default;
    for ((k, v) <- mapping.reverse)
      res = Mux(key === k, v, res);
    res
  }
  * */

  def apply[T <: dat_t] (key: Fix, default: T, mapping: Seq[(Fix, T)]): T = {
    var res = default;
    for ((k, v) <- mapping.reverse)
      res = Mux(key ===k, v, res);
    res
  }

}

object MuxCase {
/*
  def apply (default: Node, mapping: Seq[(Node, Node)]): Node = {
    var res = default;
    for ((t, v) <- mapping.reverse){
      res = Mux(t, v, res);
    }
    res
  }
  * */
  def apply[T <: dat_t] (default: T, mapping: Seq[(Fix, T)]): T = {
    var res = default;
    for ((t, v) <- mapping.reverse){
      res = Mux(t, v, res);
    }
    res
  }
}



object Log2 {
  // def log2WidthOf() = { (m: Node, n: Int) => log2(m.inputs(0).width) }
  def apply (mod: Node, n: Int): Node = {
    if (isEmittingComponents) {
      var res: Node = Fix(0);
      for (i <- 1 to n) 
        res = Multiplex(mod(i), Fix(i, sizeof(n)), res);
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



/*
object Nodes {
  def apply (nodes: Node*): Nodes = new Nodes(nodes.toList);
  def unapplySeq(nodes: List[Node]): Nodes = 
}
class Nodes extends Node {
}
*/

}

