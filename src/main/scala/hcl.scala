// author: jonathan bachrach
package Chisel {

import scala.collection.mutable.ArrayBuffer

import Component._;
import Lit._;
import Node._;
  
class Update(val reg: Node, val update: Node) {
}
class Rule(val cond: Node) {
  val updates = new ArrayBuffer[Update]();
  def addUpdate(update: Update) = updates += update;
}


object Enum {
  def apply(l:List[Symbol]) = (l zip (Range(0, l.length, 1).map(x => Lit(x, sizeof(l.length-1))))).toMap;
  def apply(l: Symbol *) = (l.toList zip (Range(0, l.length, 1).map(x => Lit(x, sizeof(l.length-1))))).toMap;
}
object Cat {
  def apply (mod: Node, mods: Node*): Node = 
    if(isEmittingComponents) {
      val res = new Cat();
      res.initOf("", sumWidth _, mod :: mods.toList);
      res
    } else
      mods.foldLeft(mod){(a, b) => a ## b};
}
class Cat extends Node {
  override def emitDef: String = {
    var res = "  assign " + emitTmp + " = {";
    var first = true;
    for(node <- inputs)
      res += (if(first) {first = false; ""} else ", ") + node.emitRef;
    res += "};\n";
    res
  }
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
	case "--include" => includeArgs = splitArg(args(i+1)); i += 1;
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


abstract class Interface extends Node {
  def apply(name: String): Interface = null
  def flatten = Array[(String, IO)]();
  def flip(): this.type = this;
}


abstract class BlackBox extends Component {
  override def doCompileV(out: java.io.FileWriter, depth: Int): Unit = {
    name_it();
    io.name_it("");
  }
}


class Delay extends Node {
  override def isReg = true;
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
      var res: Node = Lit(0);
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



/*
object Nodes {
  def apply (nodes: Node*): Nodes = new Nodes(nodes.toList);
  def unapplySeq(nodes: List[Node]): Nodes = 
}
class Nodes extends Node {
}
*/

}
