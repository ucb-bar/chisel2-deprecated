/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

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
  def apply(v: Bits)(block: => Unit) = {
    if (keys.length == 0) {
      println("NO KEY SPECIFIED");
    } else {
      val c = keys(0) === v;
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

/**
  _chiselMain_ behaves as if it constructs an execution tree from
  the constructor of a sub class of Component which is passed as a parameter.
  That execution tree is simplified by aggregating all calls which are not
  constructors of a Component instance into the parent which is.
  The simplified tree (encoded through _Component.children_) forms the basis
  of the generated verilog. Each node in the simplified execution tree is
  a _Component_ instance from which a verilog module is textually derived.
  As an optimization, _Backend_ classes output modules which are
  textually equivalent only once and update a _Component_ instance's
  _moduleName_ accordingly.
*/
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
        case "--vcd" => isVCD = true;
        case "--v" => backend = new VerilogBackend
        case "--moduleNamePrefix" => Backend.moduleNamePrefix = args(i + 1); i += 1
        case "--inlineMem" => isInlineMem = true;
        case "--noInlineMem" => isInlineMem = false;
        case "--backend" => {
          if (args(i + 1) == "v") {
            backend = new VerilogBackend
          } else if (args(i + 1) == "c") {
            backend = new CppBackend
          } else if (args(i + 1) == "flo") {
            backend = new FloBackend
          } else if (args(i + 1) == "fpga") {
            backend = new FPGABackend
          } else {
            backend = Class.forName(args(i + 1)).newInstance.asInstanceOf[Backend]
          }
          i += 1
        }
        case "--compile" => isCompiling = true
        case "--test" => isTesting = true;
        case "--targetDir" => targetDir = args(i + 1); i += 1;
        case "--include" => includeArgs = splitArg(args(i + 1)); i += 1;
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

    try {
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
    } finally {
      ChiselError.report()
    }
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
  var updates = new collection.mutable.ListBuffer[(Bool, Node)];
  def genCond() = conds.top;
  def genMuxes(default: Node, others: Seq[(Bool, Node)]): Unit = {
    val update = others.foldLeft(default)((v, u) => Multiplex(u._1, u._2, v))
    if (inputs.isEmpty) inputs += update else inputs(0) = update
  }
  def genMuxes(default: Node): Unit = {
    if (updates.length == 0) {
      if (inputs.length == 0 || inputs(0) == null) {
        ChiselError.error({"NO UPDATES ON " + this}, this.line)
      }
      return
    }
    val (lastCond, lastValue) = updates.head
    if (default == null && !lastCond.isTrue) {
      ChiselError.error({"NO DEFAULT SPECIFIED FOR WIRE: " + this}, this.line)
      return
    }
    if (default != null) {
      genMuxes(default, updates)
    } else {
      genMuxes(lastValue, updates.toList.tail)
    }
  }
  def procAssign(src: Node);
  procs += this;
}

trait nameable {
  var name: String = "";
  /** _named_ is used to indicates name was set explicitely
   and should not be overriden by a _nameIt_ generator. */
  var named = false;
}

abstract class BlackBox extends Component {
  parent.blackboxes += this;

  def setVerilogParameters(string: String) =
    this.asInstanceOf[Component].verilog_parameters = string;

  def setName(name: String) = {
    moduleName = name;
  }
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

class Log2 extends Node {
  override def toString: String = "LOG2(" + inputs(0) + ")";
}
