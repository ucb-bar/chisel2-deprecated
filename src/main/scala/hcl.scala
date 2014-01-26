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
import Literal._
import Node._
import ChiselError._

object when {
  def execWhen(cond: Bool)(block: => Unit) {
    conds.push(conds.top && cond);
    block;
    conds.pop();
  }
  def apply(cond: Bool)(block: => Unit): when = {
    execWhen(cond){ block }
    new when(cond);
  }
}

class when (prevCond: Bool) {
  def elsewhen (cond: Bool)(block: => Unit): when = {
    when.execWhen(!prevCond && cond){ block }
    new when(prevCond || cond);
  }
  def otherwise (block: => Unit) {
    val cond = !prevCond
    if (conds.length == 1) cond.canBeUsedAsDefault = true
    when.execWhen(cond){ block }
  }
}

object unless {
  def apply(c: Bool)(block: => Unit) {
    when (!c) { block }
  }
}

object switch {
  def apply(c: Bits)(block: => Unit) {
    keys.push(c);
    block;
    keys.pop();
  }
}
object is {
  def apply(v: Bits)(block: => Unit) {
    if (keys.length == 0) {
      ChiselError.error("NO KEY SPECIFIED");
    } else {
      val c = keys(0) === v;
      when (c) { block; }
    }
  }
  def apply(v: Bits, vr: Bits*)(block: => Unit) {
    if (keys.length == 0) {
      ChiselError.error("NO KEY SPECIFIED");
    } else {
      val c = vr.foldLeft(keys(0) === v)( (p: Bool, v: Bits) => keys(0) === v || p );
      when (c) { block; }
    }
  }
}

class TestIO(val format: String, val args: Seq[Data] = null)

object Scanner {
  def apply (format: String, args: Data*): TestIO =
    new TestIO(format, args.toList);
}
object Printer {
  def apply (format: String, args: Data*): TestIO =
    new TestIO(format, args.toList);
}

/**
  _chiselMain_ behaves as if it constructs an execution tree from
  the constructor of a sub class of Module which is passed as a parameter.
  That execution tree is simplified by aggregating all calls which are not
  constructors of a Module instance into the parent which is.
  The simplified tree (encoded through _Module.children_) forms the basis
  of the generated verilog. Each node in the simplified execution tree is
  a _Module_ instance from which a verilog module is textually derived.
  As an optimization, _Backend_ classes output modules which are
  textually equivalent only once and update a _Module_ instance's
  _moduleName_ accordingly.
*/
object chiselMain {
  def readArgs(args: Array[String]) {
    var i = 0;
    while (i < args.length) {
      val arg = args(i);
      arg match {
        case "--Wall" => {
          Module.saveWidthWarnings = true
          Module.saveConnectionWarnings = true
          Module.saveComponentTrace = true
          Module.isCheckingPorts = true
        }
        case "--wi" => Module.warnInputs = true
        case "--wo" => Module.warnOutputs = true
        case "--wio" => {Module.warnInputs = true; Module.warnOutputs = true}
        case "--Wwidth" => Module.saveWidthWarnings = true
        case "--Wconnection" => Module.saveConnectionWarnings = true
        case "--Wcomponent" => Module.saveComponentTrace = true
        case "--noCombLoop" => Module.dontFindCombLoop = true
        case "--genHarness" => Module.isGenHarness = true;
        case "--debug" => Module.isDebug = true;
        case "--ioDebug" => Module.isIoDebug = true;
        case "--noIoDebug" => Module.isIoDebug = false;
        case "--clockGatingUpdates" => Module.isClockGatingUpdates = true;
        case "--clockGatingUpdatesInline" => Module.isClockGatingUpdatesInline = true;
        case "--folding" => Module.isFolding = true;
        case "--vcd" => Module.isVCD = true;
        case "--v" => Module.backend = new VerilogBackend
        case "--moduleNamePrefix" => Backend.moduleNamePrefix = args(i + 1); i += 1
        case "--inlineMem" => Module.isInlineMem = true;
        case "--noInlineMem" => Module.isInlineMem = false;
        case "--backend" => {
          if (args(i + 1) == "v") {
            Module.backend = new VerilogBackend
          } else if (args(i + 1) == "c") {
            Module.backend = new CppBackend
          } else if (args(i + 1) == "flo") {
            Module.backend = new FloBackend
          } else if (args(i + 1) == "fpga") {
            Module.backend = new FPGABackend
          } else {
            Module.backend = Class.forName(args(i + 1)).newInstance.asInstanceOf[Backend]
          }
          i += 1
        }
        case "--compile" => Module.isCompiling = true
        case "--test" => Module.isTesting = true;
        case "--targetDir" => Module.targetDir = args(i + 1); i += 1;
        case "--include" => Module.includeArgs = Module.splitArg(args(i + 1)); i += 1;
        case "--checkPorts" => Module.isCheckingPorts = true
        case "--prune" => Module.isPruning = true
        case "--signalFile" => Module.signalFilename = args(i + 1) ; i += 1   // by Donggyu
        case any => ChiselError.warning("'" + arg + "' is an unkown argument.");
      }
      i += 1;
    }
  }

  def run[T <: Module] (args: Array[String], gen: () => T): T = apply(args, () => Module(gen())) // hack to avoid supplying default parameters and invoke Module.apply manually for invocation in sbt

  def apply[T <: Module]
      (args: Array[String], gen: () => T,
       scanner: T => TestIO = null, printer: T => TestIO = null, ftester: T => Tester[T] = null): T = {
    Module.initChisel();
    readArgs(args)

    try {
      val c = gen();
      if (scanner != null) {
        val s = scanner(c);
        Module.scanArgs  ++= s.args;
        for (a <- s.args) a.isScanArg = true
        Module.scanFormat  = s.format;
      }
      if (printer != null) {
        val p = printer(c);
        Module.printArgs   ++= p.args;
        for(a <- p.args) a.isPrintArg = true
        Module.printFormat   = p.format;
      }
      if (ftester != null) {
        Module.tester = ftester(c)
      }
      Module.backend.elaborate(c)
      if (Module.isCheckingPorts) Module.backend.checkPorts(c)
      if (Module.isCompiling && Module.isGenHarness) Module.backend.compile(c)
      if (Module.isTesting) Module.tester.tests()
      c
    } finally {
      ChiselError.report()
    }
  }
}


class ChiselException(message: String, cause: Throwable) extends Exception(message, cause)

object throwException {
  def apply(s: String, t: Throwable = null) = {
    val xcpt = new ChiselException(s, t)
    findFirstUserLine(xcpt.getStackTrace) foreach { u => xcpt.setStackTrace(Array(u)) }
    throw xcpt
  }
}

object chiselMainTest {
  def apply[T <: Module](args: Array[String], gen: () => T)(tester: T => Tester[T]): T =
    chiselMain(args, gen, null, null, tester)
}

trait proc extends Node {
  var updates = new collection.mutable.ListBuffer[(Bool, Node)];
  var updated = false
  def genCond(): Bool = conds.top;
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
    val (topCond, topValue) = updates.head
    val (lastCond, lastValue) = updates.last
    if (default == null && !topCond.isTrue && !lastCond.canBeUsedAsDefault) {
      ChiselError.error(
        {"NO DEFAULT SPECIFIED FOR WIRE: " + this + " in component " + this.component.getClass}, 
        this.line)
      return
    }
    if (default != null) {
      genMuxes(default, updates)
    } else {
      if (topCond.isTrue)
        genMuxes(topValue, updates.toList.tail)
      else if (lastCond.canBeUsedAsDefault)
        genMuxes(lastValue, updates)
    }
  }
  def procAssign(src: Node): Unit
  Module.procs += this;
}

trait nameable {
  var name: String = "";
  /** _named_ is used to indicates name was set explicitely
   and should not be overriden by a _nameIt_ generator. */
  var named = false;
}

abstract class BlackBox extends Module {
  Module.blackboxes += this

  def setVerilogParameters(string: String) {
    this.asInstanceOf[Module].verilog_parameters = string;
  }

  def setName(name: String) {
    moduleName = name;
  }
}


class Delay extends Node {
  override def isReg: Boolean = true;
}

