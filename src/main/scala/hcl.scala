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

import ChiselError._


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
          } else if (args(i + 1) == "dot") {
            Module.backend = new DotBackend
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
        case any => ChiselError.warning("'" + arg + "' is an unkown argument.");
      }
      i += 1;
    }
  }

  def run[T <: Module] (args: Array[String], gen: () => T): T = apply(args, () => Module(gen())) // hack to avoid supplying default parameters and invoke Module.apply manually for invocation in sbt

  def apply[T <: Module]
      (args: Array[String], gen: () => T,
       ftester: T => Tester[T] = null): T = {
    Module.initChisel();
    readArgs(args)

    try {
      val c = gen();
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
    chiselMain(args, gen, tester)
}

trait nameable {
  var name: String = "";

  /** _named_ is used to indicates name was set explicitely
   and should not be overriden by a _nameIt_ generator. */
  var named = false;
}



