/*
 Copyright (c) 2011 - 2016 The Regents of the University of
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
import scala.util.Properties
import scala.collection.mutable.{ArrayBuffer, HashMap}

class TestIO(val format: String, val args: Seq[Data] = Seq())
object Scanner {
  def apply (format: String, args: Data*): TestIO = new TestIO(format, args.toList)
}
object Printer {
  def apply (format: String, args: Data*): TestIO = new TestIO(format, args.toList)
}

/**
  _chiselMain_ behaves as if it constructs an execution tree from
  the constructor of a sub class of Module which is passed as a parameter.
  That execution tree is simplified by aggregating all calls which are not
  constructors of a Module instance into the parent which is.
  The simplified tree (encoded through _Driver.children_) forms the basis
  of the generated verilog. Each node in the simplified execution tree is
  a _Module_ instance from which a verilog module is textually derived.
  As an optimization, _Backend_ classes output modules which are
  textually equivalent only once and update a _Module_ instance's
  _moduleName_ accordingly.
*/
object chiselMain {
  val wrapped = true
  val unwrapped = false

  def apply[T <: Module](args: Array[String], gen: () => T): T =
    Driver(args, gen, wrapped)

  def apply[T <: Module](args: Array[String], gen: () => T, ftester: T => Tester[T]): T =
    Driver(args, gen, ftester, wrapped)

  // Assumes gen needs to be wrapped in Module()
  def run[T <: Module] (args: Array[String], gen: () => T): T =
    Driver(args, gen, unwrapped)

  def run[T <: Module] (args: Array[String], gen: () => T, ftester: T => Tester[T]): T =
    Driver(args, gen, ftester, unwrapped)
}

//Is this antiquated?
object chiselMainTest {
  def apply[T <: Module](args: Array[String], gen: () => T)(tester: T => Tester[T]): T =
    chiselMain(args, gen, tester)
}

class ChiselException(message: String, cause: Throwable) extends Exception(message, cause)

object throwException {
  def apply(s: String, t: Throwable = null) = {
    val xcpt = new ChiselException(s, t)
    ChiselError.findFirstUserLine(xcpt.getStackTrace) foreach { u => xcpt.setStackTrace(Array(u)) }
    // Record this as an error (for tests and error reporting).
    ChiselError.error(s)
    throw xcpt
  }
}


trait proc extends Node {
  protected var procAssigned = false

  protected[Chisel] def verifyMuxes: Unit = {
    if (!defaultRequired && (inputs.length == 0 || inputs(0) == null))
      ChiselError.error({"NO UPDATES ON " + this}, this.line)
    if (defaultRequired && defaultMissing)
      ChiselError.error({"NO DEFAULT SPECIFIED FOR WIRE: " + this + " in component " + this.component.getClass}, this.line)
  }

  protected[Chisel] def doProcAssign(src: Node, cond: Bool): Unit = {
    if (procAssigned) {
      inputs(0) = Multiplex(cond, src, inputs(0))
    } else if (cond.litValue() != 0) {
      procAssigned = true
      val mux = Multiplex(cond, src, default)
      if (inputs.isEmpty) inputs += mux
      else { require(inputs(0) == default); inputs(0) = mux }
    }
  }

  protected[Chisel] def procAssign(src: Node): Unit =
    doProcAssign(src, Module.current.whenCond)

  protected[Chisel] def muxes: Seq[Mux] = {
    def traverse(x: Node): List[Mux] = x match {
      case m: Mux => m :: (if (m.inputs(2) eq default) Nil else traverse(m.inputs(2)))
      case _ => Nil
    }
    traverse(inputs(0))
  }

  protected[Chisel] def nextOpt = if (getNode.inputs.isEmpty) None else Some(getNode.inputs(0).getNode)
  protected[Chisel] def next = nextOpt getOrElse throwException("Input is empty")
  protected def default = if (defaultRequired) null else this
  protected def defaultRequired: Boolean = false
  protected def defaultMissing: Boolean =
    procAssigned && inputs(0).isInstanceOf[Mux] && (muxes.last.inputs(2) eq default)
  protected def setDefault(src: Node): Unit = muxes.last.inputs(2) = src
}

/** This trait allows an instantiation of something to be given a particular name */
trait Nameable {
  /** Name of the instance. */
  var name: String = ""
  /** named is used to indicate that name was set explicitly and should not be overriden */
  var named = false
  /** Set the name of this module to the string 'n'
    * @example {{{ my.io.node.setName("MY_IO_NODE") }}}
    */
  def setName(n: String) { name = n ; named = true }
}

/** This class enables the definition of verilog parameters without having to to string building
  * @example
  * {{{ class MyParams extends VerilogParameters {
  *   val MY_STR_PARAM = "FOO_BAR"
  *   val MY_INT_PARAM = 3
  *   val MY_BOOL_PARAM = true
  * } }}}
  * This will generate a string
  * {{{ #(
  *   .MY_STR_PARAM("FOO_BAR"),
  *   .MY_INT_PARAM(3),
  *   .MY_BOOL_PARAM(TRUE)
  * ) }}}
  * Set these parameters with
  * {{{ setVerilogParameters(new MyParams) }}}
  * inside a [[Chisel.BlackBox BlackBox]] module
  * or use {{{ setVerilogParameters(myCustomString) }}}
  */
class VerilogParameters {
  /** Get the string of verilog parameters */
  override def toString() : String = {
    val myFields = this.getClass().getDeclaredFields()
    val paramStr = new ArrayBuffer[String]
    paramStr += "# ("
    for ( field <- myFields ) {
      field.setAccessible(true)
      val fieldName = field.toString().split(" ").last.split('.').last
      val fieldVal = field.get(this)
      val fieldStr = {
        if (fieldVal.isInstanceOf[Boolean]) {
          if ( fieldVal.asInstanceOf[Boolean] ) "TRUE" else "FALSE"
        } else if (fieldVal.isInstanceOf[String])
          "\"" + fieldVal + "\""
        else
          fieldVal
      }
      val verilogStr = "    ." + fieldName + "(" + fieldStr + ")"
      if ( myFields.last == field )
        paramStr += verilogStr
      else
        paramStr += verilogStr + ","
    }
    paramStr += "  )"
    paramStr.mkString("\n")
  }
}

/** This class allows the connection to Verilog modules outside of chisel after generation
  * @example
  * {{{ class DSP48E1 extends BlackBox {
  * val io = new [[Chisel.Bundle Bundle]] // Create I/O with same as DSP
  * val dspParams = new [[Chisel.VerilogParameters VerilogParameters]] // Create Parameters to be specified
  * setVerilogParams(dspParams)
  * renameClock("clk", "clock")
  * renameReset("rst")
  * // Implement functionality of DSP to allow simulation verification
  * } }}}
  */
abstract class BlackBox extends Module {
  Driver.blackboxes += this
  private val clockMapping = new HashMap[String, String]

  /** Set the verilog parameters to be this string
    * @param string this string must start with "#(" and end with ")" to generate valid verilog */
  def setVerilogParameters(string: String) {
    this.asInstanceOf[Module].verilog_parameters = string;
  }

  /** Set the verilog parameters directly from a class [[Chisel.VerilogParameters VerilogParameters]]
    * @param params a object where all vals defined in the class are interpreted as verilog parameters */
  def setVerilogParameters(params: VerilogParameters) {
    this.asInstanceOf[Module].verilog_parameters = params.toString
  }

  /** Rename any clock with the output name of "clkName" to "outName"
    * @note Only defined for this black box module, to generally rename the clock see [[Chisel.Clock Clock]] */
  def renameClock(clkName: String, outName: String) {
    clockMapping.put(clkName, outName)
  }

  /** Rename a clk instance to have the output name of "outName"
    * @note This maps from the current clk.name */
  def renameClock(clk: Clock, outName: String) {
    clockMapping.put(clk.name, outName)
  }

  /** Get the output name of a clock string */
  def mapClock(clkName: String) : String = {
    clockMapping.getOrElse(clkName, clkName)
  }

  /** This method renames the implicit reset for this module */
  def renameReset(name: String) {
    this.reset.setName(name)
  }
}


trait Delay extends Node {
  override lazy val isInObject: Boolean = true
  def assignReset(rst: => Bool): Boolean = false
  def assignClock(clk: Clock) { clock = Some(clk) }
}

  /** If there is an environment variable `chiselArguments`, construct an `Array[String]`
    *  of its value split on ' ', otherwise, return a 0 length `Array[String]`
    *
    *  This makes it easy to merge with command line arguments and have the latter take precedence.
    * {{{
    *  def main(args: Array[String]) {
    *      val readArgs(chiselEnvironmentArguments() ++ args)
    *      ...
    *  }
    * }}}
    */
object chiselEnvironmentArguments {
  /** The ''name'' of the environment variable containing the arguments. */
  val chiselArgumentNameDefault = "chiselArguments"

  /**
    * @return value of environment variable `chiselArguments` split on ' ' or a 0 length Array[String]
    *
    */
  def apply(envName: String = chiselArgumentNameDefault): Array[String] = {
    val chiselArgumentString = Properties.envOrElse(envName, "")
    if (chiselArgumentString != "") {
      chiselArgumentString.split(' ')
    } else {
      new Array[String](0)
    }
  }
}
