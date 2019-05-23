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

import scala.collection.mutable.{ArrayBuffer, HashMap}

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
  *   .MY_BOOL_PARAM(1)
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
          if ( fieldVal.asInstanceOf[Boolean] ) "1" else "0"
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
abstract class BlackBox( bbClock: Option[Clock] = None, bbReset: Option[Bool] = None) extends Module( bbClock, bbReset ) {
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
  def renameClock(clkIn: Clock, outName: String) {
    renameClock(clkIn.name, outName)
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
