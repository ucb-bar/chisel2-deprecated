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
