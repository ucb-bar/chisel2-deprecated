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

package Chisel.testers

import Chisel._

import scala.util.Random

// scalastyle:off regex
// scalastyle:off method.name
/**
  * provide common facilities for step based testing and decoupled interface testing
  */
abstract class HWIOTester extends BasicTester {
  val device_under_test:     Module
  var io_info:               IOAccessor = null
  def finish():              Unit

  def int(x: Bits):    BigInt = x.litValue()

  override val io = new Bundle {
    val running       = Bool(INPUT)
    val error         = Bool(OUTPUT)
    val pc            = UInt(OUTPUT, 32)
    val done          = Bool(OUTPUT)
  }
  io.done  := setDone
  io.error := setError

  val rnd = new scala.util.Random(Driver.testerSeed)

  var enable_scala_debug     = false
  var enable_printf_debug    = false
  var enable_all_debug       = false

  def logScalaDebug(msg: => String): Unit = {
    //noinspection ScalaStyle
    if(enable_all_debug || enable_scala_debug) println(msg)
  }

  def logPrintfDebug(fmt: String, args: Bits*): Unit = {
    if(enable_all_debug || enable_scala_debug) printf(fmt, args :_*)
  }
}
