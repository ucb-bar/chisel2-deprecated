/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 Sydney (Regents). All Rights Reserved.  Redistribution and use in
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

/** I/O for a writer to the [[Chisel.TSB Tri State Buffer]] */
class WriterIO(gen : Bits) extends Bundle {
  /** Enable writing to the bus */
  val write = Bool()
  /** data to write to the bus */
  val data  = gen.cloneType
}

/** This class implements a Tri State Buffer (TSB)
  * @note To read the value of the TSB use TSB.out
  * @note The TSB has a flag pulledHigh for simulation and has no effect on generated verilog
  */
class TSB(gen : Bits) extends Node {
  /** Choose whether should be pulled high or low when TSB is idle
    * @note default is true*/
  var pulledHigh = true
  /** Pass the inout bus up to the top level module
    * @note default is false*/
  var toTop = false

  /** Read only output of the TSB */
  val out = gen.cloneType.asOutput

  // Set up the output
  out.inputs += this
  consumers += out

  /** Multiple write and enable output */
  private val writers = new ArrayBuffer[WriterIO]()

  /** Add writer to the TSB */
  def addWriter(newWriter : WriterIO) : Unit = {
    if ( newWriter.data.getWidth != getWidth ) ChiselError.error("Writers must have same width as TSB")
    inputs += newWriter.write
    inputs += newWriter.data
    newWriter.write.consumers += this
    newWriter.data.consumers += this
    writers += newWriter
  }

  /** Get all writers */
  def getWriters() = writers.toList

  // Override methods
  override def ## ( b : Node ) : Node = { ChiselError.error("Cannot use cat on TSB") }
  override def <> ( b : Node ) : Unit = { ChiselError.error("Cannot use <> as the writer cannot be known") }
  override def getWidth() : Int = out.getWidth
}
