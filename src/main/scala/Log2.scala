/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
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

object Log2 {
  def apply(x: Bits): UInt = UInt().asTypeFor(new Log2(x))
  def apply(x: Bits, n: Int): UInt = apply(x(n-1,0))
}

/** Returns the bit position of the trailing 1 in the input vector
  with the assumption that multiple bits of the input bit vector can be set
  */
object PriorityEncoder
{
  def apply(in: Iterable[Bool]): UInt = PriorityMux(in, (0 until in.size).map(UInt(_)))
  def apply(in: Bits): UInt = UInt().asTypeFor(new PriorityEncoder(in))
}

/** Does the inverse of UIntToOH.
  */
object OHToUInt
{
  def apply(in: Seq[Bool]): UInt = apply(Vec(in))
  def apply(in: Vec[Bool]): UInt = apply(in.toBits)
  def apply(in: Bits): UInt = UInt().asTypeFor(new OHToUInt(in))
}

abstract class Log2Like(x: Bits, name: String) extends Op {
  val op = name
  inputs += x
  inferWidth = log2Width

  private def log2Width(x: => Node): Width = {
    val w0 = x.inputs(0).widthW
    if (w0.isKnown) {
      val w = w0.needWidth()
      if (w < 2)
        Width(w) // TODO 0WW
      else
        Width(log2Up(w))
    } else {
      Width()
    }
  }
}

class Log2(x: Bits) extends Log2Like(x, "Log2") {
  override def lower: Node = {
    val w0 = inputs(0).widthW
    if (! w0.isKnown) {
      ChiselError.warning("Log2: unknown Width - " + inputs(0))
    }
    val range = w0.needWidth()-1 to 0 by -1
    val in = UInt(inputs(0))
    PriorityMux(range.map(in(_)), range.map(UInt(_)))
  }
}

class PriorityEncoder(x: Bits) extends Log2Like(x, "PriEnc") {
  override def lower: Node = {
    val w0 = inputs(0).widthW
    if (! w0.isKnown) {
      ChiselError.warning("PriorityEncoder: unknown Width - " + inputs(0))
    }
    PriorityMux(UInt(inputs(0)), (0 until w0.needWidth()).map(UInt(_)))
  }
}

class OHToUInt(x: Bits) extends Log2Like(x, "OHToUInt") {
  override def lower: Node = {
    def doLower(x: Node, length: Int): Node = {
      if (length <= 1) UInt(0,1)
      else if (length == 2) NodeExtract(x, 1)
      else {
        val half = 1 << (log2Up(length)-1)
        val hi = NodeExtract(x, length-1, half)
        val lo = NodeExtract(x, half-1, 0)
        Concatenate(LogicalOp(hi, Literal(0, length-half), "!="), doLower(BinaryOp(hi, lo, "|"), half))
      }
    }
    val w0 = inputs(0).widthW
    if (! w0.isKnown) {
      ChiselError.warning("OHToUInt: unknown Width - " + inputs(0))
    }
    doLower(inputs(0), w0.needWidth())
  }
}
