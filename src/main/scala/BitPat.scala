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
import Literal._

/** A bit pattern object to enable representation of dont cares */
object BitPat {
  /** Get a bit pattern from a string
    * @param n a string with format b---- eg) b1?01
    * @note legal characters are 0, 1, ? and must be base 2*/
  def apply(n: String): BitPat = {
    require(n(0) == 'b', "BINARY BitPats ONLY")
    val (bits, mask, swidth) = parseLit(n.substring(1))
    new BitPat(toLitVal(bits, 2), toLitVal(mask, 2), swidth)
  }

  /** Get a bit pattern of don't cares with a specified width */
  def DC(width: Int): BitPat = BitPat("b" + ("?" * width))

  // BitPat <-> UInt
  /** enable conversion of a bit pattern to a UInt */
  implicit def BitPatToUInt(x: BitPat): UInt = {
    require(x.mask == (BigInt(1) << x.getWidth)-1)
    UInt(x.value, x.getWidth)
  }
  /** create a bit pattern from a UInt */
  implicit def apply(x: UInt): BitPat = {
    require(x.isLit)
    BitPat("b" + x.litValue().toString(2))
  }
}

/** A class to create bit patterns
  * Use the [[Chisel.BitPat$ BitPat]] object instead of this class directly */
class BitPat(val value: BigInt, val mask: BigInt, width: Int) {
  def getWidth: Int = width
  def === (other: Bits): Bool = UInt(value) === (other & UInt(mask))
  @deprecated("Use =/= rather than != for chisel comparison", "3")
  def != (other: Bits): Bool = {
    if (Driver.minimumCompatibility > "2") {
      ChiselError.error("!= is deprecated, use =/= instead")
    }
    !(this === other)
  }
  def =/= (other: Bits): Bool = !(this === other)
}
