/*
 Copyright (c) 2011, 2012, 2013, 2014, 2015 The Regents of the University of
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
import Node._
import Literal._
import ChiselError._

/* Chisel3 compatibility */
object MInt {
  def mintLit(n: String, width: Int) = {
    assert(n(0) == 'b', "BINARY MINTS ONLY")
    val w = if (width == -1) {
      n.length - 1
    } else {
      width
    }
    val res = new MInt(n.substring(1, n.length))
    res.create(null, w)
    res
  }
  def apply(x: String, width: Int): MInt = Lit(x, width){mintLit(x, width)}
  def apply(x: String): MInt = apply(x, -1)
//  def apply(value: String, width: Int): MInt = mintLit(value, width)
//  def apply(value: String): MInt = apply(value, -1)

  def DC(width: Int): MInt = MInt("b" + "?"*width, width)
}

class MInt(val value: String) extends UInt {
  // Since we have a constructor parameter, we need a clone method.
  override def clone: this.type = 
    new MInt(value).asInstanceOf[this.type]
  def fromInt(x: BigInt): MInt = MInt(x.toString(2)).asInstanceOf[this.type]
  val (bits, mask, swidth) = parseLit(value)
  def zEquals(other: Bits): Bool = 
    (Bits(toLitVal(mask, 2)) & other) === Bits(toLitVal(bits, 2))
//  def === (other: Bits): Bool = zEquals(other)
//  def != (other: Bits): Bool  = !zEquals(other)

  // Far too much magic happens here.
  override def fromNode(n: Node): this.type = {
    n match {
      case m: MInt => this
      case l: Literal if n.name.contains('?') => {
        this.asTypeFor(n).asInstanceOf[this.type]
      }
      case _ => {
        super.fromNode(n)
      }
    }
  }
}
