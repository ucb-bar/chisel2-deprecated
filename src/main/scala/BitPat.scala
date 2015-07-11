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

/* Chisel3 compatibility */
object BitPat {
  def bitPatLit(n: String, width: Int) = {
    assert(n(0) == 'b', "BINARY BitPats ONLY")
    val w = if (width == -1) {
      n.length - 1
    } else {
      width
    }
    val res = new BitPat(n.substring(1, n.length))
    res.create(null, w)
    res
  }
  def apply(x: String, width: Int): BitPat = Lit(x, width){bitPatLit(x, width)}
  def apply(x: String): BitPat = apply(x, -1)
//  def apply(value: String, width: Int): BitPat = bitPatLit(value, width)
//  def apply(value: String): BitPat = apply(value, -1)

  def DC(width: Int): BitPat = BitPat("b" + "?"*width, width)
}

class BitPat(val value: String) extends UInt {
  // Since we have a constructor parameter, we need a clone method.
  override def cloneType: BitPat.this.type = (new BitPat(value)).asInstanceOf[BitPat.this.type]
  def fromInt(x: BigInt): BitPat = (BitPat(x.toString(2))).asInstanceOf[BitPat.this.type]
  val (bits, mask, swidth) = Literal.parseLit(value)
  def zEquals(other: Bits): Bool = 
    (Bits(Literal.toLitVal(mask, 2)) & other) === Bits(Literal.toLitVal(bits, 2))
  def === (other: Bits): Bool = zEquals(other)
  override def != (other: Bits): Bool  = !zEquals(other)

  // Far too much magic happens here.
  override def fromNode(n: Node): BitPat.this.type = {
    n match {
      case m: BitPat => BitPat.this
      case l: Literal if n.name.contains('?') => {
        BitPat.this.asTypeFor(n).asInstanceOf[BitPat.this.type]
      }
      case _ => {
        ChiselError.error("Only literals (and other BitPats), may be converted into BitPats")
        (BitPat("b0")).asInstanceOf[BitPat.this.type]
      }
    }
  }

  def badOp(op: String) {
    ChiselError.error("Operator %s is illegal for BitPats".format(op))
  }

  def badOpUInt(op: String): UInt = {
    badOp(op)
    UInt(0)
  }

  def badOpSInt(op: String): SInt = {
    badOp(op)
    SInt(0)
  }

  def badOpBool(op: String): Bool = {
    badOp(op)
    Bool(false)
  }

  def badOpThis(op: String): BitPat.this.type = {
    badOp(op)
    BitPat.this
  }

  // arithmetic operators
  override def zext(): SInt = badOpSInt("zext")
  override def unary_-(): UInt = badOpUInt("-")
  override def unary_!(): Bool = badOpBool("!")
  override def >> (b: UInt): UInt = badOpUInt(">>")
  override def +  (b: UInt): UInt = badOpUInt("+")
  override def *  (b: UInt): UInt = badOpUInt("*")
  override def /  (b: UInt): UInt = badOpUInt("/")
  override def %  (b: UInt): UInt = badOpUInt("%")
  override def ?  (b: UInt): UInt = badOpUInt("?")
  override def -  (b: UInt): UInt = badOpUInt("-")
  override def >> (i: Int): UInt = badOpUInt(">>") // chisel3
  override def << (i: Int): UInt = badOpUInt("<<") // chisel3
  override def +%  (b: UInt): UInt = badOpUInt("+") // chisel3 add-wrap
  override def +&  (b: UInt): UInt = badOpUInt("+&") // chisel3 add (width +1)
  override def -%  (b: UInt): UInt = badOpUInt("-") // chisel3 sub-wrap
  override def -&  (b: UInt): UInt = badOpUInt("-&") // chisel3 sub (width +1)

  // logical operators
  override def & (b: Bits): BitPat.this.type = badOpThis("&")
  override def | (b: Bits): BitPat.this.type = badOpThis("|")
  override def ^ (b: Bits): BitPat.this.type = badOpThis("^")
  override def <<(b: UInt): BitPat.this.type = badOpThis("<<")
 
  // order operators
  override def <  (b: UInt): Bool = badOpBool("<")
  override def <= (b: UInt): Bool = badOpBool("<=")
  override def >  (b: UInt): Bool = badOpBool(">")
  override def >= (b: UInt): Bool = badOpBool(">=")

  //UInt op SInt arithmetic
  override def +   (b: SInt): SInt = badOpSInt("+")
  override def *   (b: SInt): SInt = badOpSInt("*")
  override def -   (b: SInt): SInt = badOpSInt("-")
  override def /   (b: SInt): SInt = badOpSInt("/")
  override def %   (b: SInt): SInt = badOpSInt("%")
}
