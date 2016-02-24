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

object UInt {
  /* Implementation Note: scalac does not allow multiple overloaded
   method with default parameters so we define the following four
   methods to create UInt from litterals (with implicit and explicit
   widths) and reserve the default parameters for the "direction" method.
   */
  /** Create a UInt from an Int */
  def apply(x: Int): UInt = apply(BigInt(x))
  /** Create a UInt from an Int with specified width */
  def apply(x: Int, width: Int): UInt = apply(BigInt(x), width)
  /** Create a UInt from a BigInt */
  def apply(x: BigInt): UInt = Lit(checkSign(x)){UInt()}
  /** Create a UInt from a BigInt with specified width */
  def apply(x: BigInt, width: Int): UInt = Lit(checkSign(x), width){UInt()}
  /** Create a UInt from a string of the format Bxxxx
    * where B is the base and can be:
    *  - h for hex
    *  - d for decimal
    *  - o for octal
    *  - b for binary
    */
  def apply(x: String): UInt = Lit(x, -1){UInt()}
  /** Create a UInt from a string of the format Bxxxx
    * where B is the base and can be:
    *  - h for hex
    *  - d for decimal
    *  - o for octal
    *  - b for binary
    * with an enforced 'width'
    */
  def apply(x: String, width: Int): UInt = Lit(x, width){UInt()}
  /** Create a UInt from a string
    * @param x is a String in the specified base
    * @param base is:
    *  - h for hex
    *  - d for decimal
    *  - o for octal
    *  - b for binary
    */
  def apply(x: String, base: Char): UInt = Lit(x, base, -1){UInt()}
  /** Create a UInt from a string
    * @param x is a String in the specified base
    * @param base is:
    *  - h for hex
    *  - d for decimal
    *  - o for octal
    *  - b for binary
    * @param width enforced bitwidth
    */
  def apply(x: String, base: Char, width: Int): UInt = Lit(x, base, width){UInt()}
  /** Create a UInt from a Node */
  def apply(x: Node): UInt = UInt(x, -1)
  /** Create a UInt from a Node with specified width */
  def apply(x: Node, width: Int): UInt = UInt(width = width).asTypeFor(x)
  /** Create a UInt for I/O with optional width */
  def apply(dir: IODirection = NODIR, width: Int = -1): UInt = {
    val res = new UInt()
    res.create(dir, width)
    res
  }

// FIXME: This should return a BitPat, not a UInt
//  def DC(width: Int): BitPat = BitPat.DC(width)
  def DC(width: Int): UInt = Lit("b" + "?"*width, width){UInt()}

  private def checkSign(x: BigInt) = {
    if (x < 0)
      ChiselError.error("UInt can't represent negative literal " + x)
    x
  }
}


class UInt extends Bits with Num[UInt] {
  type T = UInt;

  /** Factory method to create and assign a *UInt* type to a Node *n*.
    */
  override def fromNode(n: Node): this.type = {
    val res = UInt(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
    // NOTE: we do not inherit/clone the width.
    // Doing so breaks code in NodeFill()
    // res.width_ = n.width_.clone()
    n match {
      case l: Literal =>
        if (l.isZ && Driver.minimumCompatibility > "2") {
          // Chisel3 compatibility - generic don't care UInts/Bits are deprecated.
          ChiselError.error("General don't care UInts are deprecated. Please use BitPat().")
        }
      case _ =>
    }
    res
  }

  /** Set the value of this UInt */
  override def fromInt(x: Int): this.type = {
    UInt(x).asInstanceOf[this.type]
  }

  override def toBits: UInt = this

  // to support implicit conversions
  def ===(b: UInt): Bool = LogicalOp(this, b, "===")

  // arithmetic operators
  /** Convert a UInt to an SInt by added a MSB zero */
  def zext(): SInt = {
    // Don't sign-extend a zero-width node.
    val result = if (isZeroWidth) {
      this
    } else {
      Cat(UInt(0,1), this)
    }
    result.toSInt
  }

  def unary_-(): UInt = UInt(0) - this
  def unary_!(): Bool = this === UInt(0)
  def >> (b: UInt): UInt = newBinaryOp(b, ">>")
  def +  (b: UInt): UInt = newBinaryOp(b, "+")
  def *  (b: UInt): UInt = newBinaryOp(b, "*")
  def /  (b: UInt): UInt = newBinaryOp(b, "/")
  def %  (b: UInt): UInt = newBinaryOp(b, "%")
  def ?  (b: UInt): UInt = fromNode(Multiplex(this.toBool, b, null))
  def -  (b: UInt): UInt = newBinaryOp(b, "-")
  def >> (i: Int): UInt = newBinaryOp(UInt(i), ">>") // chisel3
  def << (i: Int): UInt = newBinaryOp(UInt(i), "<<") // chisel3
  /** chisel3 add-wrap operator */
  def +%  (b: UInt): UInt = newBinaryOp(b, "+")
  /** chisel3 add (width +1) operator */
  def +&  (b: UInt): UInt = newBinaryOp(b, "+&")
  /** chisel3 sub-wrap operator */
  def -%  (b: UInt): UInt = newBinaryOp(b, "-")
  /** chisel3 sub (width +1) operator */
  def -&  (b: UInt): UInt = newBinaryOp(b, "-&")

  // order operators
  def <  (b: UInt): Bool = newLogicalOp(b, "<")
  def <= (b: UInt): Bool = newLogicalOp(b, "<=")
  def >  (b: UInt): Bool = b < this
  def >= (b: UInt): Bool = b <= this

  //UInt op SInt arithmetic
  def +   (b: SInt): SInt = b + this
  def *   (b: SInt): SInt = b * this
  def -   (b: SInt): SInt = this.zext - b
  def /   (b: SInt): SInt = this.zext / b
  def %   (b: SInt): SInt = this.zext % b
}
