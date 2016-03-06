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

object SInt {

  /** Create a SInt from an Int */
  def apply(x: Int): SInt = Lit(x){SInt()}
  /** Create a SInt from an Int specifying the width */
  def apply(x: Int, width: Int): SInt = Lit(x, width){SInt()}
  /** Create a SInt from an BigInt */
  def apply(x: BigInt): SInt = Lit(x){SInt()}
  /** Create a SInt from an BigInt specifying the width */
  def apply(x: BigInt, width: Int): SInt = Lit(x, width){SInt()}
  /** Create a SInt for I/O specifying the width */
  def apply(dir: IODirection = NODIR, width: Int = -1): SInt = {
    val res = new SInt()
    res.create(dir, width)
    res
  }
}

class SInt extends Bits with Num[SInt] {
  type T = SInt

  /** Factory method to create and assign a *SInt* type to a Node *n*.
    */
  override def fromNode(n: Node): this.type = {
    val res = SInt(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
    // NOTE: we do not inherit/clone the width.
    // Doing so breaks code in NodeFill()
    // res.width_ = n.width_.clone()
    res
  }

  /** Set the value of this SInt to a value */
  override def fromInt(x: Int): this.type = {
    SInt(x).asInstanceOf[this.type]
  }

  override def matchWidth(w: Width): Node = {
    val this_width = this.widthW
    if (w.isKnown && this_width.isKnown) {
      val my_width = this_width.needWidth()
      val match_width = w.needWidth()
      if (match_width > my_width) {
        if (my_width == 1) {
          val res = NodeFill(match_width, this) ; res.infer
          res
        } else {
          val topBit = NodeExtract(this, my_width-1) ; topBit.infer
          val fill = NodeFill(match_width - my_width, topBit) ; fill.infer
          val res = Concatenate(fill, this) ; res.infer
          res
        }
      } else if (match_width < my_width) {
        val res = NodeExtract(this, match_width-1,0) ; res.infer
        res
      } else {
        this
      }
    } else {
      ChiselError.error("SInt.matchWidth with unknown width: " + w + ", node " + this)
      this
    }
  }

  /** casting from UInt followed by assignment. */
  override protected def colonEquals(that: Bits): Unit = that match {
    case u: UInt => this := u.zext
    case _ => super.colonEquals(that)
  }

  def gen[T <: Bits](): T = SInt().asInstanceOf[T]

  // arithmetic operators
  def unary_-(): SInt = SInt(0) - this
  def unary_!(): Bool = this === SInt(0)
  def >> (b: UInt): SInt = newBinaryOp(b, "s>>")
  def ?  (b: SInt): SInt = fromNode(Multiplex(this.toBool, b, null))
  def >> (i: Int): SInt = newBinaryOp(UInt(i), "s>>") // chisel3
  def << (i: Int): SInt = newBinaryOp(UInt(i), "<<") // chisel3

  // order operators
  def <  (b: SInt): Bool = newLogicalOp(b, "s<")
  def >  (b: SInt): Bool = b < this
  def <= (b: SInt): Bool = newLogicalOp(b, "s<=")
  def >= (b: SInt): Bool = b <= this
  @deprecated("Use =/= rather than !=", "3")
  def !=  (b: UInt): Bool = {
    if (Driver.minimumCompatibility > "2") {
      ChiselError.error("!= is deprecated, use =/= instead")
    }
    this =/= b.zext
  }
  def =/=  (b: UInt): Bool = this =/= b.zext
  def >   (b: UInt): Bool = this > b.zext
  def <   (b: UInt): Bool = this < b.zext
  def >=  (b: UInt): Bool = this >= b.zext
  def <=  (b: UInt): Bool = this <= b.zext

  override def ===[T <: Data](right: T): Bool = {
    right match {
      case b: UInt => this === b.zext
      case _ =>
        super.===(right)
    }
  }

  //SInt to SInt arithmetic
  def +  (b: SInt): SInt = newBinaryOp(b, "+")
  def *  (b: SInt): SInt = newBinaryOp(b, "s*s")
  def /  (b: SInt): SInt = newBinaryOp(b, "s/s")
  def %  (b: SInt): SInt = newBinaryOp(b, "s%s")
  def -  (b: SInt): SInt = newBinaryOp(b, "-")
  /** chisel3 add-wrap operator */
  def +%  (b: SInt): SInt = newBinaryOp(b, "+")
  /** chisel3 add (width + 1) operator */
  def +&  (b: SInt): SInt = newBinaryOp(b, "+&")
  /** chisel3 sub-wrap operator */
  def -%  (b: SInt): SInt = newBinaryOp(b, "-")
  /** chisel3 sub (width + 1) operator */
  def -&  (b: SInt): SInt = newBinaryOp(b, "-&")

  //SInt to UInt arithmetic
  def * (b: UInt): SInt = {
    // We need to detect a zero-width operand early, due to the assumptions about "s*u" width manipulation in Op.c (see mulSUWidth())
    val opType = if (b.isZeroWidth) {
      "s*s"
    } else {
      "s*u"
    }
    newBinaryOp(b.zext, opType)
  }
  def + (b: UInt): SInt = this + b.zext
  def - (b: UInt): SInt = this - b.zext
  def / (b: UInt): SInt = this / b.zext
  def % (b: UInt): SInt = this % b.zext
  def abs: UInt = Mux(this < SInt(0), (-this).toUInt, this.toUInt)
}
