/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
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
import ChiselError._

object SInt {

  def apply(x: Int): SInt = Lit(x){SInt()};
  def apply(x: Int, width: Int): SInt = Lit(x, width){SInt()};
  def apply(x: BigInt): SInt = Lit(x){SInt()};
  def apply(x: BigInt, width: Int): SInt = Lit(x, width){SInt()};

  def apply(dir: IODirection = null, width: Int = -1): SInt = {
    val res = new SInt();
    res.create(dir, width)
    res
  }
}

class SInt extends Bits with Num[SInt] {
  type T = SInt;

  /** Factory method to create and assign a *SInt* type to a Node *n*.
    */
  override def fromNode(n: Node): this.type = {
    SInt(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
  }

  override def fromInt(x: Int): this.type = {
    SInt(x).asInstanceOf[this.type]
  }

  override def matchWidth(w: Int): Node = {
    // withModule(component, () =>
    if (w > this.width) {
      val topBit = NodeExtract(this, this.width-1); topBit.infer
      val fill = NodeFill(w - this.width, topBit); fill.infer
      val res = Concatenate(fill, this); res.infer
      res
    } else if (w < this.width) {
      val res = NodeExtract(this, w-1,0); res.infer
      res
    } else {
      this
    }
    // )
  }

  /** casting from UInt followed by assignment. */
  def :=(src: UInt): Unit = this := src.zext;

  def gen[T <: Bits](): T = SInt().asInstanceOf[T];

  // arithmetic operators
  def unary_-(): SInt = newUnaryOp("-");
  def unary_!(): SInt = newUnaryOp("!");
  def << (b: UInt): SInt = newBinaryOp(b, "<<");
  def >> (b: UInt): SInt = newBinaryOp(b, "s>>");
  def ?  (b: SInt): SInt = newBinaryOp(b, "?");

  // order operators
  def <  (b: SInt): Bool = newLogicalOp(b, "s<");
  def >  (b: SInt): Bool = b < this
  def <= (b: SInt): Bool = newLogicalOp(b, "s<=");
  def >= (b: SInt): Bool = b <= this
  def !=  (b: UInt): Bool = this != b.zext
  def >   (b: UInt): Bool = this > b.zext
  def <   (b: UInt): Bool = this < b.zext
  def >=  (b: UInt): Bool = this >= b.zext
  def <=  (b: UInt): Bool = this <= b.zext

  override def ===[T <: Data](right: T): Bool = {
    right match {
      case b: UInt => this === b.zext;
      case _ =>
        super.===(right)
    }
  }

  //SInt to SInt arithmetic
  def +  (b: SInt): SInt = newBinaryOp(b, "+");
  def *  (b: SInt): SInt = newBinaryOp(b, "s*s");
  def /  (b: SInt): SInt = newBinaryOp(b, "s/s");
  def %  (b: SInt): SInt = newBinaryOp(b, "s%s");
  def -  (b: SInt): SInt = newBinaryOp(b, "-");

  //SInt to UInt arithmetic
  def * (b: UInt): SInt = newBinaryOp(b.zext, "s*u")
  def + (b: UInt): SInt = this + b.zext
  def - (b: UInt): SInt = this - b.zext
  def / (b: UInt): SInt = this / b.zext
  def % (b: UInt): SInt = this % b.zext
  def abs: UInt = Mux(this < SInt(0), (-this).toUInt, this.toUInt)
}
