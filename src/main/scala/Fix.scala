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

object Fix {

  def apply(x: Int): Fix = Lit(x){Fix()};
  def apply(x: Int, width: Int): Fix = Lit(x, width){Fix()};

  def apply(dir: IODirection = null, width: Int = -1): Fix = {
    val res = new Fix();
    res.create(dir, width)
    res
  }
}

class Fix extends Bits {
  setIsSigned

  override def setIsTypeNode {
    inputs(0).setIsSigned;
    super.setIsTypeNode
  }

  type T = Fix;

  /** Factory method to create and assign a *Fix* type to a Node *n*.
    */
  override def fromNode(n: Node) = {
    Fix(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
  }

  override def fromInt(x: Int) = {
    Fix(x).asInstanceOf[this.type]
  }

  override def matchWidth(w: Int): Node = {
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
  }

  /** casting from UFix followed by assignment. */
  def :=(src: UFix): Unit = this := src.zext;

  def gen[T <: Bits](): T = Fix().asInstanceOf[T];

  // arithmetic operators
  def unary_-(): Fix = newUnaryOp("-");
  def unary_!(): Fix = newUnaryOp("!");
  def << (b: UFix): Fix = newBinaryOp(b, "<<");
  def >> (b: UFix): Fix = newBinaryOp(b, ">>");
  def ?  (b: Fix): Fix = newBinaryOp(b, "?");

  // order operators
  def >  (b: Fix): Bool = newLogicalOp(b, ">");
  def <  (b: Fix): Bool = newLogicalOp(b, "<");
  def <= (b: Fix): Bool = newLogicalOp(b, "<=");
  def >= (b: Fix): Bool = newLogicalOp(b, ">=");
  def !=  (b: UFix): Bool = this != b.zext;
  def >   (b: UFix): Bool = this > Cat(UFix(1, 1), b).toFix;
  def <   (b: UFix): Bool = this < Cat(UFix(1, 1), b).toFix;
  def >=  (b: UFix): Bool = this >= Cat(UFix(1, 1), b).toFix;
  def <=  (b: UFix): Bool = this <= Cat(UFix(1, 1), b).toFix;

  override def ===[T <: Data](right: T): Bool = {
    right match {
      case b: UFix => this === b.zext;
      case _ =>
        this.asInstanceOf[Bits] === right
    }
  }

  //Fix to Fix arithmetic
  def +  (b: Fix): Fix = newBinaryOp(b, "+");
  def *  (b: Fix): Fix = newBinaryOp(b, "s*s");
  def /  (b: Fix): Fix = newBinaryOp(b, "s/s");
  def %  (b: Fix): Fix = newBinaryOp(b, "s%s");
  def -  (b: Fix): Fix = newBinaryOp(b, "-");

  //Fix to UFix arithmetic
  def +   (b: UFix): Fix = this + b.zext;
  def *   (b: UFix): Fix = newBinaryOp(b.zext, "s*u");
  def /   (b: UFix): Fix = newBinaryOp(b.zext, "s/u");
  def %   (b: UFix): Fix = newBinaryOp(b.zext, "s%u");
  def -   (b: UFix): Fix = this - b.zext;
}
