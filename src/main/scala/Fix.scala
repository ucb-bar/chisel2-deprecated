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
    res.dir = dir;
    if(width > 0) {
      res.init("", width);
    } else {
      res.init("", widthOf(0))
    }
    res
  }
}

class Fix extends Num {
  setIsSigned

  override def setIsTypeNode {
    inputs(0).setIsSigned;
    super.setIsTypeNode
  }

  type T = Fix;
  override def fromNode(n: Node) = {
    val res = Fix(OUTPUT).asInstanceOf[this.type];
    res assign n;
    res};

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

  private def colonEqual(src: Num) = {
    if(comp != null) {
      comp procAssign src.toNode;
    } else {
      this procAssign src.toNode;
    }
  }

  override def :=[T <: Data](src: T): Unit = {
    src match {
      case ufix: UFix => {
        this := ufix;
      }
      case fix: Fix => {
        this := fix;
      }
      case any =>
        ChiselError.error(":= not defined on " + this.getClass + " and " + src.getClass)
    }
  }

  override def :=(src: Fix)  = colonEqual(src);
  override def :=(src: UFix) = colonEqual(Cat(Bits(0), src).toUFix);

  def gen[T <: Num](): T = Fix().asInstanceOf[T];

  override def apply(bit: Int): Fix = { Extract(this, bit){Fix()}};
  override def apply(hi: Int, lo: Int): Fix = {Extract(this, hi, lo){Fix()}};
  override def apply(bit: UFix): Fix = {Extract(this, bit){Fix()}};
  override def apply(hi: UFix, lo: UFix): Fix = {Extract(this, hi, lo, -1){Fix()}};
  override def apply(range: (Int, Int)): Fix = this(range._1, range._2);

  override def andR(): Bool    = ReductionOp(this, "&"){Fix()};
  override def orR():  Bool    = ReductionOp(this, "|"){Fix()};
  override def xorR(): Bool   = ReductionOp(this, "^"){Fix()};
  override def unary_-(): Fix = UnaryOp(this, "-"){Fix()};
  override def unary_~(): Fix = UnaryOp(this, "~"){Fix()};
  def unary_!(): Fix = UnaryOp(this, "!"){Fix()};
  override def << (b: UFix): Fix = BinaryOp(this, b.toFix, "<<"){Fix()};
  override def >> (b: UFix): Fix = BinaryOp(this, b.toFix, ">>"){Fix()};
  def ^  (b: Fix): Fix = BinaryOp(this, b, "^"){Fix()};
  def ?  (b: Fix): Fix = BinaryOp(this, b, "?"){Fix()};
  def ## (b: Fix): Fix = BinaryOp(this, b, "##"){Fix()};
  def &  (b: Fix): Fix = BinaryOp(this, b, "&"){Fix()};
  def |  (b: Fix): Fix = BinaryOp(this, b, "|"){Fix()};
  def abs: UFix = Mux(this < Fix(0), (-this).toUFix, this.toUFix)

  //Fix to Fix arithmetic
  def +  (b: Fix): Fix = BinaryOp(this, b, "+"){Fix()};
  def *  (b: Fix): Fix = BinaryOp(this, b, "s*s"){Fix()};
  def /  (b: Fix): Fix = BinaryOp(this, b, "s/s"){Fix()};
  def %  (b: Fix): Fix = BinaryOp(this, b, "s%s"){Fix()};
  def ===(b: Fix): Bool = LogicalOp(this, b, "==="){Fix()};
  def -  (b: Fix): Fix = BinaryOp(this, b, "-"){Fix()};
  def != (b: Fix): Bool = LogicalOp(this, b, "!="){Fix()};
  def >  (b: Fix): Bool = LogicalOp(this, b, ">"){Fix()};
  def <  (b: Fix): Bool = LogicalOp(this, b, "<"){Fix()};
  def <= (b: Fix): Bool = LogicalOp(this, b, "<="){Fix()};
  def >= (b: Fix): Bool = LogicalOp(this, b, ">="){Fix()};

  //Fix to UFix arithmetic
  def +   (b: UFix): Fix = this + Cat(Bits(0, 1), b).toFix;
  def *   (b: UFix): Fix = BinaryOp(this, b, "s*u"){Fix()}.toFix;
  def /   (b: UFix): Fix = BinaryOp(this, b, "s/u"){Fix()}.toFix;
  def %   (b: UFix): Fix = BinaryOp(this, b, "s%u"){Fix()}.toFix;
  def -   (b: UFix): Fix = this - Cat(Bits(0, 1), b).toFix;
  def === (b: UFix): Bool = this === Cat(Bits(0, 1), b).toFix;
  def !=  (b: UFix): Bool = this != Cat(Bits(0, 1), b).toFix;
  def >   (b: UFix): Bool = this > Cat(Bits(1, 1), b).toFix;
  def <   (b: UFix): Bool = this < Cat(Bits(1, 1), b).toFix;
  def >=  (b: UFix): Bool = this >= Cat(Bits(1, 1), b).toFix;
  def <=  (b: UFix): Bool = this <= Cat(Bits(1, 1), b).toFix;
}
