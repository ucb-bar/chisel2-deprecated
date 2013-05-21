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

object UFix {
  // def apply(x: BigInt): UFix = Lit(x){UFix()};
  // def apply(x: BigInt, width: Int): UFix = Lit(x, width){UFix()};
  def apply(x: Int): UFix = Lit(x){UFix()};
  def apply(x: Int, width: Int): UFix = Lit(x, width){UFix()};

  def apply(dir: IODirection = null, width: Int = -1): UFix = {
    val res = new UFix();
    res.dir = dir;
    if(width > 0) {
      res.init("", width);
    } else {
      res.init("", widthOf(0))
    }
    res
  }
}

class UFix extends Num {
  type T = UFix;
  override def fromNode(n: Node) = {
    val res = UFix(OUTPUT).asInstanceOf[this.type];
    res assign n;
    res
  }

  override def :=(src: UFix) = {
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
      case any =>
        ChiselError.error(":= not defined on " + this.getClass + " and " + src.getClass)
    }
  }

  override def apply(bit: Int): UFix = { Extract(this, bit){UFix()}};
  override def apply(hi: Int, lo: Int): UFix = {Extract(this, hi, lo){UFix()}};
  override def apply(bit: UFix): UFix = {Extract(this, bit){UFix()}};
  override def apply(hi: UFix, lo: UFix): UFix = {Extract(this, hi, lo, -1){UFix()}};
  override def apply(range: (Int, Int)): UFix = this(range._1, range._2);

  override def unary_-(): UFix = UnaryOp(this, "-"){UFix()};
  override def unary_~(): UFix = UnaryOp(this, "~"){UFix()};
  override def andR(): Bool    = ReductionOp(this, "&"){UFix()};
  override def orR():  Bool    = ReductionOp(this, "|"){UFix()};
  override def xorR():  Bool   = ReductionOp(this, "^"){Bits()};
  override def << (b: UFix): UFix = BinaryOp(this, b, "<<"){UFix()};
  override def >> (b: UFix): UFix = BinaryOp(this, b, ">>"){UFix()};
  def +  (b: UFix): UFix = BinaryOp(this, b, "+"){UFix()};
  def *  (b: UFix): UFix = BinaryOp(this, b, "*"){UFix()};
  def /  (b: UFix): UFix = BinaryOp(this, b, "/"){UFix()};
  def %  (b: UFix): UFix = BinaryOp(this, b, "%"){UFix()};
  def ^  (b: UFix): UFix = BinaryOp(this, b, "^"){UFix()};
  def ?  (b: UFix): UFix = BinaryOp(this, b, "?"){UFix()};
  def -  (b: UFix): UFix = BinaryOp(this, b, "-"){UFix()};
  def ## (b: UFix): UFix = BinaryOp(this, b, "##"){UFix()};
  def ===(b: UFix): Bool = LogicalOp(this, b, "==="){UFix()};
  def != (b: UFix): Bool = LogicalOp(this, b, "!="){UFix()};
  def >  (b: UFix): Bool = LogicalOp(this, b, ">"){UFix()};
  def <  (b: UFix): Bool = LogicalOp(this, b, "<"){UFix()};
  def <= (b: UFix): Bool = LogicalOp(this, b, "<="){UFix()};
  def >= (b: UFix): Bool = LogicalOp(this, b, ">="){UFix()};
  def &  (b: UFix): UFix = BinaryOp(this, b, "&"){UFix()};
  def |  (b: UFix): UFix = BinaryOp(this, b, "|"){UFix()};

  //UFix op Fix arithmetic
  def *   (b: Fix): Fix = BinaryOp(this, b, "u*s"){Fix()}.toFix;
  def %   (b: Fix): Fix = BinaryOp(this, b, "u%s"){Fix()}.toFix;
  def /   (b: Fix): Fix = BinaryOp(this, b, "u/s"){Fix()}.toFix;
}

class Eyum extends UFix { };
object Eyum {
  def apply[T <: Eyum](x: Int, w: Int)(gen: => T): T = { Lit(x, w){ gen } }
  def apply[T <: Eyum](w: Int)(gen: => T): Int => T = { (x: Int) => Lit(x, w){ gen } }
}
