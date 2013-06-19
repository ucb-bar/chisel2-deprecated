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
  /* Implementation Note: scalac does not allow multiple overloaded
   method with default parameters so we define the following four
   methods to create UFix from litterals (with implicit and explicit
   widths) and reserve the default parameters for the "direction" method.
   */
  def apply(x: Int): UFix = Lit(x){UFix()};
  def apply(x: Int, width: Int): UFix = Lit(x, width){UFix()};
  def apply(x: String): UFix = Lit(x, -1){UFix()};
  def apply(x: String, width: Int): UFix = Lit(x, width){UFix()};

  def apply(dir: IODirection = null, width: Int = -1): UFix = {
    val res = new UFix();
    res.create(dir, width)
    res
  }
}


class UFix extends Bits /* with Numeric[UFix] */ {
  type T = UFix;

  /** Factory method to create and assign a *UFix* type to a Node *n*.
    */
  override def fromNode(n: Node): this.type = {
    UFix(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
  }

  override def fromInt(x: Int): this.type = {
    UFix(x).asInstanceOf[this.type]
  }

  // arithmetic operators
  def zext(): Fix = Cat(UFix(0,1), this).toFix
  def unary_-(): UFix = newUnaryOp("-");
  def unary_!(): Bool = Bool(OUTPUT).fromNode(UnaryOp(this, "!"));
  def << (b: UFix): UFix = newBinaryOp(b, "<<");
  def >> (b: UFix): UFix = newBinaryOp(b, ">>");
  def +  (b: UFix): UFix = newBinaryOp(b, "+");
  def *  (b: UFix): UFix = newBinaryOp(b, "*");
  def /  (b: UFix): UFix = newBinaryOp(b, "/");
  def %  (b: UFix): UFix = newBinaryOp(b, "%");
  def ?  (b: UFix): UFix = newBinaryOp(b, "?");
  def -  (b: UFix): UFix = newBinaryOp(b, "-");

  // order operators
  def >  (b: UFix): Bool = newLogicalOp(b, ">");
  def <  (b: UFix): Bool = newLogicalOp(b, "<");
  def <= (b: UFix): Bool = newLogicalOp(b, "<=");
  def >= (b: UFix): Bool = newLogicalOp(b, ">=");

  //UFix op Fix arithmetic
  def *   (b: Fix): Fix = Fix(OUTPUT).fromNode(BinaryOp(this.zext, b, "u*s"));
  def %   (b: Fix): Fix = Fix(OUTPUT).fromNode(BinaryOp(this.zext, b, "u%s"));
  def /   (b: Fix): Fix = Fix(OUTPUT).fromNode(BinaryOp(this.zext, b, "u/s"));
}
