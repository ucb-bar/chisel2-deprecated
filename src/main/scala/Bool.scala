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

object Bool {
  def apply(x: Boolean) = Lit(x);

  def apply(dir: IODirection = null): Bool = {
    val res = new Bool();
    res.dir = dir;
    res.init("", 1)
    res
  }

  def apply(): Bool = Bool(null);
}

class Bool extends Bits {
  override def fromNode(n: Node) = {
    val res = Bool(OUTPUT).asInstanceOf[this.type];
    res assign n;
    res
  }

  override def :=(src: Bool) = {
    if(comp != null) {
      comp procAssign src.toNode;
    } else {
      this procAssign src.toNode;
    }
  }

  def := (src: Bits): Unit = {
    val res = src.toBool;
    if(src.getWidth > 1) {
      throw new Exception("multi bit signal " + src + " converted to Bool");
    }
    if(src.getWidth == -1) {
      throw new Exception("unable to automatically convert " + src + " to Bool, convert manually instead");
    }
    this := res;
  }

  override def :=[T <: Data](src: T): Unit = {
    src match {
      case bool: Bool => {
        this := bool;
      }
      case bits: Bits => {
        this := bits;
      }
      case any =>
        ChiselError.error(":= not defined on " + this.getClass + " and " + src.getClass)
    }
  }

  def generateError = {
    ChiselError.error("Cannot perform extraction on a Bool");
  }

  override def apply(bit: Int): Bool = { generateError; this};
  override def apply(hi: Int, lo: Int): Bool = {generateError; this};
  override def apply(bit: UFix): Bool = {generateError; this};
  override def apply(hi: UFix, lo: UFix): Bool = {generateError; this};

  override def unary_-(): Bool = UnaryBoolOp(this, "-");
  override def unary_~(): Bool = UnaryBoolOp(this, "~");
  def unary_!(): Bool = UnaryBoolOp(this, "!");
  def ^  (b: Bool): Bool = BinaryBoolOp(this, b, "^");
  def ===(b: Bool): Bool = BinaryBoolOp(this, b, "===");
  def != (b: Bool): Bool = BinaryBoolOp(this, b, "!=");
  override def && (b: Bool): Bool = if (b.isTrue) this else BinaryBoolOp(this, b, "&&");
  override def || (b: Bool): Bool = BinaryBoolOp(this, b, "||");
  def &  (b: Bool): Bool = BinaryBoolOp(this, b, "&");
  def |  (b: Bool): Bool = BinaryBoolOp(this, b, "|");

  def isTrue: Boolean = {
    if(inputs.length == 0) return false
    inputs(0) match {
      case l: Literal => {l.isLit && l.value == 1};
      case any        => false;
    }
  }

  override def clone = Bool(dir).asInstanceOf[this.type]
}
