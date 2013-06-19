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

object Bool {
  def apply(x: Boolean): Bool = Lit(if(x) 1 else 0, 1){Bool()}

  def apply(dir: IODirection = null): Bool = {
    val res = new Bool();
    res.dir = dir;
    res.init("", 1)
    res
  }
}

class Bool extends UFix {

  /** Factory method to create and assign a *Bool* type to a Node *n*.
    */
  override def fromNode(n: Node): this.type = {
    Bool(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
  }

  override def fromInt(x: Int): this.type = {
    Bool(x > 0).asInstanceOf[this.type]
  }

  def :=(src: Bool): Unit = {
    if(comp != null) {
      comp procAssign src;
    } else {
      this procAssign src;
    }
  }

  def := (src: Bits): Unit = {
    if(src.getWidth > 1) {
      throw new Exception("multi bit signal " + src + " converted to Bool");
    }
    if(src.getWidth == -1) {
      throw new Exception("unable to automatically convert " + src + " to Bool, convert manually instead");
    }
    this := src(0) // We only have one bit in *src*.
  }

  def && (b: Bool): Bool = if (b.isTrue) this else BinaryBoolOp(this, b, "&&");
  def || (b: Bool): Bool = BinaryBoolOp(this, b, "||");

  def isTrue: Boolean = {
    if(inputs.length == 0) {
      false
    } else {
      inputs(0) match {
        case l: Literal => {l.isLit && l.value == 1};
        case any        => false;
      }
    }
  }

  override def clone: this.type = Bool(dir).asInstanceOf[this.type]
}
