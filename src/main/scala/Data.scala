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

abstract class Data extends Node {
  var comp: proc = null;
  def toFix(): Fix = chiselCast(this){Fix()};
  def toUFix(): UFix = chiselCast(this){UFix()};
  def toBits(): Bits = chiselCast(this){Bits()};
  def toBool(): Bool = chiselCast(this){Bool()};

  def setIsTypeNode {
    assert(inputs.length > 0, {println("Type Node must have an input") })
    isTypeNode = true
    inferWidth = widthOf(0)
  }

  def apply(name: String): Data = null
  def flatten = Array[(String, Bits)]();
  def terminate(): Unit = { }
  def flip(): this.type = this;
  def asInput(): this.type = this;

  /** Sets the direction (*dir*) of instances derived from Bits to OUTPUT
    or recursively sets members of Bundle/Vec to OUTPUT.
    Returns this instance with its exact type.
    */
  def asOutput(): this.type
  def isDirectionless: Boolean = true;

  def toNode: Node = this;
  def fromNode(n: Node): this.type = this;
  def fromBits(b: Bits): this.type = {
    val n = fromNode(b)
    n.setIsTypeNode
    n
  }
  def :=[T <: Data](data: T) = {
    if(this.getClass != data.getClass) println("Mismatched types: " + this.getClass + " " + data.getClass);
    comp procAssign data.toNode;
  }

  override def clone(): this.type = {
    try {
      val res = this.getClass.newInstance.asInstanceOf[this.type];
      res
    } catch {
      case e: java.lang.Exception => {
        throwException("Parameterized Bundle " + this.getClass + " needs clone method")
        this
      }
    }
  }

  override def nameIt(path: String) {
    if (isTypeNode && comp != null) {
      comp.nameIt(path)
    } else {
      super.nameIt(path)
    }
  }

  def setWidth(w: Int) {
    this.width = w;
  }
}

