/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
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

object Width {
  def apply(n: Node, w: Int = -1, throwUnset: Boolean = true): Width = {
    if (w < -1) {
      ChiselError.warning("Width:apply < -1: " + n)
      throw new Exception("Width:apply < -1");
    }
    val neww = new Width(n, w, throwUnset)
    neww
  }

  // Implicit conversion to Int - should be removed once
  // all dependent classes understand 'width'
  implicit def toInt(w: Width): Int = w.needWidth()

}

class Width(_node: Node, _width: Int, _throwIfUnset: Boolean) extends Ordered[Width] {
  val node = _node
  var width_ = _width
  val throwIfUnsetNeed = _throwIfUnset
  val originalSet: Boolean = false

  var inferWidth: (Node) => Int = maxWidth;
  def width_value: Int = width
  def width: Int = if (Driver.isInGetWidth) inferWidth(node) else width_

  /** Sets the width of a Node. */
  def width_=(w: Int) {
    setWidth(w)
  }

  def setWidth(w: Int) = {
    // The original set code looks wrong. It sets width_ to the previous width
    if (originalSet)
      width_ = width
    else
      width_ = w
    inferWidth = fixWidth(w);
    println("Width(" + this + "):setWidth(" + w + "), " + node)
    if (w < -1) {
      ChiselError.warning("Width:setWidth < -1: " + node)
      throw new Exception("Width:setWidth < -1");
    }
  }

  // Print a string representation of width
  // Indicate whether width is actually set or not.
  def isSet: Boolean = { width_ >= 0 }

  val line: StackTraceElement =
    if (Driver.getLineNumbers) {
      val trace = new Throwable().getStackTrace
      findFirstUserLine(trace) getOrElse trace(0)
    } else null

  // Return a value or raise an exception.
  def needWidth(): Int = {
    val w = width
    if (w == -1) {
      ChiselError.warning("needWidth but width not set: " + node)
      node.printTree(System.err)
      if (throwIfUnsetNeed)
        ChiselError.report()
        throw new Exception("uninitialized width");
    }
    w
  }

  // Return a value or 0 if the width isn't set
  def WidthOrValue(v: Int): Int = {
    val w = width
    if (w >= 0) w else v
  }

  // Set the inferWidth function, returning the current value.
  def setInferWidth(widthfunc: (Node) => Int): (Node) => Int = {
    val widthfunc_old = inferWidth
    inferWidth = widthfunc
    widthfunc_old
  }

  def compare(that: Width) = this.needWidth() - that.needWidth()

  override def toString: String = width_.toString

  def clone(n: Node = null): Width = {
    val w = new Width(n, width_, throwIfUnsetNeed)
    w.inferWidth = this.inferWidth
    w
  }
}
