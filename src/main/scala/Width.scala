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
  def apply(w: Int = -1, throwUnset: Boolean = true): Width = {
    if (w < -1) {
      ChiselError.warning("Width:apply < -1")
      if (throwUnset) {
        throw new Exception("Width:apply < -1");
      }
    }
    val neww = new Width(w, throwUnset)
    neww
  }

  // Implicit conversion to Int - should be removed once
  // all dependent classes understand 'width'
  // implicit def toInt(w: Width): Int = w.needWidth()

}

class Width(_width: Int, _throwIfUnset: Boolean) extends Ordered[Width] {
  var width_ = _width
  val throwIfUnsetRef = _throwIfUnset
  val originalSet: Boolean = false
  val debug: Boolean = false

  if (debug) {
    ChiselError.warning("Width: w " + _width + ", throw " + _throwIfUnset)
  }
  def width_value: Int = width
  def width: Int = width_

  /** Sets the width of a Node. */
  def width_=(w: Int) {
    setWidth(w)
  }

  def setWidth(w: Int) = {
    assert(w > -2, ChiselError.error("Width.setWidth: setting width to " + w))
    if (w < 0 && throwIfUnsetRef) {
      throw new Exception("Width:setWidth < -1");
    }
    // The original set code looks wrong. It sets width_ to the previous width
    if (originalSet)
      width_ = width
    else
      width_ = w

    if (debug) {
      println("Width(" + this + "):setWidth(" + w + ")")
    }
    if (w < -1) {
      ChiselError.warning("Width:setWidth < -1")
      if (throwIfUnsetRef) {
        throw new Exception("Width:setWidth < -1");
      }
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
      ChiselError.warning("needWidth but width not set")
      if (throwIfUnsetRef) {
        ChiselError.report()
        throw new Exception("uninitialized width");
      }
    }
    w
  }

  // Return a value or 0 if the width isn't set
  def widthOrValue(v: Int): Int = {
    val w = width
    if (w >= 0) w else v
  }

  def compare(that: Width) = this.needWidth() - that.needWidth()

  override def toString: String = width_.toString

  def copy(w: Int = this.width_, t: Boolean = this.throwIfUnsetRef): Width = {
    val neww = new Width(w, t)
    neww
  }

  /*
  override def clone(): Width = {
    val w = new Width(this.width_, this.throwIfUnsetRef)
    w
  }
  */
  
  // Define the arithmetic operations so we can deal with unspecified widths
  def BinaryOp(op: String, operand: Int): Width = {
    if (!this.isSet) {
      this
    } else {
      op match {
        case "+" => Width(this.width_ + operand)
        case "-" => {
          assert(this.width_ >= operand)
          Width(this.width_ - operand)
        }
      }
    }
  }
}
