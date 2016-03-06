/*
 Copyright (c) 2011 - 2016 The Regents of the University of
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

import java.util.NoSuchElementException

/** Get the number of bits required */
object Width {
  type WidthType = Option[Int]  // The internal representation of width
  // Class variables.
  /** The internal "unset" value */
  val unSet: WidthType = None
  /** The external "unset" value */
  val unSetVal: Int = -1
  val throwIfUnsetRef: Boolean = false
  val debug: Boolean = false

  def apply(w: Int = -1): Width = {
    if (w < -1) {
      ChiselError.warning("Width:apply < -1")
      if (throwIfUnsetRef) {
        throwException("Width:apply < -1");
      }
    }
    val neww = new Width(w)
    neww
  }
}

/** Create a new width */
class Width(_width: Int) extends Ordered[Width] {
  import Width._

  // Construction: initialize internal state
  private var widthVal: WidthType = if (_width > -1) Some(_width) else unSet

  if (debug) {
    ChiselError.warning("Width: w " + _width)
  }

  // Caller requires a single, integer value (legacy code).
  @deprecated("Clients should not expect a Width is directly convertable to an Int", "2.3")
  def width: Int = if (isKnown) widthVal.get else unSetVal

  /** Set the width of a Node, w must be >= 0*/
  def setWidth(w: Int): Unit = {
    assert(w >= 0, ChiselError.error("Width.setWidth: setting width to " + w))
    if (w < 0) {
      if (throwIfUnsetRef) {
        throwException("Width:setWidth < -1");
      }
      widthVal = unSet
    } else {
      widthVal = Some(w)
    }
    if (debug) {
      // scalastyle:off regex
      println("Width(" + this + "):setWidth(" + w + ")")
    }
  }

  // scalastyle:off method.name
  // Syntactic sugar - an attempt to set "width" to an integer.
  def width_=(w: Int) {
    setWidth(w)
  }

  /** Indicate whether width is actually known(set) or not */
  def isKnown: Boolean = widthVal != unSet

  /** @return an "known" integer value
    * @throws NoSuchElementException if called when the width is unknown */
  def needWidth(): Int = {
    if (! isKnown ) {
      ChiselError.warning("needWidth but width not set")
    }
    widthVal.get
  }

  /** @return the width or the specificed value if the width isn't set */
  def widthOrValue(v: Int): Int = {
    if (widthVal != unSet) widthVal.get else v
  }

  /** Compare two widths where an unknown width is less than any known width */
  def compare(that: Width): Int = {
    if (this.isKnown && that.isKnown) {
      this.widthVal.get - that.widthVal.get
    } else if (this.isKnown) {
      + 1       // that
    } else {
      - 1       // this
    }
  }

  /** Print a string representation of width */
  override def toString: String = (widthVal match {
    case Some(w) => w
    case x => x
  }).toString

  /** create a copy of this width
    * @param w optionally set the width for the new copy */
  def copy(w: Int = this.widthVal.get): Width = {
    val neww = new Width(w)
    neww
  }

  /** clone this width */
  override def clone(): Width = {
    if (this.isKnown) {
      Width(this.widthVal.get)
    } else {
      Width()
    }
  }

  // Define the arithmetic operations so we can deal with unspecified widths
  // TODO: make private?
  def binaryOp(op: String, operand: Width): Width = {
    if (!this.isKnown) {
      this
    } else if (! operand.isKnown) {
      operand
    } else {
      val (v1,v2) = (this.widthVal.get, operand.widthVal.get)
      op match {
        case "+" => Width(v1 + v2)
        case "-" => {
          val w = v1 - v2
          if (w < 0) {
            ChiselError.warning(s"Width.op- setting width to Width(0): $v1 < $v2")
          }
          Width(math.max(w,0)) // must floor at 0 because negative widths just interpreted as unset
        }
      }
    }
  }

  // scalastyle:off method.name spaces.after.plus
  // Define addition of two widths
  def +(w: Width): Width = binaryOp("+", w)

  // scalastyle:off method.name
  // Define subtraction of one Width from another
  def -(w: Width): Width = binaryOp("-", w)

  // scalastyle:off method.name spaces.after.plus
  // Define addition of an Int to a Width
  def +(i: Int): Width = binaryOp("+", Width(i))

  // scalastyle:off method.name
  // Define subtraction of an Int from a Width
  def -(i: Int): Width = binaryOp("-", Width(i))

  implicit def fromInt(i: Int): Width = Width(i)

  def max(that: Width): Width = if (compare(that) > 0) this else that

  // Define the equality trio: hashCode, equals, canEqual
  //   Programming in Scala, Chapter 28: Object Equality
  //   We want defined widths with idential values to produce the same hashCode
  //   but undefined widths should have different hashCodes
  //   (undefined widths should NOT compare equal)
  //   Since hashCode (and equals) depends on a var, it can change if the value changes.
  //   This will be problematic for collections of widths.
  /** Define the hashcode based on the width
    * the same widths should have the same hash code
    * unknown widths should be unique
    */
  override def hashCode: Int = widthVal match {
    case Some(w) => w * 41
    case _ => super.hashCode
  }
  /** Define equality for width */
  override def equals(other: Any): Boolean = other match {
    case that: Width =>
      (that canEqual this) && (this.widthVal != unSet)
      (this.widthVal == that.widthVal)
    case _ =>
      false
  }
  /** check if 'other' is a Width to check if its possible to equal a Width */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Width]

}
