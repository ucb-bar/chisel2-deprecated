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
import Width._
import ChiselError._

/** Factory object to construct Width objects. */
object Width {
  type WidthType = Option[Int]  // The internal representation of width
  // Class variables.
  val unSet: WidthType = None   // The internal "unset" value
  val unSetVal: Int = -1        // The external "unset" value
  val throwIfUnsetRef: Boolean = false
  val debug: Boolean = false

  /** Create a Width object with the specified value.
    * @param w Int width value; -1 (the default) indicates the Width is unknown.
    */
  def apply(w: Int = -1): Width = {
    if (w < -1) {
      ChiselError.warning("Width:apply < -1")
      if (throwIfUnsetRef) {
        throw new Exception("Width:apply < -1");
      }
    }
    val neww = new Width(w)
    neww
  }
}

/** Represent the width of a Chisel object.
  *
  * @constructor Create a Width object.
  * @param _width Integer value of Width. -1 indicates unknown.
  * @note Width objects may also be constructed via the Width companion object.
  *
  * We define the equality trio: hashCode, equals, canEqual for Width objects
  * (see ''Programming in Scala, Chapter 28: Object Equality'').
  * We want defined Widths with idential values to produce the same hashCode
  * but undefined Widths should have different hashCodes
  * (undefined Widths should NOT compare equal)
  * Since hashCode (and equals) depends on a var, it can change if the value changes.
  * This will be problematic for collections of Widths, if the value of
  * individual objects changes after they've been place in the collection.
  *
  */
class Width(_width: Int) extends Ordered[Width] {
  // Construction: initialize internal state
  private var widthVal: WidthType = if (_width > -1) Some(_width) else unSet

  if (debug) {
    ChiselError.warning("Width: w " + _width)
  }

  // Caller requires a single, integer value (legacy code).
  @deprecated("Clients should not expect a Width is directly convertable to an Int", "2.3")
  def width: Int = if (isKnown) widthVal.get else unSetVal

  /** Set the width (known) of a Width object explicitly.
    * @param w Int width value.
    * @throws Exception if w is < 0.
    */
  def setWidth(w: Int): Unit = {
    assert(w >= 0, ChiselError.error("Width.setWidth: setting width to " + w))
    if (w < 0) {
      if (throwIfUnsetRef) {
        throw new Exception("Width:setWidth < -1");
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

  /** Return a Boolean Indicating whether Width is actually known(set) or not. */
  def isKnown: Boolean = widthVal != unSet

  /** Return an "known" integer value or raise an exception.
    * @return Int width value
    * @throws Exception if Width value is unknown.
    */
  def needWidth(): Int = {
    if (! isKnown ) {
      ChiselError.warning("needWidth but width not set")
      if (throwIfUnsetRef) {
        ChiselError.report()
        throw new Exception("uninitialized width");
      }
    }
    widthVal.get
  }

  /** Return either the integer Width value or the specified value if the width isn't set. */
  def widthOrValue(v: Int): Int = {
    if (widthVal != unSet) widthVal.get else v
  }

  /** Compare two Widths.
    * We assume an unknown Width is less than any known Width.
    * @return the difference in Width values if both are known,
    * or 1 if this.Width is known, or -1 if this.Width is unknown.
    */
  def compare(that: Width): Int = {
    if (this.isKnown && that.isKnown) {
      this.widthVal.get - that.widthVal.get
    } else if (this.isKnown) {
      + 1       // that
    } else {
      - 1       // this
    }
  }

  /** Return a string representation of a Width object. */
  override def toString: String = (widthVal match {
    case Some(w) => w
    case x => x
  }).toString


  /** Generate a new Width object.
    *
    * @param w Int Width value, defaults to the Width value of the object being copied.
    * @return a new Width object.
    */
  def copy(w: Int = this.widthVal.get): Width = {
    val neww = new Width(w)
    neww
  }

  /** Clone a Width object. */
  override def clone(): Width = {
    if (this.isKnown) {
      Width(this.widthVal.get)
    } else {
      Width()
    }
  }

  /** Define the arithmetic operations so we can deal with unspecified widths. */
  private def binaryOp(op: String, operand: Width): Width = {
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
  /** Add two Width objects. */
  def +(w: Width): Width = binaryOp("+", w)

  // scalastyle:off method.name
  /** Subtract the specified Width subtrahend from a Width object. */
  def -(w: Width): Width = binaryOp("-", w)

  // scalastyle:off method.name spaces.after.plus
  /** Add an Int to a Width. */
  def +(i: Int): Width = binaryOp("+", Width(i))

  // scalastyle:off method.name
  /** Subtract an Int from a Width. */
  def -(i: Int): Width = binaryOp("-", Width(i))

  /** Convert (implicitly) from an Int to a Width. */
  implicit def fromInt(i: Int): Width = Width(i)

  /** Return the maximum of two Widths. */
  def max(that: Width): Width = if (compare(that) > 0) this else that

  /** Compute the Int hashcode for a Width object.  */
  override def hashCode: Int = widthVal match {
    case Some(w) => w * 41
    case _ => super.hashCode
  }

  /** Determine Width equality.  */
  override def equals(other: Any): Boolean = other match {
    case that: Width =>
      (that canEqual this) && (this.widthVal != unSet)
      (this.widthVal == that.widthVal)
    case _ =>
      false
  }

  /** Determine if two Widths are comparable. */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Width]

}
