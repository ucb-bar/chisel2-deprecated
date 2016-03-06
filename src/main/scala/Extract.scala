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

object NodeExtract {
  // extract one bit
  def apply(mod: Node, bit: Int): Node = apply(mod, bit, bit)
  def apply(mod: Node, bit: Node): Node = apply(mod, bit, bit, 1)

  // extract bit range
  def apply(mod: Node, hi: Int, lo: Int): Node = apply(mod, hi, lo, -1)
  def apply(mod: Node, hi: Int, lo: Int, width: Int): Node = {
    if (hi < lo) {
      if (!((hi == lo - 1) && Driver.isSupportW0W)) {
        ChiselError.error("Extract " + mod.getNode.name + "(hi = " + hi + ", lo = " + lo + ") requires hi >= lo")
      }
    }
    val w = if (width == -1) hi - lo + 1 else width
    // Currently, we don't restrict literals to their width,
    // so we can't use the literal directly if it overflows its width.
    val wmod = mod.widthW
    if (lo == 0 && wmod.isKnown && w == wmod.needWidth()) mod
    else mod.litOpt match {
      case Some(l) => Literal((l.value >> lo) & ((BigInt(1) << w) - BigInt(1)), w)
      case None => makeExtract(mod, Literal(hi), Literal(lo), Node.fixWidth(w))
    }
  }

  // extract bit range
  def apply(mod: Node, hi: Node, lo: Node, width: Int = -1): Node = {
    val widthInfer = if (width == -1) Node.widthOf(0) else Node.fixWidth(width)
    (hi.litOpt, lo.litOpt) match {
      case (Some(hl), Some(ll)) => apply(mod, hl.value.toInt, ll.value.toInt, width)
      case _ => makeExtract(mod, hi, lo, widthInfer)
    }
  }

  private def makeExtract(mod: Node, hi: Node, lo: Node, widthFunc: (=> Node) => Width) = {
    val res = new Extract(hi, lo)
    res.init("", widthFunc, mod, hi, lo)
    res
  }
}

object Extract {
  //extract 1 bit
  def apply[T <: Bits](mod: T, bit: UInt)(gen: => Bool): Bool = {
    val x = NodeExtract(mod, bit)
    gen.fromNode(x)
  }

  def apply[T <: Bits](mod: T, bit: Int)(gen: => Bool): Bool = {
    val x = NodeExtract(mod, bit)
    gen.fromNode(x)
  }

  // extract bit range
  def apply[T <: Bits, R <: Bits](mod: T, hi: UInt, lo: UInt, w: Int = -1)(gen: => R): R = {
    val x = NodeExtract(mod, hi, lo, w)
    gen.fromNode(x)
  }

  def apply[T <: Bits, R <: Bits](mod: T, hi: Int, lo: Int)(gen: => R): R = {
    val x = NodeExtract(mod, hi, lo)
    gen.fromNode(x)
  }
}

class Extract(hi: Node, lo: Node) extends Node {
  override def toString: String =
    ("/*" + (if (!name.isEmpty) name else "?") + "*/ Extract("
      + inputs(0) + (if (hi == lo) "" else (", " + hi)) + ", " + lo + ")")

  def validateIndex(x: Node) {
    val w0 = inputs(0).widthW
    x.litOpt match {
      case Some(l) => assert(l.value >= 0 && l.value < w0.needWidth(),
        ChiselError.error("Extract(" + l.value + ")" +
          " out of range [0," + (w0.needWidth()-1) + "]" +
          " of " + inputs(0), line))
      case _ =>
    }
  }

  override def canCSE: Boolean = true
  override def equalsForCSE(that: Node): Boolean = that match {
    case _: Extract => CSE.inputsEqual(this, that)
    case _ => false
  }

  // Eliminate any zero-width wires attached to this node.
  override def W0Wtransform() {
   /* We require that if the source is zero-width,
     *  the high and lo must be n-1 and n respectively.
     *  If this is true, we ensure our width is zero.
     */
    (inputs(1).litOpt, inputs(2).litOpt) match {
      case (Some(hl), Some(ll)) if inputs(0).getWidth == 0 && hl.value == (ll.value-1) =>
        setWidth(0)
        modified = true
      case _ =>
        ChiselError.error("Extract(" + inputs(0) + ", " + inputs(1) + ", " + inputs(2) + ")" +
            " W0Wtransform", line)
    }
  }

  // Static width - we can determine the width of the extracted bits at elaboration time.
  def isStaticWidth: Boolean = {
    if (this.inputs.length < 3 || this.needWidth() == 1) {
      true
    } else {
      val hi = this.inputs(1)
      val lo = this.inputs(2)
      // The width is known if both inputs will have the same runtime value, or both are literals.
      hi == lo || (hi.isLit && lo.isLit)
    }
  }

  // Is this a (known) single bit extraction?
  def isOneBit: Boolean = {
    // It must at least be static, then either have only one argument, or two identical arguments,
    //  or two arguments with identical values.
    //  TODO: This last test is redundant if literal nodes are identical if their values are identical
    isStaticWidth && (this.inputs.length < 3 || {
      (this.inputs(1), this.inputs(2)) match {
        case (hi: Literal, lo: Literal) => hi.value == lo.value
        case _ => {
           // If the width is static and we get here, both inputs must be the same.
          assert(hi == lo)
          true
        }
      }
    })
  }

  // Is this operation a no-op - extract all source bits?
  def isNop: Boolean = {
    // The extracted width must be static and equal to the source width, and the lo value must be 0.
    isStaticWidth && this.inputs(1).isLit && {
      val hiValue = this.inputs(1).litValue()
      val loValue = if (this.inputs.length < 3) {
        hiValue
      } else {
        this.inputs(2).litValue()
      }
      hiValue - loValue + 1 == this.inputs(0).needWidth && loValue == 0
    }
  }

  // Chisel3 - this node contains data - used for verifying Wire() wrapping
  override def isTypeOnly = false
}
