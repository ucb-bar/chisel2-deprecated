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
import Component._
import Lit._

object NodeExtract {
  // extract one bit
  def apply(mod: Node, bit: Int): Node = apply(mod, bit, bit)

  // extract one bit
  def apply(mod: Node, bit: Node): Node = {
    val bitLit = bit.litOf
    if (bitLit != null) {
      apply(mod, bitLit.value.toInt)
    } else if (mod.litOf == null) {
      makeExtract(mod, bit)
    } else { // don't use Extract on literals
      Op(">>", 0, fixWidth(1), mod, bit)
    }
  }

  // extract bit range
  def apply(mod: Node, hi: Int, lo: Int): Node = apply(mod, hi, lo, -1)
  def apply(mod: Node, hi: Int, lo: Int, width: Int): Node = {
    val w = if (width == -1) hi - lo + 1 else width
    val bits_lit = mod.litOf
    if (bits_lit != null) {
      Literal((bits_lit.value >> lo) & ((BigInt(1) << w) - BigInt(1)), w)
    } else {
      makeExtract(mod, Literal(hi), Literal(lo), fixWidth(w))
    }
  }

  // extract bit range
  def apply(mod: Node, hi: Node, lo: Node, width: Int = -1): Node = {
    val hiLit = hi.litOf
    val loLit = lo.litOf
    val widthInfer = if (width == -1) widthOf(0) else fixWidth(width)
    if (hiLit != null && loLit != null) {
      apply(mod, hiLit.value.toInt, loLit.value.toInt, width)
    } else if (mod.litOf == null) {
      makeExtract(mod, hi, lo, widthInfer)
    } else { // don't use Extract on literals
      val rsh = Op(">>", 0, widthInfer, mod, lo)
      val hiMinusLoPlus1 = Op("+", 2, maxWidth _, Op("-", 2, maxWidth _, hi, lo), UFix(1))
      val mask = Op("-", 2, widthInfer, Op("<<", 0, widthInfer, UFix(1), hiMinusLoPlus1), UFix(1))
      Op("&", 2, widthInfer, rsh, mask)
    }
  }

  private def makeExtract(mod: Node, bit: Node) = {
    val res = new Extract
    res.init("", fixWidth(1), mod, bit)
    res.hi = bit
    res.lo = bit
    res
  }

  private def makeExtract(mod: Node, hi: Node, lo: Node, width: (Node) => Int) = {
    val res = new Extract
    res.init("", width, mod, hi, lo)
    res.hi = hi
    res.lo = lo
    res
  }
}

object Extract {
  //extract 1 bit
  def apply[T <: Bits](mod: T, bit: UFix)(gen: => Bool): Bool = {
    val x = NodeExtract(mod, bit)
    gen.fromNode(x)
  }

  def apply[T <: Bits](mod: T, bit: Int)(gen: => Bool): Bool = {
    val x = NodeExtract(mod, bit)
    gen.fromNode(x)
  }

  // extract bit range
  def apply[T <: Bits, R <: Bits](mod: T, hi: UFix, lo: UFix, w: Int = -1)(gen: => R): R = {
    val x = NodeExtract(mod, hi, lo, w)
    gen.fromNode(x)
  }

  def apply[T <: Bits, R <: Bits](mod: T, hi: Int, lo: Int)(gen: => R): R = {
    val x = NodeExtract(mod, hi, lo)
    gen.fromNode(x)
  }
}

class Extract extends Node {
  var lo: Node = null;
  var hi: Node = null;

  override def toString: String =
    if (hi == lo) {
      "BITS(" + inputs(0) + ", " + lo + ")";
    } else {
      "BITS(" + inputs(0) + ", " + hi + ", " + lo + ")";
    }

  def validateIndex(x: Node) {
    val lit = x.litOf
    assert(lit == null || lit.value >= 0 && lit.value < inputs(0).width,
           {println("Extract(" + lit.value + ")" +
                    " out of range [0," + (inputs(0).width-1) + "]" +
                    " of " + inputs(0) +
                    " on line " + line.getLineNumber +
                    " in class " + line.getClassName +
                    " in file " + line.getFileName)
          }
         )
  }
}
