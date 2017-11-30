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

package FixedPoint

import Chisel._

// Defined format of numbers is 0.[raw] * 2^exp
abstract class Fix[B<:Bits with Num[B],T<:Fix[B,T]](val exp: Int, val raw: B) extends Bundle {
  // This type parameterization is necessary so the functions polymorph to the subclass
  def Factory(exp: Int, width: Int): T
  def toRaw(a: Bits): B
  def get_sext(source: Bits): UInt

  def aligned_with(b: T): Tuple3[B,B,Int] = {
    val teff_exp = exp-raw.needWidth()
    val beff_exp = b.exp-b.raw.needWidth()

    val int_exp = math.min(teff_exp, beff_exp)
    // must zero extend on right side to lower effective exponents so everything matches
    val t_adj_rd = toRaw(if(teff_exp > int_exp) Cat(  raw, UInt(0, width=teff_exp-int_exp)) else   raw)
    val b_adj_rd = toRaw(if(beff_exp > int_exp) Cat(b.raw, UInt(0, width=beff_exp-int_exp)) else b.raw)

    (t_adj_rd, b_adj_rd, int_exp)
  }

  def do_addsub(b: T, isSub: Boolean = false): T = {
    val (t_adj_rd, b_adj_rd, int_exp) = aligned_with(b)

    val new_width = math.max(exp - int_exp, b.exp - int_exp) + 1
    // note: exp - int_exp = width + eff_exp - int_exp where eff_exp = exp - width
    val new_exp = int_exp + new_width
    val result = Factory(new_exp, new_width)

    result.raw := (if(isSub) t_adj_rd - b_adj_rd else t_adj_rd + b_adj_rd)
    result
  }

  def do_lessthan(b: T): Bool = {
    val (t_adj, b_adj, _) = aligned_with(b)
    t_adj < b_adj
  }
  def do_lesseq(b: T): Bool = {
    val (t_adj, b_adj, _) = aligned_with(b)
    t_adj <= b_adj
  }

  def do_mult(b: T): T = {
    val result = Factory(exp + b.exp,raw.needWidth() + b.raw.needWidth())
    result.raw := raw * b.raw
    result
  }
  def do_divide(b: T): T = {
    val result = Factory(exp-b.exp,raw.needWidth())
    result.raw := raw / b.raw
    result
  }
  def do_truncate(source: T): Unit = {
    if(exp > source.exp) {
      val prepend_amt = exp-source.exp // need to extend source since it is too small...
      val gotWidth = source.raw.needWidth()
      val taken_source = math.min(raw.needWidth()-prepend_amt, gotWidth)
      val append_zs = raw.needWidth()-taken_source-prepend_amt
      raw := toRaw(Cat(Fill(prepend_amt, get_sext(source.raw)), (
        if(append_zs>0) (
          if(taken_source>0) Cat(source.raw(gotWidth-1, gotWidth-taken_source), UInt(0, width=append_zs))
          else               UInt(0, append_zs)
        ) else source.raw(gotWidth-1, gotWidth-taken_source)
      )))
    }
    else {
      val msb_extract = source.raw.needWidth()-(source.exp-exp)-1
      val remaining_source = if(msb_extract>=0) msb_extract + 1 else 0
      val taken_source = math.min(remaining_source, raw.needWidth())
      val append_zs = raw.needWidth()-taken_source
      raw := toRaw(
        if(append_zs>0) (
          if(taken_source>0) Cat(source.raw(msb_extract, msb_extract - taken_source + 1), UInt(0, width=append_zs))
          else               UInt(0, append_zs)
        ) else source.raw(msb_extract, msb_extract - taken_source + 1)
      )
    }
  }
}

/** An unsigned Fixed point representation
  * Consider using [[Chisel.Fixed Fixed]] instead */
object UFix {
  def apply(exp: Int, width: Int): UFix = new UFix(exp, UInt(width=width))
}

/** An unsigned Fixed point representation
  * Consider using [[Chisel.Fixed Fixed]] instead */
class UFix(exp: Int, raw: UInt) extends Fix[UInt,UFix](exp, raw) with Num[UFix] {
  def Factory(exp: Int, width: Int) = UFix(exp, width)
  def toRaw(a: Bits) = a.toUInt
  def get_sext(source: Bits) = UInt(0,1)

  def + (b: UFix): UFix = do_addsub(b)
  def - (b: UFix): UFix = do_addsub(b, isSub=true)
  def * (b: UFix): UFix = do_mult(b)
  def / (b: UFix): UFix = do_divide(b)
  def unary_-(): UFix = (new UFix(exp,UInt(0))) - this

  override protected def colonEquals(that: Bundle): Unit = that match {
    case u: UFix => do_truncate(u)
    case _ => illegalAssignment(that)
  }

  def <<(b: Int): UFix = new UFix(exp + b, raw)
  def >>(b: Int): UFix = new UFix(exp - b, raw)

  def <  (b: UFix): Bool = do_lessthan(b)
  def <= (b: UFix): Bool = do_lesseq(b)
  def >  (b: UFix): Bool = b.do_lessthan(this)
  def >= (b: UFix): Bool = b.do_lesseq(this)

  def %  (b: UFix): UFix = throwException("% unavailable for UFix")
}

@deprecated("Use [[Chisel.Fixed Fixed]] instead", "3")
object SFix {
  def apply(exp: Int, width: Int): SFix = new SFix(exp, SInt(width=width))
}

@deprecated("Use [[Chisel.Fixed Fixed]] instead", "3")
class SFix(exp: Int, raw: SInt) extends Fix[SInt,SFix](exp, raw) with Num[SFix] {
  def Factory(exp: Int, width: Int) = SFix(exp, width)
  def toRaw(a: Bits) = a.toSInt
  def get_sext(source: Bits) = source(source.needWidth()-1)

  def + (b: SFix): SFix = do_addsub(b)
  def - (b: SFix): SFix = do_addsub(b, isSub=true)
  def * (b: SFix): SFix = do_mult(b)
  def / (b: SFix): SFix = do_divide(b)
  def unary_-(): SFix = (new SFix(exp,SInt(0))) - this

  override protected def colonEquals(that: Bundle): Unit = that match {
    case s: SFix => do_truncate(s)
    case _ => illegalAssignment(that)
  }

  def <<(b: Int): SFix = new SFix(exp + b, raw)
  def >>(b: Int): SFix = new SFix(exp - b, raw)

  def <  (b: SFix): Bool = do_lessthan(b)
  def <= (b: SFix): Bool = do_lesseq(b)
  def >  (b: SFix): Bool = b.do_lessthan(this)
  def >= (b: SFix): Bool = b.do_lesseq(this)

  def %  (b: SFix): SFix = throwException("% unavailable for SFix")
}
