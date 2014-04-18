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

package FixedPoint

import Chisel._

// Defined format of numbers is 0.[raw] * 2^exp
abstract class Fix[B<:Bits,T<:Fix[B,T]](val exp: Int, val raw: B) extends Bundle {
  // This type parameterization is necessary so the functions polymorph to the subclass
  def Factory(exp: Int, width: Int): T
  def toRaw(a: Bits): B
  def get_sext(source: Bits): UInt

  def do_add(b: T, isSub: Boolean = false): T = {
    val teff_exp = exp-raw.width
    val beff_exp = b.exp-b.raw.width

    val int_exp = math.min(teff_exp, beff_exp)
    // must zero extend on right side to lower effective exponents so everything matches
    val t_adj_rd = toRaw(if(teff_exp > int_exp) Cat(  raw, UInt(0, width=teff_exp-int_exp)) else   raw)
    val b_adj_rd = toRaw(if(beff_exp > int_exp) Cat(b.raw, UInt(0, width=beff_exp-int_exp)) else b.raw)

    val new_width = math.max(raw.width + teff_exp-int_exp, b.raw.width + beff_exp-int_exp)+1
    val new_exp = int_exp + new_width
    val result = Factory(new_exp, new_width)

    result.raw := (if(isSub) t_adj_rd - b_adj_rd else t_adj_rd + b_adj_rd)
    result
  }
  def do_mult(b: T): T = {
    val result = Factory(exp+b.exp,raw.width+b.raw.width)
    result.raw := raw * b.raw
    return result
  }
  def do_truncate(source: T): Unit = {
    if(exp > source.exp) {
      val prepend_amt = exp-source.exp // need to extend source since it is too small...
      val taken_source = math.min(raw.width-prepend_amt, source.raw.width)
      val append_zs = raw.width-taken_source-prepend_amt
      raw := toRaw(Cat(Fill(prepend_amt, get_sext(source.raw)), (
        if(append_zs>0) (
          if(taken_source>0) Cat(source.raw(source.raw.width-1, source.raw.width-taken_source), UInt(0, width=append_zs))
          else               UInt(0, append_zs)
        ) else source.raw(source.raw.width-1, source.raw.width-taken_source)
      )))
    }
    else {
      val msb_extract = source.raw.width-(source.exp-exp)-1
      val remaining_source = if(msb_extract>=0) msb_extract+1 else 0
      val taken_source = math.min(remaining_source, raw.width)
      val append_zs = raw.width-taken_source
      raw := toRaw(
        if(append_zs>0) (
          if(taken_source>0) Cat(source.raw(msb_extract, msb_extract-taken_source+1), UInt(0, width=append_zs))
          else               UInt(0, append_zs)
        ) else source.raw(msb_extract, msb_extract-taken_source+1)
      )
    }
  }
}

object UFix {
  def apply(exp: Int, width: Int): UFix = new UFix(exp, UInt(width=width))
}

class UFix(exp: Int, raw: UInt) extends Fix[UInt,UFix](exp, raw) with Num[UFix] {
  def Factory(exp: Int, width: Int) = UFix(exp, width)
  def toRaw(a: Bits) = a.toUInt
  def get_sext(source: Bits) = UInt(0,1)

  def + (b: UFix): UFix = do_add(b)
  def * (b: UFix): UFix = do_mult(b)

  override protected def colonEquals(that: Bundle): Unit = that match {
    case u: UFix => do_truncate(u)
    case _ => illegalAssignment(that)
  }

  def <<(b: Int): UFix = new UFix(exp+b, raw)
  def >>(b: Int): UFix = new UFix(exp-b, raw)

  def unary_-(): UFix = throw new Exception("unimplemented unary -");
  def /  (b: UFix): UFix = throw new Exception("unimplemented /");;
  def %  (b: UFix): UFix = throw new Exception("unimplemented %");;
  def -  (b: UFix): UFix = throw new Exception("unimplemented -");;
  def <  (b: UFix): Bool = throw new Exception("unimplemented <");;
  def <= (b: UFix): Bool = throw new Exception("unimplemented <=");;
  def >  (b: UFix): Bool = throw new Exception("unimplemented >");;
  def >= (b: UFix): Bool = throw new Exception("unimplemented >=");;
}

object SFix {
  def apply(exp: Int, width: Int): SFix = new SFix(exp, SInt(width=width))
}

class SFix(exp: Int, raw: SInt) extends Fix[SInt,SFix](exp, raw) with Num[SFix] {
  def Factory(exp: Int, width: Int) = SFix(exp, width)
  def toRaw(a: Bits) = a.toSInt
  def get_sext(source: Bits) = source(source.width-1,source.width-2)

  def + (b: SFix): SFix = do_add(b)
  def - (b: SFix): SFix = do_add(b, isSub=true)
  def * (b: SFix): SFix = do_mult(b)
  def unary_-(): SFix = (new SFix(exp,SInt(0))) - this

  override protected def colonEquals(that: Bundle): Unit = that match {
    case s: SFix => do_truncate(s)
    case _ => illegalAssignment(that)
  }

  def <<(b: Int): SFix = new SFix(exp+b, raw)
  def >>(b: Int): SFix = new SFix(exp-b, raw)

  def /  (b: SFix): SFix = throw new Exception("unimplemented /");;
  def %  (b: SFix): SFix = throw new Exception("unimplemented %");;
  def <  (b: SFix): Bool = throw new Exception("unimplemented <");;
  def <= (b: SFix): Bool = throw new Exception("unimplemented <=");;
  def >  (b: SFix): Bool = throw new Exception("unimplemented >");;
  def >= (b: SFix): Bool = throw new Exception("unimplemented >=");;
}

object QR {
  def genSFix(int: Int, frac: Int) = SFix(int, int+frac)
  def genUFix(int: Int, frac: Int) = UFix(int, int+frac)
}

/*
class Toy extends Module {
  val io = new Bundle {
    val in0 = SFix(2, 4).asInput
    val in1 = SFix(2, 4).asInput

    val out = SFix(4,16).asOutput
    val oraw = Bits(OUTPUT, width=128)
  }

  val int_result = -io.in0 * (io.in0 + io.in1)

  io.out := int_result
  io.oraw := int_result.raw
}

class ToyTester(dut: Toy) extends AdvTester.AdvTester(dut) {
  poke(dut.io.in0.raw, BigInt(16-2))
  poke(dut.io.in1.raw, BigInt(5))
  takestep()
  def signed_peek(target: Bits): Double = {
    val raw = peek(target)
    val max_pos = (BigInt(1) << (target.getWidth-1))-1
    (if(raw > max_pos) raw - (BigInt(1) << target.getWidth) else raw).toDouble
  }
  def convert[T<:Fix[_,_]](target: T): Double = {
    target match {
      case s: SFix => (signed_peek(s.raw).toDouble * math.pow(2, s.exp-s.raw.width))
      case u: UFix => (peek(u.raw).toDouble * math.pow(2, u.exp-u.raw.width))
    }
  }
  println("In = %g, %g : Out = %g".format(convert(dut.io.in0), convert(dut.io.in1), convert(dut.io.out)))
  println("Raw output is: %s".format(peek(dut.io.oraw).toByteArray.map("%02X".format(_)).reduce(_+" "+_)))
}
*/
