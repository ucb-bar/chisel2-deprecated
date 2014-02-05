package FixedPoint

import Chisel._

// Defined format of numbers is 0.[raw] * 2^exp
abstract class Fix[B<:Bits,T<:Fix[B,T]](val exp: Int, val raw: B) extends Bundle {
  // This type parameterization is necessary so the functions polymorph to the subclass
  def Factory(exp: Int, width: Int): T
  def toRaw(a: Bits): B
  def get_sext(source: Bits): UInt

  def do_add(b: T): T = {
    val teff_exp = exp-raw.width
    val beff_exp = b.exp-b.raw.width

    val int_exp = math.min(teff_exp, beff_exp)
    // must zero extend on right side to lower effective exponents so everything matches
    val t_adj_rd = toRaw(if(teff_exp > int_exp) Cat(  raw, UInt(0, width=teff_exp-int_exp)) else   raw)
    val b_adj_rd = toRaw(if(beff_exp > int_exp) Cat(b.raw, UInt(0, width=beff_exp-int_exp)) else b.raw)

    val new_width = math.max(raw.width + teff_exp-int_exp, b.raw.width + beff_exp-int_exp)+1
    val new_exp = int_exp + new_width
    val result = Factory(new_exp, new_width)

    result.raw := t_adj_rd + b_adj_rd
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

class UFix(exp: Int, raw: UInt) extends Fix[UInt,UFix](exp, raw) {
  def Factory(exp: Int, width: Int) = UFix(exp, width)
  def toRaw(a: Bits) = a.toUInt
  def get_sext(source: Bits) = UInt(0,1)

  def + (b: UFix): UFix = do_add(b)
  def * (b: UFix): UFix = do_mult(b)
  def :=(source: UFix): Unit = do_truncate(source)

  def <<(b: Int): UFix = new UFix(exp+b, raw)
  def >>(b: Int): UFix = new UFix(exp-b, raw)
}

object SFix {
  def apply(exp: Int, width: Int): SFix = new SFix(exp, SInt(width=width))
}

class SFix(exp: Int, raw: SInt) extends Fix[SInt,SFix](exp, raw) {
  def Factory(exp: Int, width: Int) = SFix(exp, width)
  def toRaw(a: Bits) = a.toSInt
  def get_sext(source: Bits) = source(source.width-1,source.width-2)

  def + (b: SFix): SFix = do_add(b)
  def * (b: SFix): SFix = do_mult(b)
  def :=(source: SFix): Unit = do_truncate(source)

  def <<(b: Int): SFix = new SFix(exp+b, raw)
  def >>(b: Int): SFix = new SFix(exp-b, raw)
}
