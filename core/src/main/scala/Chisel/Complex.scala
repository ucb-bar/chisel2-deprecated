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

object Complex {
  var use_four_mults = false

  def apply[T<:Data with Num[T]](real: T, imag: T) = new Complex(real, imag)
}

class Complex[T<:Data with Num[T]](val real: T, val imag: T) extends Bundle {
  override def clone() = {
    new Complex(real.clone, imag.clone).asInstanceOf[this.type]
  }

  def * (r: Complex[T]): Complex[T] =
  {
    val a = real; val b = imag; val c = r.real; val d = r.imag;

    if(Complex.use_four_mults)
    {
      val ac = a*c; val bd = b*d; val ad = a*d; val bc = b*c;
      new Complex(ac - bd, ad + bc)
    }
    else //use three mults
    {
      val ac_p_ad = a * (c + d)
      val ad_p_bd = (a + b) * d
      val bc_m_ac = (b - a) * c

      new Complex(ac_p_ad - ad_p_bd, ac_p_ad + bc_m_ac)
    }
  }

  def / (r: Complex[T]): Complex[T] = ???

  def * (r: T): Complex[T] =
  {
    new Complex(real*r, imag*r)
  }
  def / (r: T): Complex[T] =
  {
    new Complex(real/r, imag/r)
  }

  def + (r: Complex[T]): Complex[T] =
  {
    new Complex(real + r.real, imag + r.imag)
  }
  def - (r: Complex[T]): Complex[T] =
  {
    new Complex(real - r.real, imag - r.imag)
  }

  def unary_-(): Complex[T] =
  {
    new Complex(-real, -imag)
  }
}

class ComplexTest extends Module {
  val io = new Bundle {
    val in_t = Complex(SInt(width=16),SInt(width=16)).asInput
    val in_f = Complex(SInt(width=16),SInt(width=16)).asInput
    val cond = Bool(INPUT)
    val out  = Complex(SInt(width=16),SInt(width=16)).asOutput

    val b_t = UInt(width=1).asInput
    val b_f = UInt(width=1).asInput
    val b_o = UInt(width=1).asOutput
  }

  val myLit = Complex(SInt(1), SInt(1))

  io.out := Mux(io.cond, io.in_t+io.in_f, io.in_t-io.in_f) + myLit

  io.b_o := Mux(io.cond, io.b_t, Bool(false))

}
