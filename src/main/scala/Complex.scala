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

/** Representation for complex numbers */
object Complex {
  /** flag to indicate if using four multiplications or three, default is false
    * Four mult calculates: {{{ (a + bi)*(c + di) = (ac - bd) + (ad + bc)i }}}
    * Three mult calculates:
    * {{{ ac_p_ad = a * (c + d), ad_p_bd = (a + b) * d, bc_m_ac = (b - a) * c
    * (ac_p_ad - ad_p_bd) + (ac_p_ad + bc_m_ac)i }}} */
  var use_four_mults = false

  /** Create a new Complex number: real + imag*i
    * @tparam T the type to represent the complex number with, eg) UInt, SInt, Fixed
    * @example {{{ val myNum = Complex(UInt(3), UInt(1)) }}} */
  def apply[T<:Data with Num[T]](real: T, imag: T) = new Complex(real, imag)
}

/** Compute the conjugate of a complex number using the function [[Chisel.Complex.conj conj]] */
object conjugate {
  /** @example {{{ conjugate(Complex(SInt(3), SInt(1))) => Complex(SInt(3), SInt(-1)) }}}*/
  def apply[T<: Data with Num[T]](x : T) : T = x match {
    case x : Complex[_] => x.conj.asInstanceOf[T]
    case _ => x
  }
}

/** Complex number representation
  * create using the object [[Chisel.Complex$ Complex]]
  * @example {{{ val myNum = Complex[Fixed](Fixed(3, 16, 8), Fixed(1, 16, 8)) }}} */
class Complex[T<:Data with Num[T]](val real: T, val imag: T) extends Bundle with Num[Complex[T]] {
  /** Clone a complex instantiation */
  override def cloneType() = {
    new Complex(real.cloneType, imag.cloneType).asInstanceOf[this.type]
  }

  /** Check that 'name' is a valid component of Complex, ie) real or imag */
  override protected def checkPort(obj : Any, name : String) : Boolean = name match {
    case "real" => true
    case "imag" => true
    case "abs2" => false
    case "conj" => false
    case "unary_-" => false
    case _      => true
  }

  /** A complex multiply, uses 3 multiplies by default
    * Change to use four with the [[Chisel.Complex$.use_four_mults use_four_mults]] boolean variable */
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

  /** Create a new complex number which is the conjugate of this one */
  def conj : Complex[T] =
  {
    new Complex(real, -imag)
  }
  def / (r: Complex[T]): Complex[T] =
  {
    this * r.conj / r.abs2
  }
  def * (r: T): Complex[T] =
  {
    new Complex(real*r, imag*r)
  }
  /** Uses the % operator defined in the types
    * Is defined as:
    * {{{ (this.real % r.real) + (this.imag % r.imag)i }}}*/
  def % (r : Complex[T]): Complex[T] =
  {
    // this is bad, but what can we do?
    new Complex(real % r.real, imag % r.imag)
  }
  /** Compare the magnitudes of the complex numbers */
  def < (b : Complex[T]) : Bool =
  {
    this.abs2 < b.abs2
  }
  /** Compare the magnitudes of the complex numbers */
  def <= (b : Complex[T]) : Bool =
  {
    this.abs2 <= b.abs2
  }
  /** Compare the magnitudes of the complex numbers */
  def > (b : Complex[T]) : Bool =
  {
    this.abs2 > b.abs2
  }
  /** Compare the magnitudes of the complex numbers */
  def >= (b : Complex[T]) : Bool =
  {
    this.abs2 >= b.abs2
  }
  /** Compute the magnitude of the complex number: real^2 + imag^2 */
  def abs2 : T =
  {
    real * real + imag * imag
  }
  def / (r: T): Complex[T] =
  {
    new Complex(real/r, imag/r)
  }
  /** Add a scalar value to both the real and imaginary parts */
  def + (r: Complex[T]): Complex[T] =
  {
    new Complex(real + r.real, imag + r.imag)
  }
  /** Subtract a scalar value from both the real and imaginary parts */
  def - (r: Complex[T]): Complex[T] =
  {
    new Complex(real - r.real, imag - r.imag)
  }

  def unary_-(): Complex[T] =
  {
    new Complex(-real, -imag)
  }
}
