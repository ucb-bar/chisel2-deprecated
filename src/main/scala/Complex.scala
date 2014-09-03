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
