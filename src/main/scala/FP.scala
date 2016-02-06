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
import Node.fixWidth

/// FLO
/** Create a 32 bit floating point Object */
object Flo {
  /** Convert a Float to a Flo literal */
  def apply(x: Float): Flo = Lit(java.lang.Float.floatToIntBits(x), 32){ Flo() }
  /** Convert a Double to a Flo literal
    * @note Flo uses 32 bits so the Double is first converted to a Float */
  def apply(x: Double): Flo = Flo(x.toFloat);

  /** Create a Flo for I/O */
  def apply(dir: IODirection = NODIR): Flo = {
    val res = new Flo();
    res.dir = dir;
    // The width is fixed, really fixed.
    res.init("", 32)
    res
  }
}

/** A 32 bit floating point representation class
  * Create using the [[Chisel.Flo$ Flo]] object */
class Flo extends Bits with Num[Flo] {
  // setIsSigned

  // override def setIsTypeNode = {inputs(0).setIsSigned; super.setIsTypeNode}

  type T = Flo;
  /** Convert a node to a [[Chisel.Flo Flo]] */
  override def fromNode(n: Node): this.type = {
    val res = Flo(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
    res
  }

  /** Get the floating point representation of an Int */
  override def fromInt(x: Int): this.type = {
    Flo(x.toFloat).asInstanceOf[this.type]
  }

  override protected def colonEquals(that: Bits): Unit = that match {
    case _: Flo => super.colonEquals(that)
    case _ => illegalAssignment(that)
  }

  /** Get Flo as an instance of T */
  def gen[T <: Bits](): T = Flo().asInstanceOf[T];

  def unary_-(): Flo = newUnaryOp("f-")
  def +  (b: Flo): Flo = newBinaryOp(b, "f+")
  def -  (b: Flo): Flo = newBinaryOp(b, "f-")
  def *  (b: Flo): Flo = newBinaryOp(b, "f*")
  def /  (b: Flo): Flo = newBinaryOp(b, "f/")
  def %  (b: Flo): Flo = newBinaryOp(b, "f%")
  def ===(b: Flo): Bool = newLogicalOp(b, "f==")
  @deprecated("Use =/= rather than != for chisel comparison", "3")
  def != (b: Flo): Bool = newLogicalOp(b, "f!=")
  def =/= (b: Flo): Bool = newLogicalOp(b, "f!=")
  def >  (b: Flo): Bool = newLogicalOp(b, "f>")
  def <  (b: Flo): Bool = newLogicalOp(b, "f<")
  def <= (b: Flo): Bool = newLogicalOp(b, "f<=")
  def >= (b: Flo): Bool = newLogicalOp(b, "f>=")
  def pow (b: Flo): Flo = newBinaryOp(b, "fpow")
  def sin: Flo = newUnaryOp("fsin")
  def cos: Flo = newUnaryOp("fcos")
  def tan: Flo = newUnaryOp("ftan")
  def asin: Flo = newUnaryOp("fasin")
  def acos: Flo = newUnaryOp("facos")
  def atan: Flo = newUnaryOp("fatan")
  def sqrt: Flo = newUnaryOp("fsqrt")
  def floor: Flo = newUnaryOp("ffloor")
  def ceil: Flo = newUnaryOp("fceil")
  def round: Flo = newUnaryOp("fround")
  def log: Flo = newUnaryOp("flog")
  override def toSInt (): SInt = SInt(OUTPUT).fromNode(Op("fToSInt", fixWidth(32), this))
  override def toUInt (): UInt = UInt(OUTPUT).fromNode(Op("fToUInt", fixWidth(32), this))
}

/// DBL

/** Create a 64 bit double precision floating point representation */
object Dbl {

  /** Convert a float to a [[Chisel.Dbl Dbl]] literal
    * @note first converted to a Double */
  def apply(x: Float): Dbl = Dbl(x.toDouble);
  /** Convert a Double to a [[Chisel.Dbl Dbl]] literal */
  def apply(x: Double): Dbl = Lit(java.lang.Double.doubleToLongBits(x), 64){ Dbl() }

  /** Create a [[Chisel.Dbl Dbl]] as I/O */
  def apply(dir: IODirection = NODIR): Dbl = {
    val res = new Dbl();
    res.dir = dir;
    // The width is fixed, really fixed.
    res.init("", 64)
    res
  }
}

/** A double precision floating point representation
  * Create using [[Chisel.Dbl$ Dbl]] */
class Dbl extends Bits with Num[Dbl] {
  // setIsSigned

  // override def setIsTypeNode = {inputs(0).setIsSigned; super.setIsTypeNode}

  type T = Dbl;
  /** Convert a node to [[Chisel.Dbl Dbl]] representation */
  override def fromNode(n: Node) = {
    val res = Dbl(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
    res
  }

  /** Convert an Integer to [[Chisel.Dbl Dbl]] representation */
  override def fromInt(x: Int): this.type = {
    Dbl(x.toDouble).asInstanceOf[this.type]
  }

  override protected def colonEquals(that: Bits): Unit = that match {
    case _: Dbl => super.colonEquals(that)
    case _ => illegalAssignment(that)
  }

  def gen[T <: Bits](): T = Dbl().asInstanceOf[T];

  def unary_-(): Dbl = newUnaryOp("d-")
  def +  (b: Dbl): Dbl = newBinaryOp(b, "d+")
  def -  (b: Dbl): Dbl = newBinaryOp(b, "d-")
  def *  (b: Dbl): Dbl = newBinaryOp(b, "d*")
  def /  (b: Dbl): Dbl = newBinaryOp(b, "d/")
  def %  (b: Dbl): Dbl = newBinaryOp(b, "d%")
  def ===(b: Dbl): Bool = newLogicalOp(b, "d==")
  @deprecated("Use =/= rather than != for chisel comparison", "3")
  def != (b: Dbl): Bool = newLogicalOp(b, "d!=")
  def =/= (b: Dbl): Bool = newLogicalOp(b, "d!=")
  def >  (b: Dbl): Bool = newLogicalOp(b, "d>")
  def <  (b: Dbl): Bool = newLogicalOp(b, "d<")
  def <= (b: Dbl): Bool = newLogicalOp(b, "d<=")
  def >= (b: Dbl): Bool = newLogicalOp(b, "d>=")
  def pow (b: Dbl): Dbl = newBinaryOp(b, "dpow")
  def sin: Dbl = newUnaryOp("dsin")
  def cos: Dbl = newUnaryOp("dcos")
  def tan: Dbl = newUnaryOp("dtan")
  def asin: Dbl = newUnaryOp("dasin")
  def acos: Dbl = newUnaryOp("dacos")
  def atan: Dbl = newUnaryOp("datan")
  def sqrt: Dbl = newUnaryOp("dsqrt")
  def floor: Dbl = newUnaryOp("dfloor")
  def ceil: Dbl = newUnaryOp("dceil")
  def round: Dbl = newUnaryOp("dround")
  def log: Dbl = newUnaryOp("dlog")
  override def toSInt (): SInt = SInt(OUTPUT).fromNode(Op("dToSInt", fixWidth(64), this))
  override def toUInt (): UInt = UInt(OUTPUT).fromNode(Op("dToUInt", fixWidth(64), this))
}

/** Compute the sin value of a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]] in radians
  * @note This computes the value in scala on a literal and will not generate hardware */
object Sin {
  def apply (x: Flo): Flo = x.sin
  def apply (x: Dbl): Dbl = x.sin
}

/** Compute the cos value of a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]] in radians
  * @note This computes the value in scala on a literal and will not generate hardware */
object Cos {
  def apply (x: Flo): Flo = x.cos
  def apply (x: Dbl): Dbl = x.cos
}

/** Compute the tan value of a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]] in radians
  * @note This computes the value in scala on a literal and will not generate hardware */
object Tan {
  def apply (x: Flo): Flo = x.tan
  def apply (x: Dbl): Dbl = x.tan
}

/** Compute the asin or inverse sin value of a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]] returning the result in radians
  * @note This computes the value in scala on a literal and will not generate hardware */
object ASin {
  def apply (x: Flo): Flo = x.asin
  def apply (x: Dbl): Dbl = x.asin
}

/** Compute the acos or inverse cos value of a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]] returning the result in radians
  * @note This computes the value in scala on a literal and will not generate hardware */
object ACos {
  def apply (x: Flo): Flo = x.acos
  def apply (x: Dbl): Dbl = x.acos
}

/** Compute the atan or inverse tan value of a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]] returning the result in radians
  * @note This computes the value in scala on a literal and will not generate hardware */
object ATan {
  def apply (x: Flo): Flo = x.atan
  def apply (x: Dbl): Dbl = x.atan
}

/** Compute the square root of a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]]
  * @note This computes the value in scala on a literal and will not generate hardware */
object Sqrt {
  def apply (x: Flo): Flo = x.sqrt
  def apply (x: Dbl): Dbl = x.sqrt
}

/** Compute the floor of a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]]
  * @note This computes the value in scala on a literal and will not generate hardware */
object Floor {
  def apply (x: Flo): Flo = x.floor
  def apply (x: Dbl): Dbl = x.floor
}

/** Compute the ceiling of a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]]
  * @note This computes the value in scala on a literal and will not generate hardware */
object Ceil {
  def apply (x: Flo): Flo = x.ceil
  def apply (x: Dbl): Dbl = x.ceil
}

/** Round a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]] to the nearest integer
  * @note This computes the value in scala on a literal and will not generate hardware */
object Round {
  def apply (x: Flo): Flo = x.round
  def apply (x: Dbl): Dbl = x.round
}

/** Compute the log of a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]]
  * @note This computes the value in scala on a literal and will not generate hardware */
object Log {
  def apply (x: Flo): Flo = x.log
  def apply (x: Dbl): Dbl  = x.log
  /** @param p the base of the log to use */
  def apply (x: Flo, p: Flo): Flo = Log(x)/Log(p)
  /** @param p the base of the log to use */
  def apply (x: Dbl, p: Dbl): Dbl = Log(x)/Log(p)
}

/** Compute the power of a [[Chisel.Flo Flo]] or [[Chisel.Dbl Dbl]]
  * @note This computes the value in scala on a literal and will not generate hardware */
object Pow {
  /** Compute x^y */
  def apply (x: Flo, y: Flo): Flo = x.pow(y)
  /** Compute x^y */
  def apply (x: Dbl, y: Dbl): Dbl = x.pow(y)
}
