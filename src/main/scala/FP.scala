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
import ChiselError._

/// FLO

import java.lang.Float.floatToIntBits

object Flo {
  def apply(x: Float): Flo = Lit(floatToIntBits(x), 32){ Flo() }
  def apply(x: Double): Flo = Flo(x.toFloat);
  
  def apply(dir: IODirection = null): Flo = {
    val res = new Flo();
    res.dir = dir;
    res.init("", fixWidth(32))
    res
  }
}

class Flo extends Bits with Num[Flo] {
  // setIsSigned

  // override def setIsTypeNode = {inputs(0).setIsSigned; super.setIsTypeNode}

  type T = Flo;
  override def fromNode(n: Node): this.type = {
    val res = Flo(OUTPUT).asInstanceOf[this.type]; 
    res assign n; 
    res};

  override def fromInt(x: Int): this.type = {
    Flo(x.toFloat).asInstanceOf[this.type]
  }

  override protected def colonEquals(that: Bits): Unit = that match {
    case _: Flo => super.colonEquals(that)
    case _ => illegalAssignment(that)
  }

  def gen[T <: Bits](): T = Flo().asInstanceOf[T];

  def unary_-(): Flo = newUnaryOp("f-")
  def +  (b: Flo): Flo = newBinaryOp(b, "f+")
  def -  (b: Flo): Flo = newBinaryOp(b, "f-")
  def *  (b: Flo): Flo = newBinaryOp(b, "f*")
  def /  (b: Flo): Flo = newBinaryOp(b, "f/")
  def %  (b: Flo): Flo = newBinaryOp(b, "f%")
  def ===(b: Flo): Bool = newLogicalOp(b, "f==")
  def != (b: Flo): Bool = newLogicalOp(b, "f!=")
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

import java.lang.Double.doubleToLongBits

object Dbl {

  def apply(x: Float): Dbl = Dbl(x.toDouble);
  def apply(x: Double): Dbl = Lit(doubleToLongBits(x), 64){ Dbl() }
  
  def apply(dir: IODirection = null): Dbl = {
    val res = new Dbl();
    res.dir = dir;
    res.init("", fixWidth(64))
    res
  }
}

class Dbl extends Bits with Num[Dbl] {
  // setIsSigned

  // override def setIsTypeNode = {inputs(0).setIsSigned; super.setIsTypeNode}

  type T = Dbl;
  override def fromNode(n: Node) = {
    val res = Dbl(OUTPUT).asInstanceOf[this.type]; 
    res assign n; 
    res};

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
  def != (b: Dbl): Bool = newLogicalOp(b, "d!=")
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

object Sin {
  def apply (x: Flo): Flo = x.sin
  def apply (x: Dbl): Dbl = x.sin
}

object Cos {
  def apply (x: Flo): Flo = x.cos
  def apply (x: Dbl): Dbl = x.cos
}

object Tan {
  def apply (x: Flo): Flo = x.tan
  def apply (x: Dbl): Dbl = x.tan
}

object ASin {
  def apply (x: Flo): Flo = x.asin
  def apply (x: Dbl): Dbl = x.asin
}

object ACos {
  def apply (x: Flo): Flo = x.acos
  def apply (x: Dbl): Dbl = x.acos
}

object ATan {
  def apply (x: Flo): Flo = x.atan
  def apply (x: Dbl): Dbl = x.atan
}

object Sqrt {
  def apply (x: Flo): Flo = x.sqrt
  def apply (x: Dbl): Dbl = x.sqrt
}

object Floor {
  def apply (x: Flo): Flo = x.floor
  def apply (x: Dbl): Dbl = x.floor
}

object Ceil {
  def apply (x: Flo): Flo = x.ceil
  def apply (x: Dbl): Dbl = x.ceil
}

object Round {
  def apply (x: Flo): Flo = x.round
  def apply (x: Dbl): Dbl = x.round
}

object Log {
  def apply (x: Flo): Flo = x.log
  def apply (x: Dbl): Dbl  = x.log
  def apply (x: Flo, p: Flo): Flo = Log(x)/Log(p)
  def apply (x: Dbl, p: Dbl): Dbl = Log(x)/Log(p)
}

object Pow {
  def apply (x: Flo, y: Flo): Flo = x.pow(y)
  def apply (x: Dbl, y: Dbl): Dbl = x.pow(y)
}
