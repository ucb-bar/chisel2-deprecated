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
import ChiselError._

object SInt {

  def apply(x: Int): SInt = SInt(Literal(x))
  def apply(x: Int, width: Int): SInt = SInt(Literal(x, width))
  def apply(x: BigInt): SInt = SInt(Literal(x))
  def apply(x: BigInt, width: Int): SInt = SInt(Literal(x, width))

  def apply(dir: IODirection = NODIRECTION, width: Int = -1): SInt = {
    val res = new SInt()
    res.node = new IOBound(dir, width)
    res
  }

  def apply(node: Node): SInt = {
    val res = new SInt()
    res.node = node
    res
  }
}

class SInt extends Bits {

  type T = SInt;

  /** casting from UInt followed by assignment. */
  override def :=(src: Data): Unit = {
    src match {
      case uint: UInt =>
        this procAssign uint.zext;
      case _ =>
        super.:=(src)
    }
  }

  def gen[T <: Bits](): T = SInt().asInstanceOf[T];

  // arithmetic operators
  def unary_-(): SInt = SignRev(this)
  def unary_!(): Bool = LogicalNeg(this)
  def << (right: UInt): SInt = LeftShift(this, right)
  def >> (right: UInt): SInt = RightShift(this, right)
// XXX deprecated  def ?  (b: SInt): SInt = newBinaryOp(b, "?");

  // order operators
  def <  (right: SInt): Bool = LtnS(this, right)
  def <= (right: SInt): Bool = LteS(this, right)
  def >  (right: SInt): Bool = right < this
  def >= (right: SInt): Bool = right <= this
  def !=  (right: UInt): Bool = this != right.zext
  def >   (right: UInt): Bool = this > right.zext
  def <   (right: UInt): Bool = this < right.zext
  def >=  (right: UInt): Bool = this >= right.zext
  def <=  (right: UInt): Bool = this <= right.zext

  override def ===(right: Data): Bool = {
    right match {
      case right: UInt => UInt(this.lvalue()) === right.zext;
      case _ => super.===(right)
    }
  }

  //SInt to SInt arithmetic
  def +  (right: SInt): SInt = Add(this, right)
  def *  (right: SInt): SInt = MulS(this, right)
  def /  (right: SInt): SInt = DivS(this, right)
  def %  (right: SInt): SInt = RemS(this, right)
  def -  (right: SInt): SInt = Sub(this, right)

  //SInt to UInt arithmetic
  def +   (right: UInt): SInt = this + right.zext;
  def -   (right: UInt): SInt = this - right.zext;
  def *   (right: UInt): SInt = MulSU(this, right)
  def /   (right: UInt): SInt = DivSU(this, right)
  def %   (right: UInt): SInt = RemSU(this, right)
  def abs: UInt = Mux(this < SInt(0), UInt((-this).node), UInt(this.node))
}


object SignRev {
  def apply(opand: Bits): SInt = {
    SInt(
      if( opand.isConst ) {
        Literal(-opand.node.asInstanceOf[Literal].value, opand.node.width)
      } else {
        new SignRevOp(opand.lvalue())
      })
  }
}

object DivS {
  def apply[T <: SInt]( left: SInt, right: SInt): SInt = {
      SInt(new DivSOp(left.lvalue(), right.lvalue()))
  }
}

object DivSU {
  def apply[T <: SInt]( left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op = new DivSUOp(left.lvalue(), right.zext.lvalue())
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object MulS {
  def apply[T <: SInt]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op = new MulSOp(left.lvalue(), right.lvalue())
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object MulSU {
  def apply[T <: SInt]( left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op = new MulSUOp(left.lvalue(), right.zext.lvalue())
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object RemS {
  def apply[T <: SInt]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op = new RemSOp(left.lvalue(), right.lvalue())
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object RemSU {
  def apply[T <: SInt]( left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op = new RemSUOp(left.lvalue(), right.zext.lvalue())
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object LteS {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Lte(left, right)
  }
}

object LtnS {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Ltn(left, right)
  }
}
