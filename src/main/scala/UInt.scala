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

object UInt {
  /* Implementation Note: scalac does not allow multiple overloaded
   method with default parameters so we define the following four
   methods to create UInt from litterals (with implicit and explicit
   widths) and reserve the default parameters for the "direction" method.
   */
  def apply(x: Int): UInt = UInt(Literal(x))
  def apply(x: Int, width: Int): UInt = UInt(Literal(x, width))
  def apply(x: String): UInt = UInt(Literal(x, -1))
  def apply(x: String, width: Int): UInt = UInt(Literal(x, width))
  def apply(x: String, base: Char): UInt = UInt(Literal(x, base, -1))
  def apply(x: String, base: Char, width: Int): UInt
    = UInt(Literal(x, base, width))
  def apply(x: BigInt): UInt = UInt(Literal(x))
  def apply(x: BigInt, width: Int): UInt = UInt(Literal(x, width))

  def apply(dir: IODirection = NODIRECTION, width: Int = -1): UInt = {
    val res = new UInt()
    res.node = new IOBound(dir, width)
    res
  }

  def apply(node: Node): UInt = {
    val res = new UInt()
    res.node = node
    res
  }
}


class UInt extends Bits /* XXX with Numeric[UInt] */ {
  type T = UInt;

  def toBool(): Bool = {
    Bool(this.node)
  }

  // unary operators
  def zext: UInt = UInt(0, 1) ## this
  def unary_-(): SInt = SignRev(this.zext)
  def unary_!(): Bool = LogicalNeg(this)

  // arithmetic operators

  def << (right: UInt): UInt = LeftShift(this, right)
  def >> (right: UInt): UInt = RightShift(this, right)

// XXX deprecated?  def ?  (b: UInt): UInt = newBinaryOp(b, "?");

  def + (right: UInt): UInt = Add(this, right)
  def -  (right: UInt): UInt = Sub(this, right)
  def * (right: UInt): UInt = Mul(this, right)
  def % (right: UInt): UInt = Rem(this, right)
  def / (right: UInt): UInt = Div(this, right)

  // order operators
  def >  (right: UInt): Bool = Gtr(this, right)
  def <  (right: UInt): Bool = Ltn(this, right)
  def <= (right: UInt): Bool = Lte(this, right)
  def >= (right: UInt): Bool = Gte(this, right)

  //UInt op SInt arithmetic
  def + (right: SInt): SInt = Add(SInt(this.zext.node), right)
  def - (right: SInt): SInt = Sub(SInt(this.zext.node), right)
  def * (right: SInt): SInt = MulSU(right, this.zext)
  def % (right: SInt): SInt = RemUS(this.zext, right)
  def / (right: SInt): SInt = DivUS(this.zext, right)
}

object Div {
  def apply( left: UInt, right: UInt): UInt = {
    UInt(new DivOp(left.lvalue(), right.lvalue()))
  }
}

object DivUS {
  def apply[T <: SInt]( left: UInt, right: T)(implicit m: Manifest[T]): T = {
    val op = new DivUSOp(left.lvalue(), right.lvalue())
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object Gte {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(
      if( left.isConst && right.isConst ) {
        Literal(if (left.node.asInstanceOf[Literal].value
          >= right.node.asInstanceOf[Literal].value) 1 else 0)
      } else {
        new GteOp(left.lvalue(), right.lvalue())
      })
  }
}

object Gtr {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(
      if( left.isConst && right.isConst ) {
        Literal(if (left.node.asInstanceOf[Literal].value
          > right.node.asInstanceOf[Literal].value) 1 else 0)
      } else {
        new GtrOp(left.lvalue(), right.lvalue())
    })
  }
}

object Log2 {
  def apply (opand: Bits, n: Int): UInt = {
    UInt(new Log2Op(opand.lvalue(), Literal.sizeof(n-1)))
  }
}

object Lte {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(if( left.isConst && right.isConst ) {
      Literal(if (left.node.asInstanceOf[Literal].value
        <= right.node.asInstanceOf[Literal].value) 1 else 0)
    } else {
      new LteOp(left.lvalue(), right.lvalue())
    })
  }
}

object Ltn {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(
      if( left.isConst && right.isConst ) {
        Literal(if (left.node.asInstanceOf[Literal].value
          < right.node.asInstanceOf[Literal].value) 1 else 0)
      } else {
        new LtnOp(left.lvalue(), right.lvalue())
      })
  }
}

object Mul {
  def apply[T <: UInt]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op = new MulOp(left.lvalue(), right.lvalue())
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object Rem {
  def apply[T <: UInt]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op = new RemOp(left.lvalue(), right.lvalue())
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object RemUS {
  def apply[T <: SInt]( left: UInt, right: T)(implicit m: Manifest[T]): T = {
    val op = new RemUSOp(left.lvalue(), right.lvalue())
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}


