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

import scala.math.max

import ChiselError._

/* backward compatibility */
object Bits {
  def apply(x: Int): UInt = UInt(x)
  def apply(x: Int, width: Int): UInt = UInt(x, width)
  def apply(x: String): UInt = UInt(x)
  def apply(x: String, width: Int): UInt = UInt(x, width)
  def apply(x: BigInt): UInt = UInt(x)
  def apply(x: BigInt, width: Int): UInt = UInt(x, width)

  def apply(dir: IODirection = NODIRECTION, width: Int = -1): UInt
    = UInt(dir, width);
}


/** Base class for Chisel built-in types like UInt and SInt.

  Implementation Note:
  A no-argument constructor is different from a constructor with default
  arguments. Since we will use reflection to create instances of subclasses
  of Bits, we must use the no-argument constructor form.
  */
abstract class Bits extends Data {

  var node: Node = null

  /* Node to attach the default value while building a mux tree through
   conditional assigments. */
  var default: MuxOp = null

  /** Stores the lvalue generated for an IOBound node. */
  var cachedLValue: Node = null

  Module.ioMap += ((this, Module.ioCount));
  Module.ioCount += 1;

  override def asDirectionless(): this.type = {
    if( node != null ) node.asDirectionless()
    this
  }

  override def asInput(): this.type = {
    if( node != null ) node.asInput()
    this
  }

  override def asOutput(): this.type = {
    if( node != null ) node.asOutput()
    this
  }

  override def nodes(): Seq[Node] = node :: Nil

  override def flatten: Array[(String, Bits)] = Array((name, this));

  override def flip(): this.type = {
    if( node != null ) node.flip()
    this
  }

  override def fromBits( bits: Bits ): this.type = {
    this.node = bits.node
    this.cachedLValue = bits.cachedLValue
    this
  }

  /** Infer widths on the partially constructed graph rooted at this
    and returns the width of this node.
  */
  def getWidth(): Int = {
    val inferWF = new InferWidthForward
    GraphWalker.tarjan(node :: Nil, inferWF)
    node.width
  }

  /** Returns ``true`` when this Bits instance is bound to a ``Node``
    that generates a constant signal.
    */
  def isConst: Boolean = {
    node != null && node.isInstanceOf[Literal]
  }

  /** Returns the lvalue associated with the node */
  def lvalue(): Node = {
    node match {
      case memref: MemReference => {
        val read = if( memref.addr.isInstanceOf[RegDelay] ) {
          new MemSeqRead(memref.mem, memref.addr)
        } else {
          new MemRead(memref.mem, memref.addr)
        }
        read
      }
      case vecref: VecReference[_] => {
        VecMux(vecref.addr, vecref.elts).toBits.node
      }
      case iob: IOBound =>
        if( iob.isDirected(OUTPUT)
          && !Module.scope.compStack.isEmpty
          && iob.component != Module.scope.compStack.top ) {
          /* We are using an output of a child module, we thus
           need to make an intermediate variable holder here. */
          if( cachedLValue == null ) {
            cachedLValue = new IOBound(INPUT, -1, iob)
          }
          cachedLValue
        } else {
          iob
        }
        case _ => node
    }
  }

  /** Assign and returns the rvalue associated with the node */
  def rvalue( value: Node ) = {
    node match {
      case memref: MemReference =>
        val write = new MemWrite(memref.mem, memref.addr,
          value, Module.scope.genCond())

      case vecref: VecReference[_] => {
        val onehot = VecUIntToOH(vecref.addr, vecref.elts.length)
        Module.searchAndMap = true
        for( (elem , i) <- vecref.elts.zipWithIndex ) {
          when( Vec.getEnable(onehot, i) ) {
            elem := UInt(value)
          }
        }
        Module.searchAndMap = false
      }
      case regd: RegDelay => {
        if( regd.inputs.length > regd.CLOCK_NEXT ) {
          regd.inputs(regd.CLOCK_NEXT) = value
        } else {
          regd.inputs.append(value)
        }
        if( !Module.scope.isDefaultCond() ) {
          val cond = Module.scope.genCond()
          regd.setEnable(if (regd.enable != null) new LogicalOrOp(regd.enable, cond) else cond)
        }
      }
      case iob: IOBound =>
        iob.inputs.clear()
        iob.inputs.append(value)
      case node =>
        ChiselError.error("cannot assign to wire net")
    }
  }

  override def nameIt(name: String): this.type = {
    if( !named ) {
     if( node != null ) node.nameIt(name)
      this.name = name
    }
    this
  }

  /* Assignment to this */
  def procAssign(src: Bits) {
    if( Module.scope.isDefaultCond() ) {
      if( default != null ) {
        if( default.otherwise != null ) {
          /* We are dealing with a default assignement but the default
           position is already occupied. */
          ChiselError.warning("re-assignment to mux (" + default + ") under default condition.")
        } else {
          default.inputs.append(src.lvalue())
        }
      } else {
        if( node != null ) {
          if( node.assigned != null ) {
            /* We are dealing with a default assignement, there are no
             mux tree but the default position is already occupied. */
            ChiselError.warning(
              "re-assignment to node under default condition.")
          }
          rvalue(src.lvalue())
        } else {
          node = src.lvalue()
        }
      }
    } else {
      if( node != null && node.assigned != null ) {
        rvalue(new MuxOp(
          Module.scope.genCond(), src.lvalue(), node.assigned))
      } else {
        /* First assignment we construct a mux tree with a dangling
         default position. */
        default = new MuxOp(Module.scope.genCond(), src.lvalue())
        if( node != null ) {
          rvalue(default)
        } else {
          node = default
        }
      }
    }
  }

  override def toBits(): UInt = UInt(this.node)

  override def toString: String = {
    var str = (
      "/*" + (if (name != null && !name.isEmpty) name else "?")
        + (if (component != null) (" in " + component) else "") + "*/ "
        + getClass.getName + "("
        + "node=" + node
        + (if( node != null) (", width=" + node.width) else ""))
    str = str + ")"
    str
  }

  /* The <> operator bulk connects interfaces of opposite direction between
   sibling modules or interfaces of same direction between parent/child modules.

   Bulk connections connect leaf ports using pathname matching. Connections
   are only made if one of the ports is non-null, allowing users to repeatedly
   bulk-connect partially filled interfaces.

   After all connections are made and the circuit is being elaborated,
   Chisel warns users if ports have other than exactly one connection to them.
  */
  override def <>(right: Data): Unit = {
    right match {
      case other: Bits => this <> other;
      case _ => this <> right.toBits()
    }
  }

  def <>(right: Bits) {
    var parent: Bits = null
    var cousin: Bits = null
    if( this.node.component == Module.scope.compStack.top ) {
      if( right.node.component == Module.scope.compStack.top ) {
        /* Both nodes are sibblings in the current component.
         We wire INPUT <> OUTPUT. */
      } else {
        /* The left node is in the current component while
         the right node is not (i.e. child or cousin).
         We wire INPUT > INPUT and OUTPUT > OUTPUT. */
        parent = this
        cousin = right
      }
    } else {
      if( right.node.component == Module.scope.compStack.top ) {
        /* The right node is in the current component while
         the left node is not (i.e. child or cousin).
         We wire INPUT > INPUT and OUTPUT > OUTPUT. */
        parent = right
        cousin = this
      } else {
        /* Both nodes are sibblings. We wire INPUT <> OUTPUT. */
      }
    }

    if( parent != null ) {
      parent.node match {
        case leftBond: IOBound =>
          cousin.node match {
            case rightBond: IOBound => {
              if(leftBond.isDirected(INPUT) && rightBond.isDirected(INPUT) ) {
                cousin := parent
              } else if (leftBond.isDirected(OUTPUT) && rightBond.isDirected(OUTPUT) ) {
                parent := cousin
              } else {
                ChiselError.error("matching parent against child/cousin with different directions.")
              }
            }
            case _ =>
              parent := cousin
          }
      }
    } else {
      node match {
        case leftBond: IOBound =>
          right.node match {
            case rightBond: IOBound => {
              if(leftBond.isDirected(INPUT) && rightBond.isDirected(OUTPUT) ) {
                this := right
              } else if (leftBond.isDirected(OUTPUT) && rightBond.isDirected(INPUT) ) {
                right := this
              } else {
                // XXX Is it OK to match sibblings with same directions?
                this := right
              }
            }
            case _ =>
              this := right
          }
      }
    }
  }

  override def clone: this.type = {
    val res = this.getClass.newInstance.asInstanceOf[this.type];
    res.node = new IOBound(NODIRECTION, this.getWidth)
    res
  }

  /** Assignment operator.

    The assignment operator can be called multiple times
   */
  override def :=(src: Data): Unit = {
    src match {
      case bits: Bits =>
        this procAssign bits;
      case _ =>
        super.:=(src)
    }
  }


  // bitwise operators
  // =================

  /** Extract a single Bool at index *bit*.
    */
  final def apply(bit: Int): Bool = Extract(this, UInt(bit))
  final def apply(bit: UInt): Bool = Extract(this, bit)

  /** Extract a range of bits */
  final def apply(hi: Int, lo: Int): UInt = Extract(this, UInt(hi), UInt(lo))
  final def apply(hi: UInt, lo: UInt): UInt = Extract(this, hi, lo)

/** can't define apply(range: (UInt, UInt)) because it gets same
  signature after type erasure. */
  final def apply(range: (Int, Int)): UInt = this(range._1, range._2)

  // to support implicit convestions
  override def ===(right: Data): Bool = {
    right match {
      case bits: Bits => Eql(this, bits)
      case _ => this === right.toBits
    }
  }

  def unary_~(): UInt = BitwiseRev(this)
  def andR(): Bool = ReduceAnd(this)
  def orR(): Bool = ReduceOr(this)
  def xorR(): Bool = ReduceXor(this)
  def != (right: Bits): Bool = Neq(this, right)
  def & (right: Bits): this.type = And(this, right)
  def | (right: Bits): this.type = Or(this, right)
  def ^ (right: Bits): this.type = Xor(this, right)

  def ##(right: Bits): UInt = Cat(this, right)
}


object BitwiseRev {
  def apply(opand: Bits): UInt = {
    UInt(
      if( opand.isConst ) {
        Literal((-opand.node.asInstanceOf[Literal].value - 1)
          & ((BigInt(1) << opand.node.width) - 1),
          opand.node.width)
      } else {
        new BitwiseRevOp(opand.lvalue())
      })
  }
}


object And {
  def apply[T <: Bits](left: T, right: Bits)(implicit m: Manifest[T]): T = {
    val op = if( left.isConst && right.isConst ) {
      Literal(left.node.asInstanceOf[Literal].value
        & right.node.asInstanceOf[Literal].value,
        max(left.node.width, right.node.width))
    } else {
      new AndOp(left.lvalue(), right.lvalue())
    }
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}


object Eql {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(
      if( left.isConst && right.isConst ) {
        Literal(if (left.node.asInstanceOf[Literal].value
          == right.node.asInstanceOf[Literal].value) 1 else 0)
      } else {
        new EqlOp(left.lvalue(), right.lvalue())
      })
  }
}


object Extract {

  def apply(opand: Bits, bit: UInt): Bool = Bool(apply(opand, bit, bit).node)

  // extract bit range
  def apply(opand: Bits, hi: Bits, lo: Bits): UInt = {
    UInt(
      if( opand.isConst && hi.isConst && lo.isConst ) {
        val w = (hi.node.asInstanceOf[Literal].value.toInt
            - lo.node.asInstanceOf[Literal].value.toInt + 1)
        Literal((opand.node.asInstanceOf[Literal].value
          >> lo.node.asInstanceOf[Literal].value.toInt)
          & ((BigInt(1) << w) - BigInt(1)), w)
      } else if( opand.isConst ) {
        /* XXX Why would this be better than an ExtractOp? */
        val rsh = new RightShiftOp(opand.lvalue(), lo.lvalue())
        val hiMinusLoPlus1 = new AddOp(
          new SubOp(hi.lvalue(), lo.lvalue()), Literal(1))
        val mask = new SubOp(
          new LeftShiftOp(Literal(1), hiMinusLoPlus1), Literal(1))
        new AndOp(rsh, mask)
      } else {
        val n = new ExtractOp(opand.lvalue(), hi.lvalue(), lo.lvalue())
        n
      })
  }
}

object Add {
  def apply[T <: Bits]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        Literal(left.node.asInstanceOf[Literal].value
          + right.node.asInstanceOf[Literal].value,
          max(left.node.width, right.node.width) + 1) // XXX does not always need carry.
      } else {
        new AddOp(left.lvalue(), right.lvalue())
      }
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}

object Fill {

  def apply(n: Int, opand: Bits): UInt = apply(opand, n)

  def apply(opand: Bits, n: Int): UInt = {
    UInt(
      if( opand.isConst ) {
        var c = BigInt(0)
        val w = opand.node.width
        val a = opand.node.asInstanceOf[Literal].value
        for (i <- 0 until n)
          c = (c << w) | a
        Literal(c, n * w)
      } else if( n == 1 ) {
        opand.lvalue()
      } else {
        new FillOp(opand.lvalue(), n)
      })
  }
}


object LeftShift {
  def apply[T <: Bits](left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        Literal(left.node.asInstanceOf[Literal].value
          << right.node.asInstanceOf[Literal].value.toInt,
          left.node.width + right.node.width)
      } else {
        new LeftShiftOp(left.lvalue(), right.lvalue())
      }
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}


object LogicalNeg {
  def apply( opand: Bits): Bool = {
    Bool(
      if( opand.isConst ) {
        if( opand.node.asInstanceOf[Literal].value == 0) Literal(1)
        else Literal(0)
      } else {
        new LogicalNegOp(opand.lvalue())
      })
  }
}


object RightShift {
  def apply[T <: Bits](left: T, right: UInt)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        if( left.isInstanceOf[UInt] ) {
          Literal(left.node.asInstanceOf[Literal].value
            >> right.node.asInstanceOf[Literal].value.toInt,
            left.node.width - right.node.width)
        } else {
          /* XXX BigInt signed right shift? */
          Literal(left.node.asInstanceOf[Literal].value
            >> right.node.asInstanceOf[Literal].value.toInt,
            left.node.width - right.node.width)
        }
      } else {
        if( left.isInstanceOf[UInt] ) {
          new RightShiftOp(left.lvalue(), right.lvalue())
        } else {
          new RightShiftSOp(left.lvalue(), right.lvalue())
        }
      }
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}


object Neq {
  def apply[T <: Bits]( left: T, right: T): Bool = {
    Bool(
      if( left.isConst && right.isConst ) {
        Literal(if (left.node.asInstanceOf[Literal].value
          != right.node.asInstanceOf[Literal].value) 1 else 0)
      } else {
        new NeqOp(left.lvalue(), right.lvalue())
      })
  }
}


object Or {
  def apply[T <: Bits](left: T, right: Bits)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        Literal(left.node.asInstanceOf[Literal].value
          | right.node.asInstanceOf[Literal].value,
          max(left.node.width, right.node.width))
      } else {
        new OrOp(left.lvalue(), right.lvalue())
      }
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}


object Xor {
  def apply[T <: Bits](left: T, right: Bits)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        Literal(left.node.asInstanceOf[Literal].value
          ^ right.node.asInstanceOf[Literal].value,
          max(left.node.width, right.node.width))
      } else {
        new XorOp(left.lvalue(), right.lvalue())
      }
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}


object ReduceAnd {
  def apply[T <: Bits](opand: T): Bool = {
    val op = new ReduceAndOp(opand.lvalue())
    Bool(op)
  }
}

object andR {
    def apply(x: Bits): Bool = ReduceAnd(x)
}


object ReduceOr {
  def apply[T <: Bits](opand: T): Bool = {
    val op = new ReduceOrOp(opand.lvalue())
    Bool(op)
  }
}

object orR {
    def apply(x: Bits): Bool = ReduceOr(x)
}


object ReduceXor {
  def apply[T <: Bits](opand: T): Bool = {
    val op = new ReduceXorOp(opand.lvalue())
    Bool(op)
  }
}

object xorR {
    def apply(x: Bits): Bool = ReduceXor(x)
}

object Sub {
  def apply[T <: Bits]( left: T, right: T)(implicit m: Manifest[T]): T = {
    val op =
      if( left.isConst && right.isConst ) {
        Literal(left.node.asInstanceOf[Literal].value
          - right.node.asInstanceOf[Literal].value,
          max(left.node.width, right.node.width) + 1) // XXX unnecessary carry?
      } else {
        new SubOp(left.lvalue(), right.lvalue())
      }
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
    result.node = op
    result
  }
}
