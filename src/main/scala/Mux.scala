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

import scala.math._


object MuxLookup {
  def apply[S <: UInt, T <: Bits](key: S, default: T, mapping: Seq[(S, T)])
    (implicit m: reflect.ClassTag[T]): T
  = {
    var res = default;
    for ((k, v) <- mapping.reverse)
      res = Mux(key === k, v, res);
    res
  }

}

object MuxCase {
  def apply[T <: Bits](default: T, mapping: Seq[(Bool, T)])
    (implicit m: reflect.ClassTag[T]): T = {
    var res = default;
    for ((t, v) <- mapping.reverse){
      res = Mux(t, v, res);
    }
    res
  }
}


object Mux {
  def apply[T <: Data](t: Bool, c: T, a: T): T = {
    /* XXX Cannot use classTag on Bundle/Vec until we solve
     the cloning issue. Passed that there is also the type erasure
     issue when executing a VecMux from a VecReference.
    val result = m.runtimeClass.newInstance.asInstanceOf[T]
     */
    val result = a.clone
    result.fromBits(UInt(Mux(t.lvalue, c.toBits.lvalue, a.toBits.lvalue)))
    result
  }

  // Internal Node version to avoid duplicating constant folding logic
  def apply(t: Node, c: Node, a: Node): Node = {
    if (t.isConst) {
      if (t.asInstanceOf[Literal].value == 0) a else c
    } else if (c.isInstanceOf[Literal] && a.isInstanceOf[Literal]) {
      if (c.asInstanceOf[Literal].value == a.asInstanceOf[Literal].value &&
          c.width == a.width) {
        c
      } else if (c.asInstanceOf[Literal].value == 1 &&
                 a.asInstanceOf[Literal].value == 0 &&
                 c.width == 1 && a.width == 1) {
        // Transform Mux(t, 1, 0) into t
        t
      } else if (c.asInstanceOf[Literal].value == 0 &&
                 a.asInstanceOf[Literal].value == 1 &&
                 c.width == 1 && a.width == 1) {
        // Transform Mux(t, 0, 1) into !t
        LogicalNeg(t)
      } else {
        new MuxOp(t, c, a)
      }
    } else if (a.isInstanceOf[MuxOp] && c.clearlyEquals(a.inputs(1))) {
      new MuxOp(new LogicalOrOp(t, a.inputs(0)), c, a.inputs(2))
    } else {
      new MuxOp(t, c, a)
    }
  }
}


/** Generate a mux tree that returns the item in *elts* at position *addr*.
  */
object VecMux {

  def apply(addr: UInt, elts: Seq[Data]): Data = {
    def doit(elts: Seq[Data], pos: Int): Data = {
      if (elts.length == 1) {
        elts(0)
      } else {
        val newElts = (0 until elts.length/2).map(i => Mux(addr(pos), elts(2*i + 1), elts(2*i)))
        doit(newElts ++ elts.slice(elts.length/2*2, elts.length), pos + 1)
      }
    }
    doit(elts, 0)
  }
}


