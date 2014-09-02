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
import Node._
import Reg._
import ChiselError._
import scala.reflect._

class GetWidthException(s: String) extends Exception(s)

object Reg {

  def regMaxWidth(m: => Node) =
    if (Driver.isInGetWidth) {
      throw new GetWidthException("getWidth was called on a Register or on an object connected in some way to a Register that has a statically uninferrable width")
    } else {
      maxWidth(m)
    }

  // Rule: If no width is specified, use max width. Otherwise, use the specified width.
/*
    The (implicit d: DummyImplicit) is to avoid the conflict with the
    above regWidth(w: => Width) method:
	  double definition:
	  method regWidth:(r: => Chisel.Node)(=> Chisel.Node) => Chisel.Width and
	  method regWidth:(w: => Chisel.Width)(=> Chisel.Node) => Chisel.Width at line 49
	  have same type after erasure: (r: Function0)Function1
	  def regWidth(r: => Node) = {
  */

  def regWidth(w: => Width)(implicit d: DummyImplicit) =
    if (! w.isKnown) {
      regMaxWidth _ ;
    } else {
      fixWidth(w.needWidth())	 // TODO 0WW
    }

  /** Rule: if r is using an inferred width, then don't enforce a width. If it is using a user inferred
    width, set the the width

    XXX Can't specify return type. There is a conflict. It is either
    (Node) => (Int) or Int depending which execution path you believe.
    */
  def regWidth(r: => Node) = {
    val rLit = r.litOf
    if (rLit != null && rLit.hasInferredWidth) {
      regMaxWidth _
    } else {
      fixWidth(r.getWidth)
    }
  }

  def validateGen[T <: Data](gen: => T) {
    for ((n, i) <- gen.flatten)
      if (!i.inputs.isEmpty)
        throwException("Invalid Type Specifier for Reg")
  }

  /** *type_out* defines the data type of the register when it is read.
    *update* and *reset* define the update and reset values
    respectively.
    */
  def apply[T <: Data](outType: T = null, next: T = null, init: T = null,
    clock: Clock = null): T = {
    var mType = outType
    if(mType == null) {
      mType = next
    }
    if(mType == null) {
      mType = init
    }
    if(mType == null) {
      throw new Exception("cannot infer type of Reg.")
    }

    val gen = mType.clone
    validateGen(gen)

    // asOutput flip the direction and returns this.
    val res = gen.asOutput

    if (init != null) for (((res_n, res_i), (rval_n, rval_i)) <- res.flatten zip init.flatten) {
      if (rval_i.getWidth < 0) ChiselError.error("Negative width to wire " + res_i)
      res_i.comp = new RegReset
      res_i.comp.init("", regWidth(rval_i), res_i.comp, rval_i)
      res_i.inputs += res_i.comp
    } else for ((res_n, res_i) <- res.flatten) {
      res_i.comp = new Reg
      val w = res_i.getWWidth
      res_i.comp.init("", regWidth(w), res_i.comp)
      res_i.inputs += res_i.comp
    }

    if (next != null) for (((res_n, res_i), (next_n, next_i)) <- res.flatten zip next.flatten) {
      res_i.comp.doProcAssign(next_i, Bool(true))
    }

    res.setIsTypeNode

    // set clock
    if (res.comp != null)
      res.comp.clock = clock
    else
      res.clock = clock

    res
  }

  /* Without this method, the scala compiler is not happy
   when we declare registers as Reg(signal). */
  def apply[T <: Data](outType: T): T = Reg[T](outType, null.asInstanceOf[T], null.asInstanceOf[T])
}


object RegNext {

  def apply[T <: Data](next: T): T = Reg[T](next, next, null.asInstanceOf[T])

  def apply[T <: Data](next: T, init: T): T = Reg[T](next, next, init)

}

object RegInit {

  def apply[T <: Data](init: T): T = Reg[T](init, null.asInstanceOf[T], init)

}

class RegReset extends Reg {
  override def assignReset(rst: => Bool): Boolean = {
    this.doProcAssign(inputs(1), rst)
    true
  }
}

class Reg extends Delay with proc {
  override def toString: String = {
    val nom = if (!name.isEmpty) name else pName
    "REG(" + nom + ")"
  }

  override def forceMatchingWidths: Unit =
    inputs.transform(_.matchWidth(width))

  override def usesInClockHi(n: Node) = n eq next

  // these are used to infer read enables on Mems
  protected[Chisel] def isEnable: Boolean = next.isInstanceOf[Mux] && (next.inputs(2) eq this)
  protected[Chisel] def enableSignal: Node = if (isEnable) next.inputs(0) else Bool(true)
  protected[Chisel] def updateValue: Node = if (isEnable) next.inputs(1) else next
}
