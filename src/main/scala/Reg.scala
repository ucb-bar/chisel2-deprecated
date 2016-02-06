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

class GetWidthException(s: String) extends Exception(s)

object Reg {

  def regMaxWidth(m: => Node) =
    if (Driver.isInGetWidth) {
      throw new GetWidthException("getWidth was called on a Register or on an object connected in some way to a Register that has a statically uninferrable width")
    } else {
      Node.maxWidth(m)
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
      Node.fixWidth(w.needWidth())  // TODO 0WW
    }

  /** Rule: if r is using an inferred width, then don't enforce a width. If it is using a user inferred
    width, set the the width

    XXX Can't specify return type. There is a conflict. It is either
    (Node) => (Int) or Int depending which execution path you believe.
    */
  def regWidth(r: => Node) = r.litOpt match {
    case Some(rl) if rl.hasInferredWidth => regMaxWidth _
    case _ => Node.fixWidth(r.getWidth)
  }

  def validateGen[T <: Data](gen: => T) {
    for ((n, i) <- gen.flatten if !i.inputs.isEmpty)
      throwException("Invalid Type Specifier for Reg: element \"%s\" has %d inputs (possibly an initial value?)".format(n, i.inputs.size))
  }

  /** *type_out* defines the data type of the register when it is read.
    *update* and *reset* define the update and reset values
    respectively.
    */
  def apply[T <: Data](outType: T = null, next: T = null, init: T = null, clock: Clock = null): T =
    apply(Option(outType), Option(next), Option(init), Option(clock))

  def apply[T <: Data](outType: Option[T], next: Option[T], init: Option[T], clock: Option[Clock]): T = {
    val gen = (outType match {case Some(t) => t case None =>
      next match { case Some(t) => t case None =>
      init match { case Some(t) => t case None =>
        throwException("cannot infer type of Reg.")}}}).cloneType
    validateGen(gen)

    // asOutput flip the direction and returns this.
    val res = gen.asOutput
    init match {
      case None => for (r <- res.flatten.unzip._2) {
        val p = new Reg
        val w = r.getWidthW()
        p.init("", regWidth(w), p)
        r.inputs += p
        r.comp = Some(p)
      }
      case Some(p) => for ((r, i) <- res.flatten.unzip._2 zip p.flatten.unzip._2) {
        if (i.getWidth < 0) ChiselError.error("Negative width to wire " + res)
        val p = new RegReset
        p.init("", regWidth(i), p, i)
        r.inputs += p
        r.comp = Some(p)
      }
    }
    next match {
      case None =>
      case Some(p) => for ((r, n) <- res.flatten.unzip._2 zip p.flatten.unzip._2) {
        r.comp match {
          case None => // Todo: Error!
          case Some(p) => p doProcAssign (n, Bool(true))
        }
      }
    }
    res.setIsTypeNode
    // set clock
    res.flatten.unzip._2 foreach (sig => sig.comp match {
      case None => sig.clock = clock
      case Some(p) => p.clock = clock
    })
    res
  }

  /* Without this method, the scala compiler is not happy
   when we declare registers as Reg(signal). */
  def apply[T <: Data](outType: T): T = Reg[T](Some(outType), None, None, None)
}


object RegNext {
  def apply[T <: Data](next: T): T = Reg[T](Some(next), Some(next), None, None)
  def apply[T <: Data](next: T, init: T): T = Reg[T](Some(next), Some(next), Some(init), None)
}

object RegInit {
  def apply[T <: Data](init: T): T = Reg[T](Some(init), None, Some(init), None)
}

class RegReset extends Reg {
  override def assignReset(rst: => Bool): Boolean = {
    this.doProcAssign(inputs(1), rst)
    true
  }
}

class Reg extends Delay with proc {
  override def toString: String = "REG(" + name + ")"

  override def forceMatchingWidths: Unit =
    inputs.transform(_.matchWidth(widthW))

  override def usesInClockHi(n: Node) = n eq next

  override def doProcAssign(src: Node, cond: Bool) {
    if (procAssigned || isEnable) inputs(0) = Multiplex(cond, src, inputs(0))
    else super.doProcAssign(src, cond)
  }

  // these are used to infer read enables on Mems
  // also useful for custom transforms
  def isEnable: Boolean = next.isInstanceOf[Mux] && (next.inputs(2).getNode eq this)
  def enableSignal: Node = if (isEnable) next.inputs(0) else Bool(true)
  def updateValue: Node = if (isEnable) next.inputs(1) else next
  // Chisel3 - this node contains data - used for verifying Wire() wrapping
  override def isTypeOnly = false
}
