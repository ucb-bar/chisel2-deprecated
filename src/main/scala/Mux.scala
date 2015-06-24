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
import scala.math._

object MuxLookup {
  def apply[S <: UInt, T <: Bits] (key: S, default: T, mapping: Seq[(S, T)]): T = {
    var res = default;
    for ((k, v) <- mapping.reverse)
      res = Mux(key === k, v, res);
    res
  }

}

object MuxCase {
  def apply[T <: Bits] (default: T, mapping: Seq[(Bool, T)]): T = {
    var res = default;
    for ((t, v) <- mapping.reverse){
      res = Mux(t, v, res);
    }
    res
  }
}

object Multiplex{
  def apply (t: Node, c: Node, a: Node): Node = {
    if (t.litOf != null) {
      return if (t.litOf.value == 0) a else c
    }
    if (a != null && a.isInstanceOf[Mux] && t._isComplementOf(a.inputs(0))) {
      return Multiplex(t, c, a.inputs(1))
    }
    if (c.litOf != null && a != null && a.litOf != null) {
      if (c.litOf.value == a.litOf.value) {
        return c
      }
      if (c.litOf.isKnownWidth && a.litOf.isKnownWidth
          && c.litOf.widthW.needWidth() == 1 && a.litOf.widthW.needWidth() == 1) {
        return if (c.litOf.value == 0) LogicalOp(t, Literal(0,1), "===") else t
      }
    }
    new Mux().init("", maxWidth _, t, c, a);
  }
}

object isLessThan {

  def distFromData(x: java.lang.Class[_]): Int = {
    var xClass = x
    var xCnt = 0
    while(xClass.toString != "class Chisel.Data") {
      xClass = xClass.getSuperclass
      xCnt += 1
    }
    xCnt
  }

  def checkCommonSuperclass(x: java.lang.Class[_], y: java.lang.Class[_]) {
  }

  def apply(x: java.lang.Class[_], y: java.lang.Class[_]): Boolean = {
    checkCommonSuperclass(x, y)
    distFromData(x) > distFromData(y)
  }
}

object Mux {
  def apply[T<:Data](cond: Bool, tc: T, fc: T): T = {
    // TODO: Replace this runtime check with compiletime check using type classes and imports to add special cases
    val target = if(tc.getClass.isAssignableFrom(fc.getClass)) tc.cloneType else
                 if(fc.getClass.isAssignableFrom(tc.getClass)) fc.cloneType else
                 if(classOf[Bits].isAssignableFrom(tc.getClass) && classOf[Bits].isAssignableFrom(fc.getClass)) {
                   ChiselError.warning("Mux of Bits instantiated, emits SInt")
                   SInt().asInstanceOf[T]
                 } else
                   throw new Exception(s"For Mux, tc(${tc.getClass}) or fc(${fc.getClass}) must directly descend from the other. (Or both descend from Bits)")
    Mux[T,T,T](target, cond, tc, fc)
  }

  // THIS IS THE MAIN MUX CONSTRUCTOR
  def apply[RT<:Data,TT<:Data,FT<:Data](result: RT, cond: Bool, tc: TT, fc: FT)(
   implicit evi_tc: TT <:< RT, evi_fc: FT <:< RT): RT = {
    // The implicit lines require evidence that TT and RT (the mux inputs) are subtypes of the return.
    //   This is preferable over [TT<:RT,FT<:RT] as now the scala compiler infer RT as the actual
    //   type of result (and not the supertype common to result, tc, and fc, which could be none
    //   of them!).

    // TODO CONSIDER: Should this be private?

    // TODO: consider reworking to not use flatten so that can Mux between Vecs of different lengths
    //       or a Bundle and a descendant of that Bundle which adds fields
    //       Will likely require creation of a createMux function

    require(tc.flatten.length == fc.flatten.length, "In Mux (of ${ancestor.getClass}), tc and fc too structurally different. Possibly due to non-determinism or mutability in a subtype of Aggregate e.g. a Bundle refinement adding new fields, or two Vecs of differing lengths.")

    val opdescs = result.flatten.zip(tc.flatten.zip(fc.flatten))
      // opdescs: (result, (tc, fc))

    opdescs.foreach(opdesc => {
      val op_r = opdesc._1._2; val op_tc = opdesc._2._1._2; val op_fc = opdesc._2._2._2
      op_r.asTypeFor(Multiplex(cond, op_tc, op_fc))
    })

    result
  }
}

class Mux extends Op {
  val op = "Mux"
  override def toString: String =
    inputs(0) + " ? " + inputs(1) + " : " + inputs(2)
  def ::(a: Node): Mux = { inputs(2) = a; this }

  override def forceMatchingWidths {
    if (inputs(1).widthW != widthW) inputs(1) = inputs(1).matchWidth(widthW)
    if (inputs(2).widthW != widthW) inputs(2) = inputs(2).matchWidth(widthW)
  }

  override def W0Wtransform() {
    val w1 = inputs(1).widthW
    val w2 = inputs(2).widthW
    if (w1.isKnown && w2.isKnown && w1.needWidth == 0 && w2.needWidth == 0) {
      // If both our inputs are zero-width nodes, so are we.
      setWidth(0)
      inputs.remove(1, 2) /* remove children 1 & 2 */
      modified = true
      // We assume higher level nodes will eventually remove us.
    } else {
      // Convert any zero-width children into UInt(0).
      // This has the side-effect that if we have a zero-width selector, we'll return the "false" input.
      for (c <- inputs if c.needWidth == 0) {
        c.replaceTree(UInt(0, 0))
        modified = true
      }
    }
  }

  override def review() {
    val w = widthW
    // Are we zero-width?
    if (w.isKnown && w.needWidth == 0) {
      /* Replace us with a zero-width constant. */
      replaceTree(UInt(0,0))
    } else {
      val w0 = inputs(0).widthW
      if (w0.isKnown && w0.needWidth == 0) {
        /* Our selector is zero-width. Replace us with the "false" input. */
        replaceTree(inputs(2))
      }
    }
  }
}
