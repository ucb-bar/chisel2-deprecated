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

object Fill {
  def apply(n: Int, mod: Bool): UInt = n match {
    case 0 => UInt(width=0)
    case 1 => mod
    case x if n > 1 => UInt(0,n) - mod
    case _ => throw new IllegalArgumentException(s"n (=$n) must be nonnegative integer.")
  }
  def apply(n: Int, mod: UInt): UInt = UInt(NodeFill(n, mod))
  def apply(mod: UInt, n: Int): UInt = apply(n, mod)
}

object NodeFill {
  def apply(n: Int, mod: Node): Node = {
    val w = mod.widthW
    n match {
      case 0 => UInt(width=0)
      case 1 => mod
      case x if n > 1 =>
        if (w.isKnown && w.needWidth == 1) {
          Multiplex(mod, Literal((BigInt(1) << x) - 1, x), Literal(0, x))
        } else {
          /* Build up a Concatenate tree for more ILP in simulation. */
          val p2 = Array.ofDim[Node](log2Up(x+1))
          p2(0) = mod
          for (i <- 1 until p2.length)
            p2(i) = Concatenate(p2(i-1), p2(i-1))
          Concatenate((0 until log2Up(x+1)).filter(i => (x & (1 << i)) != 0).map(p2(_)))
        }
      case _ => throw new IllegalArgumentException(s"n (=$n) must be nonnegative integer.")
    }
  }
}
