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
import Fill._
import Lit._

object Fill {
  def apply(n: Int, mod: UInt): UInt = UInt(NodeFill(n, mod))
  def apply(mod: UInt, n: Int): UInt = apply(n, mod)
}

object NodeFill {
  def apply(n: Int, mod: Node): Node = {
    if (n == 1) {
      mod
    } else if (mod.width == 1) {
      Multiplex(mod, Literal((BigInt(1) << n) - 1, n), Literal(0, n))
    } else {
      /* Build up a Concatenate tree for more ILP in simulation. */
      var out: Node = null
      var i = 0
      var cur = mod
      while ((1 << i) <= n) {
        if ((n & (1 << i)) != 0) {
          out = Concatenate(cur, out)
        }
        cur = Concatenate(cur, cur)
        i = i + 1
      }
      out
    }
  }
}
