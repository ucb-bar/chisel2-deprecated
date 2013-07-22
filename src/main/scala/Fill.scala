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
  def fillWidthOf(i: Int, n: Node): (Node) => (Int) = { (m: Node) => (m.inputs(i).width * n.maxNum.toInt) }
  def apply(n: Int, mod: UInt): UInt = {
    val (bits_lit) = (mod.litOf);
    if (n == 1) {
      mod
    } else if (Module.isFolding && bits_lit != null) {
      var res = bits_lit.value;
      val w   = mod.getWidth();
      for (i <- 0 until n-1)
        res = (res << w)|bits_lit.value;
      Lit(res, n * w){ UInt() };
    } else if (Module.backend.isInstanceOf[CppBackend] && mod.width != 1) {
      var out: UInt = null
      var i = 0
      var cur = mod
      while ((1 << i) <= n) {
        if ((n & (1 << i)) != 0) {
          out = Cat(cur, out)
        }
        cur = Cat(cur, cur)
        i = i + 1
      }
      out
    } else {
      val fill = new Fill()
      val fillConst = UInt(n)
      fill.init("", fillWidthOf(0, fillConst), mod, fillConst)
      UInt(OUTPUT).fromNode(fill)
    }
  }
  def apply(mod: UInt, n: Int): UInt = apply(n, mod)
  def apply(n: UInt, mod: UInt): UInt = {
    (mod << n) - UInt(1)
  }
}

object NodeFill {

  def apply(n: Int, mod: Node): Node = {
    if (Module.isFolding && mod.litOf != null) {
      var c = BigInt(0)
      val w = mod.litOf.width
      val a = mod.litOf.value
      for (i <- 0 until n)
        c = (c << w) | a
      Literal(c,n*w)
    } else {
      val res = new Fill()
      res.init("", (m: Node) => {m.inputs(0).width * n}, mod, Literal(n))
      res
    }
  }

  def apply(mod: Node, n: Int): Node = apply(n, mod)
}

class Fill extends Node {
  var n: Node = if(inputs.length >= 2) inputs(1) else null;
  override def toString: String = "FILL(" + inputs(0) + ", " + n + ")";
}
