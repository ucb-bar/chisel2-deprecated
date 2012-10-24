package Chisel
import Component._
import Fill._
import Component._
import Lit._

object Fill {
  def fillWidthOf(i: Int, n: Node) = { (m: Node) => (m.inputs(i).width * n.maxNum.toInt) }
  def apply(n: Int, mod: Bits): Bits = {
    val (bits_lit) = (mod.litOf);
    if (n == 1) {
      mod
    } else if (isFolding && bits_lit != null) {
      var res = bits_lit.value;
      val w   = mod.getWidth();
      for (i <- 0 until n-1) 
        res = (res << w)|bits_lit.value;
      Lit(res, n * w){ Bits() };
    } else if (backend.isInstanceOf[CppBackend] && mod.width != 1) {
      var out: Bits = null
      var i = 0
      var cur = mod
      while ((1 << i) <= n) {
        if ((n & (1 << i)) != 0)
          out = Cat(cur, out)
        cur = Cat(cur, cur)
        i = i+1
      }
      out
    } else {
      val fill = new Fill()
      val fillConst = UFix(n)
      fill.init("", fillWidthOf(0, fillConst), mod, fillConst)
      fill.setTypeNode(Bits(OUTPUT))
    }
  }
  def apply(mod: Bits, n: Int): Bits = apply(n, mod)
  def apply(n: Bits, mod: Bits): Bits = {
    (mod << n).toUFix - UFix(1)
  }
}

object NodeFill {
  def apply(n: Int, mod: Node): Node = {
    if (isFolding && mod.litOf != null) {
      var c = BigInt(0)
      val w = mod.litOf.width
      val a = mod.litOf.value
      for (i <- 0 until n)
        c = (c << w) | a
      return Literal(c,n*w)
    }

    val res = new Fill()
    res.init("", (m: Node) => {m.inputs(0).width * n}, mod, Literal(n))
    res
  }
  def apply(mod: Node, n: Int): Node = apply(n, mod)
}

class Fill extends Node {
  var n: Node = if(inputs.length >= 2) inputs(1) else null;
  override def toString: String = "FILL(" + inputs(0) + ", " + n + ")";
}
