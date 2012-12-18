package Chisel
import Node._
import Component._
import scala.math._

object MuxLookup {
  def apply[S <: Bits, T <: Data] (key: S, default: T, mapping: Seq[(S, T)]): T = {
    var res = default;
    for ((k, v) <- mapping.reverse)
      res = Mux(key === k, v, res);
    res
  }

}

object MuxCase {
  def apply[T <: Data] (default: T, mapping: Seq[(Bool, T)]): T = {
    var res = default;
    for ((t, v) <- mapping.reverse){
      res = Mux(t, v, res);
    }
    res
  }
}

object Multiplex{
  def apply (t: Node, c: Node, a: Node): Node = {
    if (isFolding) {
      if (t.litOf != null) 
        return if (t.litOf.value == 0) a else c
      if (c.litOf != null && a.litOf != null) {
        if (c.litOf.value == a.litOf.value)
          return c
        if (c.litOf.value == 1 && a.litOf.value == 0){
          if(c.litOf.width == 1 && a.litOf.width == 1) return t
          val fill = NodeFill(max(c.litOf.width-1, a.litOf.width-1), Literal(0,1))
          fill.infer
          val bit = NodeExtract(t, 0)
          bit.infer
          val cat = Concatenate(fill, bit)
          cat.infer
          return cat
        }
      }
      if (a.isInstanceOf[Mux] && c.clearlyEquals(a.inputs(1)))
        // Multiplex(t.asInstanceOf[Bits] || a.inputs(0).asInstanceOf[Bits], c, a.inputs(2))
        Multiplex(Op("||", 1, t, a.inputs(0)), c, a.inputs(2))
    }
    new Mux().init("", maxWidth _, t, c, a);
  }
}

object Mask{
  def apply (t: Node, c: Node): Node = Multiplex(t, c, Literal(0))
}

object isLessThan {

  def distFromData(x: java.lang.Class[ _ ]) = {
    var xClass = x
    var xCnt = 0
    while(xClass.toString != "class Chisel.Data") {
      xClass = xClass.getSuperclass
      xCnt += 1
    }
    xCnt
  }

  def checkCommonSuperclass(x: java.lang.Class[ _ ], y: java.lang.Class[ _ ]) = {
    
  }

  def apply(x: java.lang.Class[ _ ], y: java.lang.Class[ _ ]) = {
    checkCommonSuperclass(x, y)
    distFromData(x) > distFromData(y)
  }
}

object Mux {
  def apply[T <: Data](t: Bits, c: T, a: T): T = {
    val res = Multiplex(t, c.toNode, a.toNode)
    if (c.isInstanceOf[Bits]) {
      assert(a.isInstanceOf[Bits])
      if (c.getClass == a.getClass)
        res.setTypeNodeNoAssign(c.fromNode(res).asInstanceOf[T])
      else {
        res.setTypeNode(Bits(OUTPUT)).asInstanceOf[T]
      }
    } else {
      res.setTypeNodeNoAssign(c.fromNode(res).asInstanceOf[T])
    }
  }
}
class Mux extends Op {
  muxes += this;
  stack = Thread.currentThread.getStackTrace;
  op = "Mux";
  override def toString: String =
    inputs(0) + " ? " + inputs(1) + " : " + inputs(2)
  def ::(a: Node): Mux = { inputs(2) = a; this }

  override def forceMatchingWidths = {
    if (inputs(1).width != width) inputs(1) = inputs(1).matchWidth(width)
    if (inputs(2).width != width) inputs(2) = inputs(2).matchWidth(width)
  }

  override def genSubNodes = {
    // println("MUX0 " + this + " " + subnodes.length + " -> " + backend.words(this))
    val (t, c, a) = (inputs(0), inputs(1), inputs(2))
    for (i <- 0 until backend.words(this)) 
      setSubNode(i, new Mux())
    for (i <- 0 until backend.words(this)) 
      subnodes(i).init("", backend.thisWordBits(this, i), t.getSubNode(0), c.getSubNode(i), a.getSubNode(i))
    // println("MUX1 " + this + " " + subnodes.length)
  }
}
