package Chisel

object Log2 {
  def apply(x: Bits): UInt = UInt().asTypeFor(new Log2(x))
  def apply(x: Bits, n: Int): UInt = apply(x(n-1,0))
}

/** Returns the bit position of the trailing 1 in the input vector
  with the assumption that multiple bits of the input bit vector can be set
  */
object PriorityEncoder
{
  def apply(in: Iterable[Bool]): UInt = PriorityMux(in, (0 until in.size).map(UInt(_)))
  def apply(in: Bits): UInt = UInt().asTypeFor(new PriorityEncoder(in))
}

/** Does the inverse of UIntToOH.
  */
object OHToUInt
{
  def apply(in: Seq[Bool]): UInt = apply(Vec(in))
  def apply(in: Vec[Bool]): UInt = apply(in.toBits)
  def apply(in: Bits): UInt = UInt().asTypeFor(new OHToUInt(in))
}

abstract class Log2Like(x: Bits, name: String) extends Op(name) {
  inputs += x
  inferWidth = (x: Node) => log2Up(Node.widthOf(0)(x))
}

class Log2(x: Bits) extends Log2Like(x, "Log2") {
  override def lower: Node = {
    val range = inputs(0).width-1 to 0 by -1
    val in = UInt(inputs(0))
    PriorityMux(range.map(in(_)), range.map(UInt(_)))
  }
}

class PriorityEncoder(x: Bits) extends Log2Like(x, "PriEnc") {
  override def lower: Node = {
    PriorityMux(UInt(inputs(0)), (0 until inputs(0).width).map(UInt(_)))
  }
}

class OHToUInt(x: Bits) extends Log2Like(x, "OHToUInt") {
  override def lower: Node = {
    def doLower(x: Node, length: Int): Node = {
      if (length <= 1) UInt(0,1)
      else if (length == 2) NodeExtract(x, 1)
      else {
        val half = 1 << (log2Up(length)-1)
        val hi = NodeExtract(x, length-1, half)
        val lo = NodeExtract(x, half-1, 0)
        Concatenate(LogicalOp(hi, Literal(0, length-half), "!="), doLower(BinaryOp(hi, lo, "|"), half))
      }
    }
    doLower(inputs(0), inputs(0).width)
  }
}
