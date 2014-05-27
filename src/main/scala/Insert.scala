package Chisel

class Insert(tgt: Bits, bit: UInt, length: Int) extends proc {
  override def procAssign(src: Node): Unit = {
    if (tgt.next == null) {
      ChiselError.error("Subword assignment requires a default value to have been assigned")
    } else {
      val mask = UInt((BigInt(1) << length) - 1, length)
      val shiftedMask = mask << bit
      val fill =
        if (length == 1) src.asInstanceOf[Bits].toBool.toSInt & shiftedMask
        else (src.asInstanceOf[Bits] & mask) << bit
      tgt := UInt(tgt.next) & ~shiftedMask | fill
    }
  }
}
