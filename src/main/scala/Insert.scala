package Chisel

class Insert(tgt: Bits, bit: UInt) extends proc {
  override def procAssign(src: Node): Unit = {
    if (tgt.next == null) {
      ChiselError.error("Subword assignment requires a default value to have been assigned")
    } else {
      val mask = UInt(1, 1) << bit
      tgt := UInt(tgt.next) & ~mask | src.asInstanceOf[Data].toBool.toSInt & mask
    }
  }
}
