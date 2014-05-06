package Chisel

object when {
  def execWhen(cond: Bool)(block: => Unit) {
    Module.current.whenConds.push(Module.current.whenCond && cond)
    block
    Module.current.whenConds.pop()
  }
  def apply(cond: Bool)(block: => Unit): when = {
    execWhen(cond){ block }
    new when(cond)
  }
}

class when (prevCond: Bool) {
  def elsewhen (cond: Bool)(block: => Unit): when = {
    when.execWhen(!prevCond && cond){ block }
    new when(prevCond || cond);
  }
  def otherwise (block: => Unit) {
    val cond = !prevCond
    cond.canBeUsedAsDefault = !Module.current.hasWhenCond
    when.execWhen(cond){ block }
  }
}

object unless {
  def apply(c: Bool)(block: => Unit) {
    when (!c) { block }
  }
}

object switch {
  def apply(c: Bits)(block: => Unit) {
    Module.current.switchKeys.push(c)
    block
    Module.current.switchKeys.pop()
  }
}

object is {
  def apply(v: Bits)(block: => Unit): Unit =
    apply(Seq(v))(block)
  def apply(v: Bits, vr: Bits*)(block: => Unit): Unit =
    apply(v :: vr.toList)(block)
  def apply(v: Iterable[Bits])(block: => Unit): Unit = {
    val keys = Module.current.switchKeys
    if (keys.isEmpty) ChiselError.error("The 'is' keyword may not be used outside of a switch.")
    else if (!v.isEmpty) when (v.map(_ === keys.top).reduce(_||_)) { block }
  }
}
