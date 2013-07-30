package Chisel

import scala.collection.mutable.ArrayBuffer

class Clock(reset: Bool = Module.implicitReset) extends Node {
  val stateElms = new ArrayBuffer[Node]
  Module.clocks += this
  init("", 1)

  def getReset: Bool = {
    if (Module.compStack.length != 0) {
      val curComp = Module.compStack.top
      if (!curComp.resets.contains(reset))
        curComp.resets += (reset -> Bool(INPUT))
      curComp.resets(reset)
    } else {
      reset
    }
  }
}
