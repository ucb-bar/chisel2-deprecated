package Chisel

import scala.collection.mutable.ArrayBuffer

class Clock(reset: Bool = Module.implicitReset) extends Node {
  val stateElms = new ArrayBuffer[Node]
  Module.clocks += this
  init("", 1)

  var srcClock: Clock = null
  var initStr = ""

  // returns a reset pin connected to reset for the component in scope
  def getReset: Bool = {
    if (Module.compStack.length != 0) {
      Module.compStack.top.getResetPin(reset)
    } else {
      reset
    }
  }

  def * (x: Int): Clock = {
    val clock = new Clock(reset)
    clock.init("", 1)
    clock.srcClock = this
    clock.initStr = " * " + x + ";\n"
    clock
  }

  def / (x: Int): Clock = {
    val clock = new Clock(reset)
    clock.init("", 1)
    clock.srcClock = this
    clock.initStr = " / " + x + ";\n"
    clock
  }
}
