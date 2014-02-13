package Chisel

import scala.collection.mutable.ArrayBuffer

trait ClockEdge
object PosEdge extends ClockEdge
object NegEdge extends ClockEdge

class Clock(reset: Bool = Module.implicitReset, val edge: ClockEdge = PosEdge) extends Node {
  val stateElms = new ArrayBuffer[Node]
  Module.clocks += this
  init("", 1)

  // var srcClock: Clock = null
  var srcClock: Node = null // by Donggyu
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

  // for negative edge 
  def unary_- = {
    val clock = new Clock(reset, NegEdge)
    clock.init(name, 1)
    clock.srcClock = this
    clock
  } 
}
