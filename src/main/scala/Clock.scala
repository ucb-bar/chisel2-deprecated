package Chisel

import scala.collection.mutable.ArrayBuffer

trait ClockEdge
object PosEdge extends ClockEdge
object NegEdge extends ClockEdge

class Clock(reset: Bool = Module.implicitReset, val edge: ClockEdge = PosEdge) extends Node {
  val stateElms = new ArrayBuffer[Node]
  Module.clocks += this
  init("", 1)

  // by Donggyu
  var srcClock: Clock = null
  // def srcClock: Node = if (inputs.isEmpty) null else inputs(0)
  var isEnabled = false          
  var initStr = ""
  
  override def isInObject = super.isInObject || isEnabled

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

  // for negative clock edges
  def unary_- = {
    val clock = new Clock(reset, NegEdge)
    clock.init(name, 1)
    clock.srcClock = this
    // clock.inputs += this
    clock
  }

  // for enabled clocks
  // Todo: generalize it!
  def enabledBy (src: Clock, enable: Bool) {
    srcClock = src
    isEnabled = true
    component = enable.getNode.component
    Module.backend match {
      case _: VerilogBackend => {
        val typeNode = Bool()
        typeNode.inputs += src
        inputs += enable && typeNode
      }
      case _ => {
        inputs += enable
      }
    }
    inputs.head.getNode setName name
  }
}
