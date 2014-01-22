package AutoPipe

import Chisel._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

abstract class AutoNode {
  var name = ""
  var delay = 0.0
  var isSource = false
  var isSink = false
  val stages = new ArrayBuffer[Int]
  var isUserAnnotated = false
  val inputs = new ArrayBuffer[AutoNode]
  val consumers = new ArrayBuffer[AutoNode]
  override def toString() : String = {
    return name
  }
}

class AutoLogic extends AutoNode {
  val inputChiselNodes = new ArrayBuffer[Node]
  val inputMap = new HashMap[AutoNode, (Node, Int)]
  val outputChiselNodes = new ArrayBuffer[Node]
}

class AutoWire extends AutoNode {
  var consumerChiselNode:Node = null
  var consumerChiselNodeInputNum = -1
}
