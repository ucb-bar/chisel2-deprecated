package AutoPipe

import Chisel._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

abstract class AutoNode {
  var name = ""
  var delay = 0.0
  var isSource = false
  var isSink = false
  var isDecoupledIO = false
  val stages = new ArrayBuffer[Int]
  var isUserAnnotated = false
  var isAutoAnnotated = false
  val inputs = new ArrayBuffer[AutoNode]
  val consumers = new ArrayBuffer[AutoNode]
  override def toString() : String = {
    return name
  }
  def findStage(node: Node, annotatedStages: HashMap[Node, Int]) = {
    if(annotatedStages.contains(node)){
      if(stages.isEmpty){
        stages += annotatedStages(node)
        isUserAnnotated = true
      } else {
        Predef.assert(annotatedStages(node) == stages(0), "all chiselNodes in the AutoNode must be annotated with same stage")
      }
    }
  }
  def findStageAuto(node: Node, annotatedStages: HashMap[Node, Int]) = {
    if(annotatedStages.contains(node)){
      if(stages.isEmpty){
        stages += annotatedStages(node)
        isAutoAnnotated = true
      } 
    }
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
