package AutoPipe

import Chisel._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import Direction._

abstract class AutoNode {
  var name = ""
  var delay = 0.0
  var isSource = false
  var isSink = false
  var isDecoupledIO = false
  var isSeqReadPort = false
  val stages = new ArrayBuffer[Int]
  var inputStage = -1
  var outputStage = -1
  var propagatedTo = false
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
        inputStage = annotatedStages(node)
        isUserAnnotated = true
      } else {
        Predef.assert(annotatedStages(node) == stages(0), "all chiselNodes in the AutoNode must be annotated with same stage")
      }

      if(isSeqReadPort){
        outputStage = annotatedStages(node) + 1
      } else {
        outputStage = annotatedStages(node)
      }
      propagatedTo = true
    }
  }
  def findStageAuto(node: Node, annotatedStages: HashMap[Node, Int]) = {
    if(annotatedStages.contains(node)){
      if(stages.isEmpty){
        stages += annotatedStages(node)
        inputStage = annotatedStages(node)
        isAutoAnnotated = true
      }
      if(isSeqReadPort){
        outputStage = annotatedStages(node) + 1
      } else {
        outputStage = annotatedStages(node)
      }
      propagatedTo = true
    }
  }
  
  //direction == FORWARD means we are propagating a stage to this node from one of its inputs; direction == BACKWARD means we are propagating a stage to this node from one its consumers
  def propagateStage(stage: Int, direction: Direction) = {
    if(isSeqReadPort){
      Predef.assert(propagatedTo == false)
      if(direction == FORWARD){
        inputStage = stage
        outputStage = stage + 1
      } else {
        outputStage = stage
        inputStage = stage - 1
      }
    } else {
      if(direction == FORWARD){
        inputStage = stage
        if(!propagatedTo){
          outputStage = stage
        }
      } else {
        outputStage = stage
        if(!propagatedTo){
          inputStage = stage
        }
      }
    }
    propagatedTo = true
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
