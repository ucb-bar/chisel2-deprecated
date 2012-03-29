package Chisel {

object Terminator {
  val terminator = new Terminator();
}
class Terminator extends Node {
  override def isTerminator = true;
}

}
