package Chisel
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import Node._
import ChiselError._

object Assert {
  def apply(cond: Bool, message: String) = 
    new Assert(cond, message)
}


class Assert(condArg: Bool, val message: String) extends Node {
  inputs += condArg;
  def cond = inputs(0);
}
