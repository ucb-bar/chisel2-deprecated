package Chisel

import Node._
import ChiselError._

abstract class Data extends Node {
  var comp: proc = null;
  def toFix(): Fix = chiselCast(this){Fix()};
  def toUFix(): UFix = chiselCast(this){UFix()};
  def toBits(): Bits = chiselCast(this){Bits()};
  def toBool(): Bool = chiselCast(this){Bool()};

  def setIsTypeNode = {
    assert(inputs.length > 0, {println("Type Node must have an input") })
    isTypeNode = true
    inferWidth = widthOf(0)
  }

  def apply(name: String): Data = null
  def flatten = Array[(String, Bits)]();
  def terminate(): Unit = { }
  def flip(): this.type = this;
  def asInput(): this.type = this;
  def asOutput(): this.type = this;
  def toNode: Node = this;
  def fromNode(n: Node): this.type = this;
  def fromBits(b: Bits): this.type = {
    val n = fromNode(b)
    n.setIsTypeNode
    n
  }
  def :=[T <: Data](data: T) = {
    if(this.getClass != data.getClass) println("Mismatched types: " + this.getClass + " " + data.getClass);
    comp procAssign data.toNode;
  }
  override def clone(): this.type = {
    try {
      val res = this.getClass.newInstance.asInstanceOf[this.type];
      res
    } catch {
      case e: java.lang.Exception => {
        throwException("Parameterized Bundle " + this.getClass + " needs clone method")
        this
      }
    }
  }
  override def name_it(path: String, setNamed: Boolean = false) = {
    if (isTypeNode && comp != null) 
      comp.name_it(path, setNamed)
    else
      super.name_it(path, setNamed);
  }
  def setWidth(w: Int) = this.width_ = w;
}

