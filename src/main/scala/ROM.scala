package Chisel
import ChiselError._
import Node._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

class ROM[T <: Data](val lits: Seq[Literal], gen: () => T) extends Vec[T](gen) {
  override def read(addr: UFix): T = {
    var port = new ROMRead(this, addr)
    val data = gen().asOutput
    data assign port
    data.setIsTypeNode
    data
  }

  override def write(addr: UFix, data: T) = {
    ChiselErrors += ChiselError("Can't write to ROM", Thread.currentThread().getStackTrace)
  }

  override def equals(x: Any): Boolean = {
    if (x.isInstanceOf[ROM[_]]) {
      this.eq(x.asInstanceOf[AnyRef])
    } else {
      super.equals(x)
    }
  }

  override def isReg = true

}

class ROMRead[T <: Data](val rom: ROM[T], addri: Bits) extends Node {
  def addr = inputs(0)
  inputs += addri
  inputs += rom

  override def toString: String = rom + "[" + addr + "]"
}
