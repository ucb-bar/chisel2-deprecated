package Chisel
import ChiselError._
import Component._
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

  override def genSubNodes: Unit = {
    println("ROM WIDTH " + width)
    for (i <- 0 until backend.words(this)) {
      val w = backend.thisWordBits(this, i)
      val sublits = Range(0, lits.length).map(j => lits(j).getSubNode(i).asInstanceOf[Literal])
      val m = new ROM(sublits, () => Bits(width = w))
      // m.width_ = w // TODO: GET REAL WIDTH
      m.width_ = backend.wordBits
      setSubNode(i, m)
      println("  SUBROM WIDTH " + w)
    }
  }
}

object RawROMRead {
  def apply(mem: Node, addri: Node) = {
    val m  = mem.asInstanceOf[ROM[Bits]]
    val ba = Bits();
    val mr = m.read(ba).getNode
    mr.width_    = m.width
    mr.inputs(0) = addri
    println("MR " + mr)
    mr
  }
}

class ROMRead[T <: Data](val rom: ROM[T], addri: Bits) extends Node {
  def addr = inputs(0)
  inputs += addri
  inputs += rom

  override def toString: String = rom + "[" + addr + "]"

  override def genSubNodes: Unit = {
    for (i <- 0 until backend.words(this)) {
      val m = rom.getSubNode(i)
      val r = RawROMRead(m, addr.getSubNode(0))
      setSubNode(i, r)
    }
    println("ROMREAD SUBNODE " + subnodes(0))
  }
}
