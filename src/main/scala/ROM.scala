/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

package Chisel
import ChiselError._
import Node._
import scala.collection.SortedMap

object ROM {
  def apply[T <: Data](elt0: T, elts: T*): ROM[T] = apply(elt0 +: elts.toSeq)
  def apply[T <: Data](elts: Iterable[T]): ROM[T] = apply(elts.toSeq.zipWithIndex.map { case(z,i) => i -> z })
  def apply[T <: Data](elt0: (Int, T), elts: (Int, T)*): ROM[T] = apply(elt0 +: elts.toSeq)
  def apply[T <: Data](elts: Seq[(Int, T)], n: Option[Int]): ROM[T] = new ROM(SortedMap(elts:_*), n)
  def apply[T <: Data](elts: Seq[(Int, T)], n: Int): ROM[T] = apply(elts, Some(n))
  def apply[T <: Data](elts: Seq[(Int, T)]): ROM[T] = apply(elts, None)
  def apply[T <: Data](elts: Array[(Int, T)]): ROM[T] = apply(elts.toSeq, None)
  def apply[T <: Data](elts: Array[(Int, T)], n: Int): ROM[T] = apply(elts.toSeq, Some(n))
}

class ROM[T <: Data](elts: SortedMap[Int, T], lengthIn: Option[Int] = None) extends Vec[T](i => elts.head._2.clone) {
  override val length: Int = lengthIn match {
    case Some(x) => require(x > elts.keySet.max); x
    case None => elts.keySet.max + 1
  }
  private lazy val data = new ROMData(elts, length)

  override def read(addr: UInt): T = {
    val res = gen(0)
    val port = new ROMRead().init("", widthOf(1), addr, data)
    res assign port
    res.setIsTypeNode
    res
  }

  override def write(addr: UInt, data: T): Unit =
    ChiselError.error("Can't write to ROM")
}

class ROMData(elts: SortedMap[Int, Node], val n: Int) extends Node {
  val w = elts.values.map(_.litOf.needWidth()).max
  val sparseLits = {
    inferWidth = fixWidth(w)
    elts.mapValues(_.matchWidth(Width(w)).litOf)
  }
  val lits = {
    val dc = UInt.DC(w).litOf
    Array.tabulate(n)(i => sparseLits.getOrElse(i, dc))
  }

  override lazy val isInObject: Boolean = true
}

class ROMRead extends Node {
  def inputsTailMaxWidth: (=> Node) => Width = { (m) => {
    m.inputs.map(_.width).tail.max
  }}
  inferWidth = inputsTailMaxWidth
  def addr: Node = inputs(0)
  def rom: ROMData = inputs(1).asInstanceOf[ROMData]
  override def toString: String = rom + "[" + addr + "]"

  override def forceMatchingWidths: Unit =
    inputs(0) = addr.matchWidth(Width(log2Up(rom.n)))
}
