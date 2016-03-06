/*
 Copyright (c) 2011 - 2016 The Regents of the University of
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
import scala.collection.SortedMap

/** Read Only Memory object
  * Optionally create in a map with integer addresses attached
  * Create a [[Chisel.Vec Vec]] of literals rather than ROM directly
  */
object ROM {
  /** @param elt0 the first data object at address 0 in the ROM
    * @param elts any number of data objects for the ROM */
  def apply[T <: Data](elt0: T, elts: T*): ROM[T] = apply(elt0 +: elts.toSeq)
  /** @param elts any number of data objects for the ROM */
  def apply[T <: Data](elts: Iterable[T]): ROM[T] = apply(elts.toSeq.zipWithIndex.map { case(z,i) => i -> z })
  /** @param elt0 the first data object at address 0 in the ROM
    * @param elts any number of data objects for the ROM */
  def apply[T <: Data](elt0: (Int, T), elts: (Int, T)*): ROM[T] = apply(elt0 +: elts.toSeq)
  /** @param elts any number of data objects for the ROM
    * @param n optionally force the size of the ROM */
  def apply[T <: Data](elts: Seq[(Int, T)], n: Option[Int]): ROM[T] = new ROM(SortedMap(elts:_*), n)
  /** @param elts any number of data objects for the ROM
    * @param n optionally force the size of the ROM */
  def apply[T <: Data](elts: Seq[(Int, T)], n: Int): ROM[T] = apply(elts, Some(n))
  /** @param elts any number of data objects for the ROM */
  def apply[T <: Data](elts: Seq[(Int, T)]): ROM[T] = apply(elts, None)
  /** @param elts any number of data objects for the ROM */
  def apply[T <: Data](elts: Array[(Int, T)]): ROM[T] = apply(elts.toSeq, None)
  /** @param elts any number of data objects for the ROM
    * @param n optionally force the size of the ROM */
  def apply[T <: Data](elts: Array[(Int, T)], n: Int): ROM[T] = apply(elts.toSeq, Some(n))
}

/** Class defining a ROM
  * Use the [[Chisel.ROM$ ROM]] object rather than instantiating the ROM directly
  * @param elts any number of data elements combined with Integer address
  * @param lengthIn optionally force the size of the ROM */
class ROM[T <: Data](elts: SortedMap[Int, T], lengthIn: Option[Int] = None) extends Vec[T](i => elts.head._2.cloneType, Nil) {
  override val self = elts.unzip._2.toVector
  override val length: Int = lengthIn match {
    case Some(x) => require(x > elts.keySet.max); x
    case None => elts.keySet.max + 1
  }
  private lazy val data = new ROMData(elts, length)

  /** Read data from the ROM at an address */
  override def read(addr: UInt): T = {
    val res = gen(0)
    val port = new ROMRead().init("", Node.widthOf(1), addr, data)
    res assign port
    res.setIsTypeNode
    res
  }

  /** Illegal, cannot write to Read only memory */
  override def write(addr: UInt, data: T): Unit =
    ChiselError.error("Can't write to ROM")
}

/** ROMData stores the data for [[Chisel.ROM ROM]] */
class ROMData(elts: SortedMap[Int, Node], val n: Int) extends Node {
  val w = elts.values.map(_.litOpt.get.needWidth()).max
  val sparseLits = {
    inferWidth = Node.fixWidth(w)
    elts.mapValues(_.matchWidth(Width(w)).litOf)
  }

  override lazy val isInObject: Boolean = true
  override lazy val isInVCD: Boolean = Driver.isVCDMem
}

/** Class to read from ROM - internal, do not use */
class ROMRead extends Node {
  def inputsTailMaxWidth: (=> Node) => Width = { (m) => {
    m.inputs.map(_.widthW).tail.max
  }}
  inferWidth = inputsTailMaxWidth
  def addr: Node = inputs(0)
  def rom: ROMData = inputs(1).asInstanceOf[ROMData]
  override def toString: String = rom + "[" + addr + "]"

  override def forceMatchingWidths: Unit =
    inputs(0) = addr.matchWidth(Width(log2Up(rom.n)))
}
