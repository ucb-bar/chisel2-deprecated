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

class ROM[T <: Data](elts: IndexedSeq[T]) extends Vec[T](i => elts.head.clone) {
  private lazy val data = new ROMData(elts)

  override def read(addr: UInt): T = {
    if (elts.length != self.length) {
      // fall back to Vec.read if the user has appended to us
      super.read(addr)
    } else {
      val res = gen(0)
      val port = new ROMRead().init("", widthOf(1), addr, data)
      res assign port
      res.setIsTypeNode
      res
    }
  }

  override def write(addr: UInt, data: T): Unit =
    ChiselError.error("Can't write to ROM")
}

class ROMData(elts: IndexedSeq[Node]) extends Node {
  val lits = {
    val width = elts.map(_.litOf.width).max
    inferWidth = fixWidth(width)
    elts.map(_.matchWidth(width).litOf)
  }

  override def isInObject: Boolean = true
  override def isInVCD: Boolean = false
}

class ROMRead extends Node {
  inferWidth = (x: Node) => inputs.map(_.width).tail.max
  def addr: Node = inputs(0)
  def rom: ROMData = inputs(1).asInstanceOf[ROMData]
  override def toString: String = rom + "[" + addr + "]"

  override def forceMatchingWidths: Unit =
    inputs(0) = addr.matchWidth(log2Up(rom.lits.length))
}
