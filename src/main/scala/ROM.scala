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
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

class ROM[T <: Data](val lits: Seq[Literal], gen: () => T) extends Vec[T](gen) {
  override def read(addr: UFix): T = {
    val data = gen().asOutput
    var port = new ROMRead().init("", fixWidth(gen().getWidth), addr, this)
    data assign port
    data.setIsTypeNode
    data
  }

  override def write(addr: UFix, data: T) = {
    ChiselError.error("Can't write to ROM")
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

class ROMRead[T <: Data]() extends Node {
  def addr = inputs(0)
  def rom = inputs(1)
  // inputs += addri
  // inputs += rom

  override def toString: String = inputs(1) + "[" + inputs(0) + "]"
}
