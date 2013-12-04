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

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

/** Create an initialized Read-Only Memory.
  */
object ROM {
  def apply[T <: Data](lits: Seq[T]): ROM[T] = {
    new ROM[T](() => lits.head,
      lits.map(x => x.toBits.node.asInstanceOf[Literal]),
      Module.scope.clock,
      Module.scope.reset)
  }
}


class ROM[T <: Data](gen: () => T, val lits: Seq[Literal],
  clock: Clock, reset: Bool,
  isInline: Boolean = Module.isInlineMem) extends Mem[T](
  gen, clock, reset, lits.length, false, isInline) {

  override val node = new ROMemDelay(lits, clock.node.asInstanceOf[Update],
    reset.node, lits.length, isInline)

  override def write(addr: UInt, data: T) {
    ChiselError.error("Can't write to ROM")
  }
}
