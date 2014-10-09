/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
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

object when {
  def execWhen(cond: Bool)(block: => Unit) {
    Module.current.whenConds.push(Module.current.whenCond && cond)
    block
    Module.current.whenConds.pop()
  }
  def apply(cond: Bool)(block: => Unit): when = {
    execWhen(cond){ block }
    new when(cond)
  }
}

class when (prevCond: Bool) {
  def elsewhen (cond: Bool)(block: => Unit): when = {
    when.execWhen(!prevCond && cond){ block }
    new when(prevCond || cond);
  }
  def otherwise (block: => Unit) {
    val cond = !prevCond
    cond.canBeUsedAsDefault = !Module.current.hasWhenCond
    when.execWhen(cond){ block }
  }
}

object unless {
  def apply(c: Bool)(block: => Unit) {
    when (!c) { block }
  }
}

object switch {
  def apply(c: Bits)(block: => Unit) {
    Module.current.switchKeys.push(c)
    block
    Module.current.switchKeys.pop()
  }
}

object is {
  def apply(v: Bits)(block: => Unit): Unit =
    apply(Seq(v))(block)
  def apply(v: Bits, vr: Bits*)(block: => Unit): Unit =
    apply(v :: vr.toList)(block)
  def apply(v: Iterable[Bits])(block: => Unit): Unit = {
    val keys = Module.current.switchKeys
    if (keys.isEmpty) ChiselError.error("The 'is' keyword may not be used outside of a switch.")
    else if (!v.isEmpty) when (v.map(_ === keys.top).reduce(_||_)) { block }
  }
}
