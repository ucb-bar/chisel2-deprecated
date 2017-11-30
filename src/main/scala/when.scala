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

/** An object to create conditional logic
  * See [[Chisel.when when]] for functions elsewhen and otherwise
  * @example
  * {{{
  * when ( myData === UInt(3) ) {
  *   ... // Some logic
  * } .elsewhen ( myData === UInt(1) ) {
  *   ... // Some logic
  * } .otherwise {
  *   ... // Some logic
  * } }}}
  */
object when {
  // TODO: make private[Chisel]?
  /** Execute a when block - internal do not use */
  def execWhen(cond: Bool)(block: => Unit) {
    Module.current.whenConds.push(Module.current.whenCond && cond)
    block
    Module.current.whenConds.pop()
  }
  /** @param cond condition to execute upon
    * @param block a section of logic to enable if cond is true */
  def apply(cond: Bool)(block: => Unit): when = {
    execWhen(cond){ block }
    new when(cond)
  }
}

/** A class representing the when block
  * Use [[Chisel.when$ when]] rather than this class directly
  */
class when (prevCond: Bool) {
  /** execute block when alternative cond is true */
  def elsewhen (cond: Bool)(block: => Unit): when = {
    when.execWhen(!prevCond && cond){ block }
    new when(prevCond || cond);
  }
  /** execute block by default */
  def otherwise (block: => Unit) {
    val cond = !prevCond
    cond.canBeUsedAsDefault = !Module.current.hasWhenCond
    when.execWhen(cond){ block }
  }
}

/** This is identical to [[Chisel.when when]] with the condition inverted */
object unless {
  def apply(c: Bool)(block: => Unit) {
    when (!c) { block }
  }
}

/** Conditional logic to form a switch block
  * @example
  * {{{ ... // default values here
  * switch ( myState ) {
  *   is( state1 ) {
  *     ... // some logic here
  *   }
  *   is( state2 ) {
  *     ... // some logic here
  *   }
  * } }}}*/
object switch {
  def apply(c: Bits)(block: => Unit) {
    Module.current.switchKeys.push(c)
    block
    Module.current.switchKeys.pop()
  }
}

/** An object for separate cases in [[Chisel.switch switch]]
  * It is equivalent to a [[Chisel.when$ when]] block comparing to the condition
  * Use outside of a switch statement is illegal */
object is {
  def apply(v: BitPat)(block: => Unit): Unit =
    when (v === switchCond) { block }
  def apply(v: Bits)(block: => Unit): Unit =
    apply(Seq(v))(block)
  def apply(v: Bits, vr: Bits*)(block: => Unit): Unit =
    apply(v :: vr.toList)(block)
  def apply(v: Iterable[Bits])(block: => Unit): Unit =
    when (v.map(_ === switchCond).fold(Bool(false))(_||_)) { block }

  private def switchCond = {
    if (Module.current.switchKeys.isEmpty) {
      ChiselError.error("The 'is' keyword may not be used outside of a switch.")
      Bits(0)
    } else Module.current.switchKeys.top
  }
}
