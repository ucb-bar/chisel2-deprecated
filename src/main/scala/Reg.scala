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

import Reg._
import ChiselError._
import scala.reflect._

object Reg {

  /** *type_out* defines the data type of the register when it is read.
    *update* and *reset* define the update and reset values
    respectively.
    */
  def apply[T <: Data](outType: T = null, next: T = null, init: T = null,
    clock: Clock = Module.scope.clock, reset: Bool = Module.scope.reset): T = {
    val res: T = if( outType != null ) outType.clone
    else if( next != null ) next.clone
    else if( init != null ) init.clone
    else null.asInstanceOf[T]
    if( res == null ) {
      ChiselError.error("Cannot infer type of Reg")
    }
    if( next != null ) {
      if( init != null ) {
        for((((resIdx, resB), (dataIdx, nextB)), (rvalIdx, initB))
          <- res.flatten zip next.flatten zip init.flatten) {
          val reg = new RegDelay(
            clock.node.asInstanceOf[Update],
            nextB.lvalue(),
            initB.lvalue(),
            reset.lvalue())
          resB.node = reg
        }
      } else {
        for(((resIdx, resB), (dataIdx, nextB))
          <- res.flatten zip next.flatten) {
          val reg = new RegDelay(
            clock.node.asInstanceOf[Update],
            nextB.lvalue(),
            null,
            reset.lvalue())
          resB.node = reg
        }
      }
    } else {
      /* next is null */
      if( init != null ) {
        for(((resIdx, resB), (rvalIdx, initB))
          <- res.flatten zip init.flatten) {
          val reg = new RegDelay(
            clock.node.asInstanceOf[Update],
            null,
            initB.lvalue(),
            reset.lvalue())
          resB.node = reg
          resB.node.inferWidth = new WidthOf(2)
        }
      } else {
        /* both next and init are null */
        for((resIdx, resB) <- res.flatten) {
          val reg = new RegDelay(
            clock.node.asInstanceOf[Update],
            null,
            null,
            reset.lvalue())
          resB.node = reg
        }
      }
    }

    /* override width inference if it was explicitely specified. */
    if( outType != null ) {
      for(((resIdx, resB), (outIdx, outN)) <- res.flatten zip outType.flatten) {
        if( outN.node.inferWidth.isInstanceOf[FixedWidth] ) {
          resB.node.inferWidth = outN.node.inferWidth
        }
      }
    }

    res
  }

  /* Without this method, the scala compiler is not happy
   when we declare registers as Reg(signal). */
  def apply[T <: Data](outType: T): T = Reg[T](outType, null.asInstanceOf[T], null.asInstanceOf[T])
}


object RegNext {

  def apply[T <: Data](next: T): T = Reg[T](next, next, null.asInstanceOf[T])

  def apply[T <: Data](next: T, init: T): T = Reg[T](next, next, init)

}

object RegInit {

  def apply[T <: Data](init: T): T = Reg[T](init, null.asInstanceOf[T], init)

}

