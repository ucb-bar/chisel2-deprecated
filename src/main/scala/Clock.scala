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

import scala.collection.mutable.ArrayBuffer

object Clock {

  /** Creates a new ``Clock`` instance.

    By default the reset signal at the current scope will be associated
    to the new clock domain.
    */
  def apply(reset: Bool = Module.scope.reset): Clock = {
    new Clock(reset)
  }
}


class Clock(reset: Bool, clk: Update = new Update()) extends Bool {

  node = clk

  // returns a reset pin connected to reset for the component in scope
  def getReset: Bool = reset

  def srcClock: Clock = {
    new Clock(null, node.asInstanceOf[Update].src)
  }

  /** Creates a new ``Clock`` with a period which is a multiple
    of this clock.
    */
  def * (right: Int): Clock = {
    new Clock(reset, new Update(this.node.asInstanceOf[Update], right, 1))
  }

  /** Creates a new ``Clock`` with a period which is a divider
    of this clock.
    */
  def / (right: Int): Clock = {
    new Clock(reset, new Update(this.node.asInstanceOf[Update], 1, right))
  }

}
