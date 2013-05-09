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
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import Node._
import ChiselError._

class Assert(condArg: Bool, val message: String) extends Node {
  inputs += condArg;
  def cond: Node = inputs(0);
}

class Printf(condIn: Bool, msgIn: String, argsIn: Seq[Node]) extends Node {
  inputs += condIn
  inputs ++= argsIn

  def cond = inputs.head
  def args = inputs.tail

  val message = {
    def bad(c: String) =
      ChiselErrors += ChiselError("Bad printf format: \"%" + c + "\"", Thread.currentThread().getStackTrace)
    var msg = ""
    var n = 0
    var percent = false
    for (c <- msgIn) {
      if (percent) {
        if (!List('b', 'd', 's', 'x', '%').contains(c))
          bad(c.toString)
        msg += (if (c == 'x') 'h' else c)
        n = n+1
        percent = false
      } else {
        msg += c
        percent = c == '%'
      }
    }
    if (percent)
      bad("")
    if (n != argsIn.size)
      ChiselErrors += ChiselError("Wrong number of printf arguments (found " + argsIn.size + ", expected " + n + ")", Thread.currentThread().getStackTrace)
    msg
  }
}
