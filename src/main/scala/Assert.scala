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
import Node._
import ChiselError._

class Assert(condArg: Bool, val message: String) extends Node {
  inputs += condArg;
  def cond: Node = inputs(0);
}

class BitsInObject(x: Node) extends Bits {
  inputs += x
  override def isInObject = true
}

class PrintfBase(formatIn: String, argsIn: Seq[Node]) extends Node {
  inputs ++= argsIn.map(a => new BitsInObject(a))
  def args = inputs
  override def isInObject = true

  private var formats = ""
  private val lengths = new HashMap[Char, (Int => Int)]
  lengths += ('b' -> ((x: Int) => x))
  lengths += ('d' -> ((x: Int) => math.ceil(math.log(2)/math.log(10)*x).toInt))
  lengths += ('x' -> ((x: Int) => (x+3)/4))
  lengths += ('s' -> ((x: Int) => (x+7)/8))
  lengths += ('%' -> ((x: Int) => 1))
  
  private def remap(c: Char) = if (c == 'x') 'h' else c

  val format = {
    var msg = ""
    var percent = false
    for (c <- formatIn) {
      if (percent) {
        if (!lengths.contains(c))
          ChiselError.error("Bad sprintf format: \"%" + c + "\"")
        formats += c
        msg += remap(c)
        percent = false
      } else {
        msg += c
        percent = c == '%'
      }
    }
    if (percent)
      ChiselError.error("Bad sprintf format: trailing %")
    if (formats.length != argsIn.size)
      ChiselError.error("Wrong number of sprintf arguments (found " + argsIn.size + ", expected " + formats.length + ")")

    msg
  }

  inferWidth = (x: Node) => {
    val argLength = formats.zip(inputs).map{case (a,b) => lengths(a)(b.width)}.sum
    8*(format.length - 2*formats.length + argLength)
  }
}

class Sprintf(formatIn: String, argsIn: Seq[Node]) extends PrintfBase(formatIn, argsIn)

class Printf(condIn: Bool, formatIn: String, argsIn: Seq[Node]) extends PrintfBase(formatIn, argsIn) {
  inputs += condIn
  override def args = inputs.init
  def cond = inputs.last
}
