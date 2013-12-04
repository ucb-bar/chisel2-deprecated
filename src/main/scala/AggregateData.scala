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

/** Base class for a logical set of references to ``Node``s.

  Each node can be individually accessed by a key of type Key,
  which is a String for ``Bundle``s (name) and an Int for ``Vec`` (index).

  This class implements the *Composite* role
  in a [Composite pattern](http://en.wikipedia.org/wiki/Composite_pattern).
  */
abstract class AggregateData[Key] extends Data {

  def items(): Seq[(Key, Data)]

  def nodes(): Seq[Node] = {
    this.flatten.map(x => x._2.node)
  }

  override def asDirectionless(): this.type = {
    items().foreach(_._2.asDirectionless)
    this
  }

  override def asInput(): this.type = {
    items().foreach(_._2.asInput)
    this
  }

  override def asOutput(): this.type = {
    items().foreach(_._2.asOutput)
    this
  }

  override def flip(): this.type = {
    items().foreach(_._2.flip())
    this
  }

  def getWidth(): Int = {
    var w = 0
    for((key, dat) <- items())
      w += dat.getWidth
    w
  }

  override def toBits(): UInt = {
    Cat(this.flatten.map(x => x._2))
  }

  override def toString: String = {
    var sep = ""
    val str = new StringBuilder
    str.append("{")
    for( (key, value) <- items ) {
      str.append(sep + key + ":" + value)
      sep = ", "
    }
    str.append("}")
    str.toString
  }

}
