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

import Literal._
import scala.collection.mutable.ArrayBuffer

/** Dataflow switch statement. default list and mapping list
  must be of equal length. */
object Lookup {

  def apply[T <: Data](addr: UInt, default: T,
    mapping: Seq[(UInt, T)])(implicit m: reflect.ClassTag[T]): T = {
    val res = m.runtimeClass.newInstance.asInstanceOf[T]
    res.fromBits(UInt(new Lookup(addr.lvalue(),
      default.toBits.lvalue(),
      mapping.map(x => (x._1.lvalue(), x._2.toBits.lvalue())))))
    res
  }
}

class Lookup(addrN: Node, default: Node,
    val map: Seq[(Node, Node)]) extends Node {

  inferWidth = new WidthOf(0)

  inputs.append(addrN)
  inputs.append(default)
  map.map(x => { inputs.append(x._1); inputs.append(x._2) })

  def addr: Node = inputs(0)
  def wires: List[Node] = map.map(x => x._1).toList
  def defaultWires: List[Node] = map.map(x => x._2).toList
}
