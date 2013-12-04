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


/** Dataflow switch statement. default list and mapping list
  must be of equal length.
  */
object ListLookup {

  def apply[T <: Data](addr: UInt, default: List[T],
    mapping: Seq[(UInt, List[T])])(implicit m: reflect.ClassTag[T]): List[T] = {

    val op = new ListLookup(addr.lvalue(),
      default.map(_.toBits.lvalue()),
      mapping.map(x => (x._1.lvalue(), x._2.map(_.toBits.lvalue()))))

    var res: List[T] = Nil
    for( i <- 0 to default.length ) {
      val lookupRef: T = m.runtimeClass.newInstance.asInstanceOf[T]
      lookupRef.fromBits(UInt(new ListLookupRef(op, default.length - i - 1)))
      res = res.::(lookupRef) // This will actually prepend ref!
    }
    res
  }
}

class ListLookup(addrN: Node, default: List[Node],
    val map: Seq[(Node, List[Node])]) extends Node {

  inferWidth = new WidthOf(1)
  inputs.append(addrN)
  inputs ++= default
  map.map(x => { inputs.append(x._1); x._2.map(inputs.append(_)) })

  def addr: Node = inputs(0)

  def wires: List[Node] = {
    consumers.filter(_.isInstanceOf[ListLookupRef]).toList
  }

  def defaultWires: List[Node] = default
}


class ListLookupRef(listN: Node, index: Int) extends Node {

  inferWidth = new WidthOf(1)

  inputs.append(listN)
  inputs.append(listN.inputs(1)) // for width inference
}

