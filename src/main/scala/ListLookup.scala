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
import Node._
import scala.collection.mutable.ArrayBuffer

object ListLookup {
  def apply[T <: Bits](addr: UFix, default: List[T], mapping: Array[(UFix, List[T])]): List[T] = {
    if (Mod.backend.isInstanceOf[CppBackend]) {
      return CListLookup(addr, default, mapping)
    }
    val defaultNode = ListNode(default)
    val mappingNode = mapping.map(x => MapNode(x))
    val ll = new ListLookup[T]()
    ll.initOf("", widthOf(1), List(addr, defaultNode) ++ mappingNode)
    ll.wires = default.map(x => ListLookupRef(x, ll))
    // TODO: GENERALIZE AND SHARE THIS
    (default zip ll.wires).map{case(x, xRef) => {
      val res = x match {
        case fix: Fix => Fix(OUTPUT);
        case any => UFix(OUTPUT);
      }
      xRef.nameHolder = res
      res.inputs += xRef
      res.setIsTypeNode
      res.asInstanceOf[T]
    }}
  }
}

class ListLookup[T <: Bits] extends Node {
  var wires: List[ListLookupRef[T]] = null

  def addr: Node = inputs(0)

  def defaultWires = inputs(1).inputs

  def map: ArrayBuffer[(Node, ArrayBuffer[Node])] = {
    val mapArray = inputs.slice(2, inputs.length)
    val res = ArrayBuffer[(Node, ArrayBuffer[Node])]()
    for(elm <- mapArray)
      res += (elm.asInstanceOf[MapNode].addr -> elm.asInstanceOf[MapNode].data)
    res
  }

  override def toString: String = "LISTLOOKUP(" + inputs(0) + ")";

  override def isByValue: Boolean = false;
}

object ListLookupRef {
  def apply[T <: Bits](x: T, ll: ListLookup[T]): ListLookupRef[T] = {
    val res = new ListLookupRef[T]()
    res.init("", widthOf(1), ll, x)
    res
  }
}

class ListLookupRef[T <: Bits]() extends Node {
  override def toString: String = name
}

object ListNode {
  def apply[T <: Data](nodes: List[T]): ListNode = {
    val res = new ListNode()
    res.init("", widthOf(0), nodes: _*)
    res
  }
}

class ListNode extends Node {
}

object MapNode {
  def apply[T <: Bits](map: (UFix, List[T])): MapNode = {
    val res = new MapNode()
    res.initOf("", widthOf(0), List(map._1) ++ map._2)
    res
  }
}

class MapNode extends Node {
  def addr: Node = inputs(0)
  def data = inputs.slice(1, inputs.length)
}
