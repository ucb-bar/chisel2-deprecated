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
import Literal._
import scala.collection.mutable.ArrayBuffer

object Lookup {
  def apply[T <: Bits](addr: UFix, default: T, mapping: Seq[(UFix, T)]): T = {
    if (Component.backend.isInstanceOf[CppBackend]) {
      return CListLookup(addr, List(default), mapping.map(m => (m._1, List(m._2))).toArray).head
    }
    val lookup = new Lookup()
    val mappingNode = mapping.map(x => LookupMap(x))
    lookup.initOf("", widthOf(1), List(addr, default) ++ mappingNode)
    default.fromNode(lookup)
  }
}

object LookupMap {
  def apply[T <: Data](map: (UFix, T)): LookupMap = {
    val res = new LookupMap()
    res.init("", widthOf(0), map._1, map._2)
    res
  }
}

class LookupMap extends Node {
  def addr = inputs(0)
  def data = inputs(1)
}

class Lookup extends Node {
  override def isInObject = true;

  def map = inputs.slice(2, inputs.length).map(x => x.asInstanceOf[LookupMap])

  override def toString: String = "LOOKUP(" + inputs(0) + ")";
}
