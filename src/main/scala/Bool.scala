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

object Bool {
  def apply(node: Node): Bool = {
    val res = new Bool()
    res.node = node
    res
  }

  def apply(x: Boolean): Bool = Bool(Literal(if(x) 1 else 0, 1))

  def apply(dir: IODirection = NODIRECTION): Bool = {
    val res = new Bool()
    res.node = new IOBound(dir, 1)
    res
  }

  /** Factory method to create a don't-care. */
  def DC: Bool = Bool(Literal("b?", 1))

  implicit def booleanToBool(x: Boolean): Bool = Bool(x)
}


class Bool extends UInt {

  def && (right: Bool): Bool = LogicalAnd(this, right)
  def || (right: Bool): Bool = LogicalOr(this, right)

}


object LogicalAnd {
  def apply( left: Bits, right: Bits): Bool = {
    if(Module.searchAndMap
      && Module.chiselAndMap.contains((left, right))) {
      Module.chiselAndMap((left, right))
    }
    val op = {
      if (left.isConst) {
        if( left.node.asInstanceOf[Literal].value > 0 ) {
          right.lvalue()
        } else {
          left.node // alias to false
        }
      } else if( right.isConst ) {
        if( right.node.asInstanceOf[Literal].value > 0 ) {
          left.lvalue()
        } else {
          right.node // alias to true
        }
      } else {
        new LogicalAndOp(left.lvalue(), right.lvalue())
      }
    }
    val result = Bool(op)
    if(Module.searchAndMap && !Module.chiselAndMap.contains((left, right))) {
      Module.chiselAndMap += ((left, right) -> result)
    }
    result
  }
}


object LogicalOr {
  def apply( left: Bits, right: Bits): Bool = {
    Bool(
      if (left.isConst) {
        if( left.node.asInstanceOf[Literal].value > 0 ) {
          left.node // alias to true
        } else {
          right.lvalue()
        }
      } else if( right.isConst ) {
        if( right.node.asInstanceOf[Literal].value > 0 ) {
          right.node // alias to true
        } else {
          left.lvalue()
        }
      } else {
        new LogicalOrOp(left.lvalue(), right.lvalue())
      })
  }
}
