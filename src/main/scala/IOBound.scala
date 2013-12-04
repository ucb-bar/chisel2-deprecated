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

trait IODirection;

object INPUT extends IODirection {
  override def toString: String = "INPUT"
}

object OUTPUT extends IODirection {
  override def toString: String = "OUTPUT"
}

object NODIRECTION extends IODirection {
  override def toString: String = "NODIRECTION"
}

object BOTHDIRECTION extends IODirection {
  override def toString: String = "BOTH"
}


/** Pass through binding

  This kind of node is used to connect nodes accross module boundaries.
*/
class IOBound(var dir: IODirection = NODIRECTION,
              widthP: Int = -1,
              opandNode: Node = null) extends Node {

  inferWidth = if( widthP > 0 ) new FixedWidth(widthP) else new WidthOf(0)
  if( opandNode != null ) this.inputs.append(opandNode)
  width = widthP


  override def asDirectionless(): this.type = {
    dir = NODIRECTION
    this
  }

  override def assigned: Node = if (this.inputs.length > 0) this.inputs(0) else null

  def isDirected( dir: IODirection ): Boolean = {
    (( this.dir == dir )
      || ( this.dir == BOTHDIRECTION && dir == INPUT )
      || ( this.dir == BOTHDIRECTION && dir == OUTPUT ))
  }

  override def asInput(): this.type = {
    dir = INPUT
    this
  }

  override def asOutput(): this.type = {
    dir = OUTPUT
    this
  }

  override def flip(): this.type = {
    if (dir == INPUT) {
      dir = OUTPUT
    } else if(dir == OUTPUT) {
      dir = INPUT
    }
    this
  }

  def isDirectionless: Boolean = {
    return dir == NODIRECTION
  }

  def target: Node = if( inputs.length > 0 ) inputs(0) else null

  override def slug: String = {
    assert( dir != null )
    dir.toString + "[" + width + "]"
  }
}
