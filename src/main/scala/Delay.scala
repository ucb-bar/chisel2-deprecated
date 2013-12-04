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

trait StateWrite {

  def getClock: Update = null

}

/** posedge update signal for delays. This is a clock  */
class Update(srci: Update = null, val mul: Int = 1, val div: Int = 1)
    extends Node {

  inferWidth = new FixedWidth(1)
  if (srci != null) this.inputs.append(srci)

  def src: Update = if( inputs.length > 0 ) inputs(0).asInstanceOf[Update] else null

}


/** Represents nodes which are holding state (registers and srams) */
abstract class Delay(clockNode: Update) extends Node {

  this.inputs.append(clockNode)

  def clock: Update = this.inputs(0).asInstanceOf[Update]

  def reset: Node

}

/** Storage nodes represented as registers. */
class RegDelay(clockNode: Update, nextNode: Node,
  initNode: Node = null, resetNode: Node = null,
  val depth: Int = 1) extends Delay(clockNode) with StateWrite {

  val CLOCK_NEXT = 1
  val CLOCK_INIT = 2
  val CLOCK_INIT_RESET = 4

  var state = CLOCK_NEXT

  inferWidth = new WidthOf(1)
  this.inputs.append(nextNode)
  if( initNode != null ) {
    this.inputs.append(initNode)
    state = CLOCK_INIT
    /* If there is no init/reset value, we don't bother to hook-up the reset
     control signal. */
    if( resetNode != null ) {
      resetNode.isReset = true
      this.inputs.append(resetNode)
      state = CLOCK_INIT_RESET
    }
  }

  def next: Node = this.inputs(CLOCK_NEXT)
  def init: Node  = if( state == CLOCK_INIT_RESET || state == CLOCK_INIT ) this.inputs(2) else null

  override def reset: Node  = if ( state == CLOCK_INIT_RESET ) this.inputs(3) else null

  def hasReset: Boolean = (init != null)

  override def assigned: Node = next

  override def getClock: Update = clock

/** XXX Don't try to be smart here, use when to generate enable signal.
  def enable(): Node = {
    next match {
      case mux: MuxOp => mux.enable
      case _ => null
    }
  }
  */
  def enable: Node = if ( this.inputs.length > 4 ) this.inputs(4) else null

  def setEnable( n: Node ) {
    while( this.inputs.length < 3 ) {
      this.inputs.append(null)
    }
    this.inputs.append(n)
  }

}

/** Storage nodes represented as srams. */
class MemDelay(clockNode: Update, resetNode: Node = null,
  val depth: Int = 1, isInlineP: Boolean = false) extends Delay(clockNode) {

  resetNode.isReset = true
  this.inputs.append(resetNode)

  override def reset: Node  = this.inputs(1)

  override def isInVCD = false

  def isInline = isInlineP

  val ports = new ArrayBuffer[MemAccess]()

  def writes(): Seq[MemWrite] = ports.filter(_.isInstanceOf[MemWrite]).map(
    _.asInstanceOf[MemWrite])
}


class ROMemDelay(inits: Seq[Node],
  clockNode: Update, resetNode: Node = null,
  depth: Int = 1, isInlineP: Boolean = false)
    extends MemDelay(clockNode, resetNode, depth, isInlineP) {

  this.inputs ++= inits
}


/** Base class for memory ports (read, write, read/write)
  */
abstract class MemAccess(memN: MemDelay, addrN: Node) extends Node {

  inferWidth = new WidthOf(0)

  inputs += memN
  inputs += addrN
  memN.ports.append(this)

  def mem: MemDelay = inputs(0).asInstanceOf[MemDelay]
  def addr: Node = inputs(1)
  def cond: Node

  var referenced = false
  def used = referenced
  def getPortType: String
}


/** Memory Read Port
  */
class MemRead(mem: MemDelay, addr: Node) extends MemAccess(mem, addr) {

  override def cond = Literal(1)

  override def toString: String = mem + "[" + addr + "]"
  override def getPortType: String = "cread"
}


/** Memory Sequential Read Port
  */
class MemSeqRead(mem: MemDelay, addri: Node) extends MemAccess(mem, addri) {

  override def cond: Node = {
    val enable = addr.asInstanceOf[RegDelay].enable
    if( enable != null ) enable else Literal(1)
  }

  override def toString: String = mem + "[" + addr + "]"
  override def getPortType: String = "read"

}


/** Memory Write Port
  */
class MemWrite(mem: MemDelay, addrN: Node, dataN: Node,
  condN: Node, maskN: Node = null) extends MemAccess(mem, addrN)
    with StateWrite {

  this.inputs += dataN
  this.inputs += condN
  if( maskN != null ) this.inputs += maskN

//XXX cannot override mutable variable  override def clock = mem.clock

  override def cond = inputs(3)

  def data = inputs(2)

  def mask = inputs(4)

  def isMasked: Boolean = inputs.length > 4

  override def getClock: Update = mem.clock

  var pairedRead: MemSeqRead = null

  def emitRWEnable(r: MemSeqRead) = {
    def getProducts(x: Node): List[Node] = {
      if (x.isInstanceOf[LogicalAndOp]) {
        List(x) ++ getProducts(x.inputs(0)) ++ getProducts(x.inputs(1))
      } else {
        List(x)
      }
    }
    def isNegOf(x: Node, y: Node) = (
      x.isInstanceOf[LogicalNegOp] && x.inputs(0) == y)

    val wp = getProducts(cond)
    val rp = getProducts(r.cond)
    wp.find(wc => rp.exists(rc => isNegOf(rc, wc) || isNegOf(wc, rc)))
  }

  override def toString: String = mem + "[" + addr + "] = " + data + " COND " + cond
  override def getPortType: String = if (isMasked) "mwrite" else "write"
  override def isRamWriteInput(n: Node) = inputs.contains(n)
}


/** Memory Read/Write Port
  */
class MemReadWrite(val read: MemSeqRead, val write: MemWrite) extends MemAccess(read.mem, null)
{
  override def cond = throw new Exception("")
  override def getPortType = if (write.isMasked) "mrw" else "rw"
}

