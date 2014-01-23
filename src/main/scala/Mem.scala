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

import ChiselError._
import scala.reflect._
import scala.reflect.runtime.universe._
import scala.collection.mutable.{ArrayBuffer, HashMap}


/** Reference to a location inside a state-holding register or sram.
  */
class MemReference(memN: MemDelay, addrN: Node) extends Node {

  inferWidth = new FixedWidth(log2Up(memN.depth))

  this.inputs.append(memN)
  this.inputs.append(addrN)

  def mem = this.inputs(0).asInstanceOf[MemDelay]
  def addr = this.inputs(1)

}


/** *seqRead* means that if a port tries to read the same address that another
  port is writing to in the same cycle, the read data is random garbage (from
  a LFSR, which returns "1" on its first invocation).
  */
object Mem {
  def apply[T <: Data : ClassTag](out: T, depth: Int, seqRead: Boolean = false,
    clock: Clock = Module.scope.clock, reset: Bool = Module.scope.reset): Mem[T] = {

    val gen = out.clone
    val res = new Mem(() => gen, clock, reset, depth, seqRead)
    res.node.inferWidth = out.toBits.node.inferWidth
    res
  }

  Module.backend.transforms.prepend { c =>
    c.bfs { n =>
      if (n.isInstanceOf[MemAccess]) {
        n.asInstanceOf[MemAccess].referenced = true
      }
    }
    c.bfs { n =>
      if (n.isInstanceOf[Mem[_]]) {
        n.asInstanceOf[Mem[_]].computePorts
      }
    }
  }
}


abstract class AccessTracker extends nameable {
  def writeAccesses: ArrayBuffer[_ <: MemAccess]
  def readAccesses: ArrayBuffer[_ <: MemAccess]
}

/** Instantiation of a Memory.

  *seqRead* means that if a port tries to read the same address that another
  port is writing to in the same cycle, the read data is random garbage (from
  a LFSR, which returns "1" on its first invocation).
  */
class Mem[T <: Data : ClassTag](gen: () => T, clock: Clock, reset: Bool, val depth: Int,
  val seqRead: Boolean, isInline: Boolean = Module.isInlineMem)
    extends AccessTracker {

  val node = new MemDelay(
    clock.node.asInstanceOf[Update], reset.node, depth, isInline)

  Module.scope.compStack.top.mems.append(node)

  def nameIt(name: String): this.type = {
    if( !named ) {
      if( node != null ) node.nameIt(name)
    }
    this
  }

  def writeAccesses: ArrayBuffer[MemWrite] =
    (writes ++ readwrites.map(_.write))

  def readAccesses: ArrayBuffer[_ <: MemAccess] =
    (reads ++ seqreads ++ readwrites.map(_.read))

  val writes = ArrayBuffer[MemWrite]()
  val seqreads = ArrayBuffer[MemSeqRead]()
  val reads = ArrayBuffer[MemRead]()
  val readwrites = ArrayBuffer[MemReadWrite]()
  val data = gen().toBits

  private val readPortCache = HashMap[UInt, T]()

  def doRead(addr: UInt): T = {
    if (readPortCache.contains(addr)) {
      return readPortCache(addr)
    }

    val addrIsReg = (addr.isInstanceOf[UInt] && addr.node.inputs.length == 1
      && addr.node.inputs(0).isInstanceOf[RegDelay])
    val rd = if (seqRead && !Module.isInlineMem && addrIsReg) {
      (seqreads += new MemSeqRead(this.node, addr.node.inputs(0))).last
    } else {
      (reads += new MemRead(this.node, addr.node)).last
    }
    val data = gen()
    data.fromBits(UInt(rd))
    readPortCache += (addr -> data)
    data
  }

  def read(addr: UInt): T = doRead(addr)

  def write(addr: UInt, data: T): Unit = write(addr, data, null.asInstanceOf[UInt])

  def write(addr: UInt, data: T, wmaskIn: UInt): Unit = {
    val condIn = Module.scope.topCond
    /** XXX Cannot specify return type as it can either be proc or MemWrite
      depending on the execution path you believe. */
    val cond = // add bounds check if depth is not a power of 2
      if (isPow2(this.depth)) {
        condIn
      } else {
        condIn && addr(log2Up(this.depth)-1,0) < UInt(this.depth)
      }
    val wmask = // remove constant-1 write masks
      if (!(wmaskIn == null)
        && wmaskIn.isConst
        && wmaskIn.node.asInstanceOf[Literal].value == (BigInt(1) << data.getWidth)-1) {
        null
      } else {
        wmaskIn
      }

/* XXX Broken way to randomize read output
    if (seqRead && Module.backend.isInstanceOf[CppBackend] && gen().isInstanceOf[Bits]) {
      // generate bogus data when reading & writing same address on same cycle
      val reg_data = new Reg()
      reg_data.inputs.append(wdata.node)
      val reg_wmask = if (wmask == null) null else Reg(next=wmask)
      val random16 = LFSR16()
      val random_data = Cat(random16, Array.fill((width-1)/16){random16}:_*)
      doit(Reg(next=addr), Reg(next=cond), reg_data, reg_wmask)
      doit(addr, cond, UInt(random_data.node), wmask)
      reg_data
    } else {
 */
/*
     }
     */

    val wr = new MemWrite(this.node, addr.node,
      data.toBits.node, cond.node, new WrapOp(wmask.node))
    this.writes += wr
  }


  def apply(addr: UInt): T = {
    // val result = implicitly[ClassTag[T]].runtimeClass.newInstance.asInstanceOf[T]
    val result = gen().asInstanceOf[T]
    result.fromBits(UInt(new MemReference(this.node, addr.node)))
    result
  }

  override def toString: String = "TMEM(" + ")"

  override def clone: this.type = Mem(gen(), depth, seqRead).asInstanceOf[this.type]

  def computePorts = {
    reads --= reads.filterNot(_.used)
    seqreads --= seqreads.filterNot(_.used)
    writes --= writes.filterNot(_.used)

    // try to extract RW ports
    for (w <- writes; r <- seqreads)
      if (!w.emitRWEnable(r).isEmpty && !readwrites.contains((rw: MemReadWrite) => rw.read == r || rw.write == w)) {
        readwrites += new MemReadWrite(r, w)
      }
    writes --= readwrites.map(_.write)
    seqreads --= readwrites.map(_.read)
  }
}


