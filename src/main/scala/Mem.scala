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
import Node._
import scala.reflect._
import scala.collection.mutable.{ArrayBuffer, HashMap}

/** *seqRead* means that if a port tries to read the same address that another
  port is writing to in the same cycle, the read data is random garbage (from
  a LFSR, which returns "1" on its first invocation).
  */
object Mem {
  def apply[T <: Data](out: T, n: Int, seqRead: Boolean = false,
                       orderedWrites: Boolean = false,
                       clock: Clock = null): Mem[T] = {
    val gen = out.clone
    Reg.validateGen(gen)
    val res = new Mem(() => gen, n, seqRead, orderedWrites)
    if (!(clock == null)) res.clock = clock
    res
  }

  Driver.backend.transforms.prepend { c =>
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

abstract class AccessTracker extends Delay {
  def writeAccesses: ArrayBuffer[_ <: MemAccess]
  def readAccesses: ArrayBuffer[_ <: MemAccess]
}

class Mem[T <: Data](gen: () => T, val n: Int, val seqRead: Boolean, val orderedWrites: Boolean) extends AccessTracker with VecLike[T] {
  if (seqRead)
    require(!orderedWrites) // sad reality of realizable SRAMs
  def writeAccesses: ArrayBuffer[MemWrite] = writes ++ readwrites.map(_.write)
  def readAccesses: ArrayBuffer[_ <: MemAccess] = reads ++ seqreads ++ readwrites.map(_.read)
  def ports: ArrayBuffer[_ <: MemAccess] = writes ++ reads ++ seqreads ++ readwrites
  val writes = ArrayBuffer[MemWrite]()
  val seqreads = ArrayBuffer[MemSeqRead]()
  val reads = ArrayBuffer[MemRead]()
  val readwrites = ArrayBuffer[MemReadWrite]()
  val data = gen().toNode

  inferWidth = fixWidth(data.getWidth)

  private val readPortCache = HashMap[UInt, T]()
  def read(addr: UInt): T = {
    if (readPortCache.contains(addr)) {
      return readPortCache(addr)
    }

    val addrIsReg = addr.isInstanceOf[UInt] && addr.inputs.length == 1 && addr.inputs(0).isInstanceOf[Reg]
    val rd = if (seqRead && !Driver.isInlineMem && addrIsReg) {
      (seqreads += new MemSeqRead(this, addr.inputs(0))).last
    } else {
      (reads += new MemRead(this, addr)).last
    }
    val data = gen().fromNode(rd).asInstanceOf[T]
    readPortCache += (addr -> data)
    data
  }

  def doWrite(addr: UInt, condIn: Bool, wdata: Node, wmaskIn: UInt): Unit = {
    val cond = // add bounds check if depth is not a power of 2
      condIn && (Bool(isPow2(n)) || addr(log2Up(n)-1,0) < UInt(n))
    val wmask = // remove constant-1 write masks
      if (!(wmaskIn == null) && wmaskIn.litOf != null && wmaskIn.litOf.value == (BigInt(1) << data.getWidth)-1) {
        null
      } else {
        wmaskIn
      }

    if (orderedWrites) // enforce priority ordering of write ports
      for (w <- writes)
        w.cond = w.cond.asInstanceOf[Bool] && !(cond && addr === w.addr.asInstanceOf[UInt])

    val wr = new MemWrite(this, cond, addr, wdata, wmask)
    writes += wr
    inputs += wr
  }

  def write(addr: UInt, data: T): Unit = {
    val cond = Module.current.whenCond
    if (seqRead && Driver.backend.isInstanceOf[CppBackend] && gen().isInstanceOf[Bits]) {
      // generate bogus data when reading & writing same address on same cycle
      val reg_data = new Reg().init("", widthOf(0), data)
      val random16 = LFSR16()
      val random_data = Cat(random16, Array.fill((width-1)/16){random16}:_*)
      doWrite(Reg(next=addr), Reg(next=cond), reg_data, null.asInstanceOf[UInt])
      doWrite(addr, cond, random_data, null.asInstanceOf[UInt])
    } else {
      doWrite(addr, cond, data, null.asInstanceOf[UInt])
    }
  }

  def write(addr: UInt, data: T, wmask: UInt): Unit =
    if (!Driver.isInlineMem) doWrite(addr, Module.current.whenCond, data, wmask)
    else doWrite(addr, Module.current.whenCond, gen().fromBits(data.toBits & wmask | read(addr).toBits & ~wmask), null.asInstanceOf[UInt])

  def apply(addr: UInt): T = {
    val rdata = read(addr)
    rdata.comp = new PutativeMemWrite(this, addr)
    rdata
  }

  override val hashCode: Int = _id
  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  def apply(addr: Int): T = apply(UInt(addr))

  def length: Int = n

  override lazy val isInVCD = false

  override def toString: String = "TMEM(" + ")"

  override def clone = new Mem(gen, n, seqRead, orderedWrites)

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

  def isInline = Driver.isInlineMem || !reads.isEmpty
}

abstract class MemAccess(val mem: Mem[_], addri: Node) extends Node {
  def addr = inputs(0)
  def cond: Node
  inputs += addri

  var referenced = false
  def used = referenced
  def getPortType: String

  override def forceMatchingWidths =
    if (addr.width != log2Up(mem.n)) inputs(0) = addr.matchWidth(log2Up(mem.n))
}

class MemRead(mem: Mem[_], addri: Node) extends MemAccess(mem, addri) {
  override def cond = Bool(true)

  inputs += mem
  inferWidth = fixWidth(mem.data.getWidth)

  override def toString: String = mem + "[" + addr + "]"
  override def getPortType: String = "cread"
}

class MemSeqRead(mem: Mem[_], addri: Node) extends MemAccess(mem, addri) {
  val addrReg = addri.asInstanceOf[Reg]
  override def cond = addrReg.enableSignal
  override def isReg = true
  override def addr = if(inputs.length > 2) inputs(2) else null

  override def forceMatchingWidths = {
    val forced = addrReg.next.matchWidth(log2Up(mem.n))
    inputs += forced
    assert(addr == forced)
  }

  inputs += mem
  inferWidth = fixWidth(mem.data.getWidth)

  override def toString: String = mem + "[" + addr + "]"
  override def getPortType: String = "read"
  override def isRamWriteInput(n: Node) = addrReg.isEnable && addrReg.enableSignal == n || addr == n
}

class PutativeMemWrite(mem: Mem[_], addri: UInt) extends Node with proc {
  override def procAssign(src: Node) =
    mem.doWrite(addri, Module.current.whenCond, src, null.asInstanceOf[UInt])
}

class MemReadWrite(val read: MemSeqRead, val write: MemWrite) extends MemAccess(read.mem, null)
{
  override def cond = throw new Exception("")
  override def getPortType = if (write.isMasked) "mrw" else "rw"
}

class MemWrite(mem: Mem[_], condi: Bool, addri: Node, datai: Node, maski: Node) extends MemAccess(mem, addri) {
  inputs += condi
  override def cond = inputs(1)
  def cond_=(c: Bool) = inputs(1) = c
  clock = mem.clock

  inferWidth = fixWidth(mem.data.getWidth)

  if (datai != null) {
    def wrap(x: Node) = { // prevent Verilog syntax errors when indexing constants
      val b = UInt()
      b.inputs += x
      b
    }
    inputs += wrap(datai)
    if (maski != null) {
      inputs += wrap(maski)
    }
  }

  override def forceMatchingWidths = {
    val w = mem.width
    super.forceMatchingWidths
    if(inputs.length >= 3 && inputs(2).width != w) inputs(2) = inputs(2).matchWidth(w)
    if(inputs.length >= 4 && inputs(3).width != w) inputs(3) = inputs(3).matchWidth(w)
  }

  var pairedRead: MemSeqRead = null
  def emitRWEnable(r: MemSeqRead) = {
    def getProducts(x: Node): List[Node] = {
      if (x.isInstanceOf[Op] && x.asInstanceOf[Op].op == "&&") {
        List(x) ++ getProducts(x.inputs(0)) ++ getProducts(x.inputs(1))
      } else {
        List(x)
      }
    }
    def isNegOf(x: Node, y: Node) = x.isInstanceOf[Op] && x.asInstanceOf[Op].op == "!" && x.inputs(0) == y

    val wp = getProducts(cond)
    val rp = getProducts(r.cond)
    wp.find(wc => rp.exists(rc => isNegOf(rc, wc) || isNegOf(wc, rc)))
  }
  def data = inputs(2)
  def mask = inputs(3)
  def isMasked = inputs.length > 3
  override def toString: String = mem + "[" + addr + "] = " + data + " COND " + cond
  override def getPortType: String = if (isMasked) "mwrite" else "write"
  override def isRamWriteInput(n: Node) = inputs.contains(n)
}
