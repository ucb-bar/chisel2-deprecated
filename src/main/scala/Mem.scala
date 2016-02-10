/*
 Copyright (c) 2011 - 2016 The Regents of the University of
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
import scala.collection.mutable.{ArrayBuffer, HashMap}

/** *seqRead* means that if a port tries to read the same address that another
  port is writing to in the same cycle, the read data is random garbage (from
  a LFSR, which returns "1" on its first invocation).
  */
object Mem {
  private def construct[T <: Data](n: Int, t: => T, seqRead: Boolean,
                       orderedWrites: Boolean,
                       clock: Clock): Mem[T] = {
    val gen = t.cloneType
    Reg.validateGen(gen)
    val res = new Mem(() => gen, n, seqRead, orderedWrites)
    if (clock != null) res.clock = Some(clock)
    res
  }

  def apply[T <: Data](n: Int, t: => T): Mem[T] = {
    construct(n, t, false, false, null)
  }

  def apply[T <: Data](n: Int, t: => T, clock: Clock): Mem[T] = {
    construct(n, t, false, false, clock)
  }

  def apply[T <: Data](out: => T, n: Int, seqRead: Boolean = false,
                       orderedWrites: Boolean = false,
                       clock: Clock = null): Mem[T] = {
    if (Driver.minimumCompatibility > "2") {
      ChiselError.error("Mem(out:T, n:Int) is deprecated. Please use Mem(n:Int, t:T) instead.")
      if (seqRead)
        ChiselError.error("Mem(..., seqRead) is deprecated. Please use SeqMem(...)")
      if (orderedWrites)
        ChiselError.error("Mem(..., orderedWrites) is deprecated.")
    }
    construct(n, out, seqRead, orderedWrites, clock)
  }
}

abstract class AccessTracker extends Delay {
  def writeAccesses: ArrayBuffer[_ <: MemAccess]
  def readAccesses: ArrayBuffer[_ <: MemAccess]
}

class Mem[T <: Data](gen: () => T, val n: Int, val seqRead: Boolean, val orderedWrites: Boolean) extends AccessTracker with VecLike[T] {
  if (seqRead) {
    require(!orderedWrites) // sad reality of realizable SRAMs
  }
  def writeAccesses: ArrayBuffer[MemWrite] = writes ++ readwrites.map(_.write)
  def readAccesses: ArrayBuffer[_ <: MemAccess] = reads ++ seqreads ++ readwrites.map(_.read)
  def ports: ArrayBuffer[_ <: MemAccess] = writes ++ reads ++ seqreads ++ readwrites
  val writes = ArrayBuffer[MemWrite]()
  val seqreads = ArrayBuffer[MemSeqRead]()
  val reads = ArrayBuffer[MemRead]()
  val readwrites = ArrayBuffer[MemReadWrite]()
  val data = gen().toNode
  def dataType = gen()

  inferWidth = Node.fixWidth(data.getWidth)

  private val readPortCache = HashMap[UInt, T]()
  def read(addr: UInt): T = readPortCache getOrElseUpdate (addr, {
    val addrIsReg = addr.getNode.isInstanceOf[Reg]
    val rd = if (seqRead && !Driver.isInlineMem && addrIsReg) {
      (seqreads += new MemSeqRead(this, addr.getNode)).last
    } else {
      (reads += new MemRead(this, addr)).last
    }
    gen().fromNode(rd).asInstanceOf[T]
  })

  def doWrite(addr: UInt, condIn: Bool, wdata: Node, wmaskIn: Option[UInt]): Unit = {
    val cond = // add bounds check if depth is not a power of 2
      condIn && (Bool(isPow2(n)) || addr(log2Up(n)-1,0) < UInt(n))
    val wmask = wmaskIn match { // remove constant-1 write masks
      case Some(mask) => mask.litOpt match {
        case Some(l) if l.value == (BigInt(1) << data.getWidth)-1 => None
        case _ => wmaskIn
      }
      case _ => wmaskIn
    }

    if (orderedWrites) // enforce priority ordering of write ports
      for (w <- writes)
        w.cond = w.cond.asInstanceOf[Bool] && !(cond && addr === w.addr.asInstanceOf[UInt])

    val wr = new MemWrite(this, cond, addr, wdata.toBits, wmask)
    writes += wr
    inputs += wr
  }

  def write(addr: UInt, dataIn: T): Unit = {
    val cond = Module.current.whenCond
    val data = dataIn.toBits
    if (seqRead && Driver.isDebugMem) {
      // generate bogus data when reading & writing same address on same cycle
      val random16 = LFSR16()
      val random_data = Cat(random16, Array.fill((needWidth()-1)/16){random16}:_*)
      doWrite(Reg(next=addr), Reg(next=cond), Reg(next=data), None)
      doWrite(addr, cond, random_data, None)
    } else {
      doWrite(addr, cond, data, None)
    }
  }

  def write(addr: UInt, data: T, wmask: UInt): Unit = {
    if (Driver.minimumCompatibility > "2")
      throwException("Chisel3 masked writes are only supported for Mem[Vec[_]]")
    doWrite(addr, Module.current.whenCond, data, wmask)
  }

  def write(addr: UInt, data: T, mask: Vec[Bool]) (implicit evidence: T <:< Vec[_]): Unit = {
    val bitmask = FillInterleaved(gen().asInstanceOf[Vec[Data]].head.getWidth, mask)
    doWrite(addr, Module.current.whenCond, data, bitmask)
  }

  def apply(addr: UInt): T = {
    val rdata = read(addr)
    rdata.comp = Some(new PutativeMemWrite(this, addr))
    rdata
  }

  override val hashCode: Int = _id
  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  def apply(addr: Int): T = apply(UInt(addr))

  def length: Int = n

  override lazy val isInVCD = Driver.isVCDMem

  override def toString: String = "TMEM(" + ")"

  def cloneType = new Mem(gen, n, seqRead, orderedWrites)
  override def clone = cloneType

  def convertMaskedWrites {
    if (Driver.isInlineMem) {
      writes filter (_.isMasked) foreach {write =>
        val addr = write.addr match {case u: UInt => u}
        val mask = write.mask match {case u: UInt => u}
        val data = write.data.asInstanceOf[T]
        val read = readPortCache getOrElseUpdate (addr, gen().fromNode(new MemRead(this, addr)))
        write.data = data.toBits & mask | read.toBits & ~mask
      }
    }
  }

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

  override def assignClock(clk: Clock): Unit = {
    for (w <- writes) w.clock = Some(clk)
    super.assignClock(clk)
  }
  // Chisel3 - this node contains data - used for verifying Wire() wrapping
  override def isTypeOnly = false
}

abstract class MemAccess(val mem: Mem[_], addri: Node) extends Node {
  def addr = inputs(0)
  def cond: Node
  inputs += addri

  var referenced = false
  def used = referenced
  def getPortType: String

  override def forceMatchingWidths =
    if (addr.needWidth() != log2Up(mem.n)) inputs(0) = addr.matchWidth(Width(log2Up(mem.n)))

  // Chisel3 - this node contains data - used for verifying Wire() wrapping
  override def isTypeOnly = false
}

class MemRead(mem: Mem[_ <: Data], addri: Node) extends MemAccess(mem, addri) {
  override def cond = Bool(true)

  inputs += mem
  inferWidth = Node.fixWidth(mem.data.getWidth)

  override def toString: String = mem + "[" + addr + "]"
  override def getPortType: String = "cread"
}

class MemSeqRead(mem: Mem[_ <: Data], addri: Node) extends MemAccess(mem, addri) with Delay {
  val addrReg = addri.asInstanceOf[Reg]
  override def addr = if (inputs.length > 2) inputs(2) else null
  override def cond = if (inputs.length > 3) inputs(3) else null

  override def forceMatchingWidths = {
    inputs += addrReg.updateValue.matchWidth(Width(log2Up(mem.n)))
    inputs += addrReg.enableSignal
  }

  inputs += mem
  inferWidth = Node.fixWidth(mem.data.getWidth)

  override def toString: String = mem + "[" + addr + "]"
  override def getPortType: String = "read"
}

class PutativeMemWrite(mem: Mem[_ <: Data], addri: UInt) extends Node with proc {
  override def procAssign(src: Node) =
    mem.doWrite(addri, Module.current.whenCond, src, None)
  // Chisel3 - this node contains data - used for verifying Wire() wrapping
  override def isTypeOnly = false
}

class MemReadWrite(val read: MemSeqRead, val write: MemWrite) extends MemAccess(read.mem, null)
{
  override def cond = throwException("")
  override def getPortType = if (write.isMasked) "mrw" else "rw"
}

class MemWrite(mem: Mem[_ <: Data], condi: Bool, addri: Node, datai: Node, maski: Option[Node]) extends MemAccess(mem, addri) {
  override def cond = inputs(1)
  def cond_=(c: Bool) = inputs(1) = c
  clock = mem.clock

  inferWidth = Node.fixWidth(mem.data.getWidth)

  inputs += condi
  inputs += datai
  maski match { case Some(m) => inputs += m case None => }

  override def forceMatchingWidths = {
    super.forceMatchingWidths
    inputs(2) = inputs(2).matchWidth(mem.widthW)
    if (isMasked) inputs(3) = inputs(3).matchWidth(mem.widthW)
  }

  def emitRWEnable(r: MemSeqRead) = {
    def getProducts(x: Node): List[Node] = {
      if (x.isInstanceOf[Op] && x.asInstanceOf[Op].op == "&") {
        List(x) ++ getProducts(x.inputs(0)) ++ getProducts(x.inputs(1))
      } else {
        List(x)
      }
    }

    val wp = getProducts(cond)
    val rp = getProducts(r.addrReg.enableSignal)
    wp.find(wc => rp.exists(rc => rc._isComplementOf(wc)))
  }
  def data_=(d: Bits) { inputs(2) = d }
  def data = inputs(2)
  def mask = inputs(3)
  def isMasked = inputs.length > 3
  override def toString: String = mem + "[" + addr + "] = " + data + " COND " + cond
  override def getPortType: String = if (isMasked) "mwrite" else "write"
  override def usesInClockHi(n: Node) = inputs.contains(n)
}

// Chisel3
object SeqMem {
  def apply[T <: Data](n: Int, out: => T): SeqMem[T] = {
    val gen = out.cloneType
    Reg.validateGen(gen)
    new SeqMem(n, gen)
  }

  @deprecated("SeqMem(out: => T, n:Int) is deprecated. Please use SeqMem(n:Int, out: => T) instead.", "2.29")
  def apply[T <: Data](out: => T, n: Int): SeqMem[T] = {
    if (Driver.minimumCompatibility > "2") {
      ChiselError.error("SeqMem(out: => T, n:Int) is deprecated. Please use SeqMem(n:Int, out: => T) instead.")
    }
    apply(n, out)
  }
}

class SeqMem[T <: Data](n: Int, out: T) extends Mem[T](() => out, n, true, false) {
  override def apply(addr: UInt): T = throwException("SeqMem.apply unsupported")
  override def read(addr: UInt): T = super.read(Reg(next = addr))
  def read(addr: UInt, enable: Bool): T = super.read(RegEnable(addr, enable))

  override def write(addr: UInt, data: T, mask: Vec[Bool]) (implicit evidence: T <:< Vec[_]): Unit = {
    val bitmask = FillInterleaved(out.asInstanceOf[Vec[Data]].head.getWidth, mask)
    doWrite(addr, Module.current.whenCond, data, bitmask)
  }

  override def write(addr: UInt, data: T, mask: UInt): Unit =
    throwException("Chisel3 masked writes are only supported for Mem[Vec[_]]")

  @deprecated("setMemName is equivalent to setName and will be removed", "2.0")
  def setMemName(name: String): Unit = setName(name)
}
