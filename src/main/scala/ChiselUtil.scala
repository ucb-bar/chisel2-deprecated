package Chisel
import Node._
import scala.math._

object log2Up
{
  def apply(in: Int) = if(in == 1) 1 else ceil(log(in)/log(2)).toInt
}


object log2Down
{
  def apply(x : Int) = if (x == 1) 1 else floor(log(x)/log(2.0)).toInt
}


object isPow2
{
  def apply(in: Int) = in > 0 && ((in & (in-1)) == 0)
}

object LFSR16
{
  def apply(increment: Bool = Bool(true)) =
  {
    val width = 16
    val lfsr = Reg(resetVal = UFix(1, width))
    when (increment) { lfsr := Cat(lfsr(0)^lfsr(2)^lfsr(3)^lfsr(5), lfsr(width-1,1)).toUFix }
    lfsr
  }
}

object PopCount
{
  def apply(in: Seq[Bool]): UFix = {
    if (in.size == 0)
      UFix(0)
    else if (in.size == 1)
      in(0)
    else
      apply(in.slice(0, in.size/2)) + Cat(Bits(0), apply(in.slice(in.size/2, in.size)))
  }
  def apply(in: Bits): UFix = apply((0 until in.getWidth).map(in(_).toBool))
}

object Reverse
{
  def doit(in: Bits, base: Int, length: Int): Bits =
  {
    val half = (1 << log2Up(length))/2
    if (length == 1)
      in(base)
    else
      Cat(doit(in, base, half), doit(in, base+half, length-half))
  }
  def apply(in: Bits) = doit(in, 0, in.getWidth)
}


object ShiftRegister
{
  def apply [T <: Data](n: Int, in: T, en: Bool = Bool(true)): T =
  {
    if (n == 1)
    {
      val res = Reg() { in.clone }
      when (en)
      {
        res := in
      }
      res
    }
    else
    {
      Reg(apply(n-1, in, en))
    }
  }
}

object UFixToOH
{
  def apply(in: UFix, width: Int = -1): Bits =
  {
    if (width == -1)
      UFix(1) << in
    else
      UFix(1) << in(log2Up(width)-1,0)
  }
}

object Mux1H 
{
  def apply [T <: Data](sel: Seq[Bits], in: Seq[T]): T = {
    if (in.size == 1)
      in(0)
    else
      in(0).fromBits(sel.zip(in).map { case(s,x) => s.toFix & x.toBits }.reduce(_|_))
  }
  def apply [T <: Data](sel: Bits, in: Seq[T]): T = apply((0 until in.size).map(sel(_)), in)
}


object OHToUFix
{
  def apply(in: Seq[Bits]): UFix = {
    if (in.size <= 1) return UFix(0)
    if (in.size == 2) return in(1)
    val hi = in.slice(in.size/2, in.size)
    val lo = in.slice(0, in.size/2)
    Cat(hi.reduceLeft(_||_), apply(hi zip lo map { case (x, y) => x || y }))
  }
  def apply(in: Bits): UFix = apply((0 until in.getWidth).map(in(_)))
}


class PipeIO[+T <: Data]()(data: => T) extends Bundle
{
  val valid = Bool(OUTPUT)
  val bits = data.asOutput
  def fire(dummy: Int = 0) = valid
  override def clone =
    try {
      super.clone()
    } catch {
      case e: java.lang.Exception => {
        new PipeIO()(data).asInstanceOf[this.type]
      }
    }
}

class FIFOIO[T <: Data]()(data: => T) extends Bundle
{
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val bits  = data.asOutput
  def fire(dummy: Int = 0) = ready && valid
  override def clone =
    try {
      super.clone()
    } catch {
      case e: java.lang.Exception => {
        new FIFOIO()(data).asInstanceOf[this.type]
      }
    }
}

class REGIO[T <: Data]()(data: => T) extends Bundle
{
	val bits = data.asOutput
}

class EnqIO[T <: Data]()(data: => T) extends FIFOIO()(data) 
{
  def enq(dat: T): T = { valid := Bool(true); bits := dat; dat }
  valid := Bool(false);
  for (io <- bits.flatten.map(x => x._2))
    io := UFix(0)
  override def clone = { new EnqIO()(data).asInstanceOf[this.type]; }
}

class DeqIO[T <: Data]()(data: => T) extends FIFOIO()(data) 
{
  flip()
  ready := Bool(false);
  def deq(b: Boolean = false): T = { ready := Bool(true); bits }
  override def clone = { new DeqIO()(data).asInstanceOf[this.type]; }
}


class FIFOIOC[+T <: Data]()(data: => T) extends Bundle
{
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val bits  = data.asOutput
}


class ioArbiter[T <: Data](n: Int)(data: => T) extends Bundle {
  val in  = Vec(n) { (new FIFOIO()) { data } }.flip
  val out = (new FIFOIO()) { data }
  val chosen = Bits(OUTPUT, log2Up(n))
}

object foldR
{
  def apply[T <: Bits](x: Seq[T])(f: (T, T) => T): T =
    if (x.length == 1) x(0) else f(x(0), foldR(x.slice(1, x.length))(f))
}

object ArbiterCtrl
{
  def apply(request: Seq[Bool]) = {
    Bool(true) +: (1 until request.length).map(i => !request.slice(0, i).foldLeft(Bool(false))(_ || _))
  }
}

class Arbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioArbiter(n)(data)

  val grant = ArbiterCtrl(io.in.map(_.valid))
  (0 until n).map(i => io.in(i).ready := grant(i) && io.out.ready)

  var dout = io.in(n-1).bits
  var choose = Bits(n-1)
  for (i <- n-2 to 0 by -1) {
    dout = Mux(io.in(i).valid, io.in(i).bits, dout)
    choose = Mux(io.in(i).valid, Bits(i), choose)
  }

  io.out.valid := io.in.map(_.valid).foldLeft(Bool(false))(_ || _)
  io.out.bits <> dout
  io.chosen := choose
}

class RRArbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioArbiter(n)(data)

  val last_grant = Reg(resetVal = Bits(0, log2Up(n)))
  val g = ArbiterCtrl((0 until n).map(i => io.in(i).valid && UFix(i) > last_grant) ++ io.in.map(_.valid))
  val grant = (0 until n).map(i => g(i) && UFix(i) > last_grant || g(i+n))
  (0 until n).map(i => io.in(i).ready := grant(i) && io.out.ready)

  var choose = Bits(n-1)
  for (i <- n-2 to 0 by -1)
    choose = Mux(io.in(i).valid, Bits(i), choose)
  for (i <- n-1 to 1 by -1)
    choose = Mux(io.in(i).valid && UFix(i) > last_grant, Bits(i), choose)
  when (io.out.valid && io.out.ready) {
    last_grant := choose
  }

  val dvec = Vec(n) { data } 
  (0 until n).map(i => dvec(i) := io.in(i).bits )

  io.out.valid := io.in.map(_.valid).foldLeft(Bool(false))( _ || _)
  io.out.bits := dvec(choose)
  io.chosen := choose
}

class ioLockingArbiter[T <: Data](n: Int)(data: => T) extends Bundle {
  val in   = Vec(n) { (new FIFOIO()) { data } }.flip
  val lock = Vec(n) { Bool() }.asInput
  val out  = (new FIFOIO()) { data }
}

class LockingArbiter[T <: Data](n: Int)(data: => T) extends Component {
  val io = new ioLockingArbiter(n)(data)
  val locked = Vec(n) { Reg(resetVal = Bool(false)) }
  val any_lock_held = (locked.toBits & io.lock.toBits).orR
  val valid_arr = Vec(n) { Bool() } 
  val bits_arr = Vec(n) { data } 
  for(i <- 0 until n) {
    valid_arr(i) := io.in(i).valid
    bits_arr(i) := io.in(i).bits
  }

  io.in(0).ready := Mux(any_lock_held, io.out.ready && locked(0), io.out.ready)
  locked(0) := Mux(any_lock_held, locked(0), io.in(0).ready && io.lock(0))
  for (i <- 1 until n) {
    io.in(i).ready := Mux(any_lock_held, io.out.ready && locked(i), 
                          !io.in(i-1).valid && io.in(i-1).ready)
    locked(i) := Mux(any_lock_held, locked(i), io.in(i).ready && io.lock(i))
  }

  var dout = io.in(n-1).bits
  for (i <- 1 until n)
    dout = Mux(io.in(n-1-i).valid, io.in(n-1-i).bits, dout)

  var vout = io.in(0).valid
  for (i <- 1 until n)
    vout = vout || io.in(i).valid

  val lock_idx = PriorityEncoder(locked.toBits)
  io.out.valid := Mux(any_lock_held, valid_arr(lock_idx), vout)
  io.out.bits  := Mux(any_lock_held, bits_arr(lock_idx), dout)
}

object FillInterleaved
{
  def apply(n: Int, in: Bits) =
  {
    var out = Fill(n, in(0))
    for (i <- 1 until in.getWidth)
      out = Cat(Fill(n, in(i)), out)
    out
  }
}


object Counter
{
  def apply(cond: Bool, n: Int) = {
    val c = Reg(resetVal = UFix(0, log2Up(n)))
    val wrap = c === UFix(n-1)
    when (cond) {
      c := Mux(Bool(!isPow2(n)) && wrap, UFix(0), c + UFix(1))
    }
    (c, wrap && cond)
  }
}

class ioQueue[T <: Data](entries: Int)(data: => T) extends Bundle
{
  val enq   = new FIFOIO()(data).flip
  val deq   = new FIFOIO()(data)
  val count = UFix(OUTPUT, log2Up(entries+1))
}

class Queue[T <: Data](val entries: Int, pipe: Boolean = false, flow: Boolean = false, resetSignal: Bool = null)(data: => T) extends Component(resetSignal)
{
  val io = new ioQueue(entries)(data)

  val do_flow = Bool()
  val do_enq = io.enq.ready && io.enq.valid && !do_flow
  val do_deq = io.deq.ready && io.deq.valid && !do_flow

  var enq_ptr = UFix(0)
  var deq_ptr = UFix(0)

  if (entries > 1) {
    enq_ptr = Counter(do_enq, entries)._1
    deq_ptr = Counter(do_deq, entries)._1
  }

  val maybe_full = Reg(resetVal = Bool(false))
  when (do_enq != do_deq) {
    maybe_full := do_enq
  }

  val ram = Mem(entries) { data }
  when (do_enq) { ram(enq_ptr) := io.enq.bits }

  val ptr_match = enq_ptr === deq_ptr
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val maybe_flow = Bool(flow) && empty
  do_flow := maybe_flow && io.deq.ready
  io.deq.valid :=  !empty || Bool(flow) && io.enq.valid
  io.enq.ready := !full || Bool(pipe) && io.deq.ready
  io.deq.bits := Mux(maybe_flow, io.enq.bits, ram(deq_ptr))

  val ptr_diff = enq_ptr - deq_ptr
  if (isPow2(entries))
    io.count := Cat(maybe_full && ptr_match, ptr_diff).toUFix
  else
    io.count := Mux(ptr_match, Mux(maybe_full, UFix(entries), UFix(0)), Mux(deq_ptr > enq_ptr, UFix(entries) + ptr_diff, ptr_diff))
}

object Queue
{
  def apply[T <: Data](enq: FIFOIO[T], entries: Int = 2, pipe: Boolean = false) = {
    val q = (new Queue(entries, pipe)) { enq.bits.clone }
    q.io.enq.valid := enq.valid // not using <> so that override is allowed
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    q.io.deq
  }
}

class Pipe[T <: Data](latency: Int = 1)(data: => T) extends Component
{
  val io = new Bundle {
    val enq = new PipeIO()(data).flip
    val deq = new PipeIO()(data)
  }

  io.deq <> Pipe(io.enq, latency)
}

object Pipe
{
  def apply[T <: Data](enqValid: Bool, enqBits: T, latency: Int): PipeIO[T] = {
    if (latency == 0) {
      val out = new PipeIO()(enqBits.clone)
      out.valid <> enqValid
      out.bits <> enqBits
      out.setIsTypeNode
      out
    } else {
      val v = Reg(enqValid, resetVal = Bool(false))
      val b = Reg() { enqBits.clone }
      when (enqValid) { b := enqBits }
      apply(v, b, latency-1)
    }
  }
  def apply[T <: Data](enqValid: Bool, enqBits: T): PipeIO[T] = apply(enqValid, enqBits, 1)
  def apply[T <: Data](enq: PipeIO[T], latency: Int = 1): PipeIO[T] = apply(enq.valid, enq.bits, latency)
}

object PriorityMux
{
  def apply[T <: Data](in: Seq[(Bits, T)]): T = {
    if (in.size == 1)
      in.head._2
    else
      Mux(in.head._1, in.head._2, apply(in.tail))
  }
  def apply[T <: Data](sel: Seq[Bits], in: Seq[T]): T = apply(sel zip in)
  def apply[T <: Data](sel: Bits, in: Seq[T]): T = apply((0 until in.size).map(sel(_)), in)
}

object PriorityEncoder
{
  def apply(in: Seq[Bits]): UFix = PriorityMux(in, (0 until in.size).map(UFix(_)))
  def apply(in: Bits): UFix = apply((0 until in.getWidth).map(in(_)))
}

object PriorityEncoderOH
{
  def apply(in: Bits): Bits = Vec(apply((0 until in.getWidth).map(in(_)))){Bool()}.toBits
  def apply(in: Seq[Bits]): Seq[Bool] = {
    var none_hot = Bool(true)
    val out = collection.mutable.ArrayBuffer[Bool]()
    for (i <- 0 until in.size) {
      out += none_hot && in(i)
      none_hot = none_hot && !in(i)
    }
    out
  }
}


