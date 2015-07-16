/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
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
import scala.math._
import Literal._

object log2Up
{
  def apply(in: Int): Int = if(in == 1) 1 else ceil(log(in)/log(2)).toInt
}

object log2Ceil
{
  def apply(in: Int): Int = ceil(log(in)/log(2)).toInt
}


object log2Down
{
  def apply(x : Int): Int = if (x == 1) 1 else floor(log(x)/log(2.0)).toInt
}

object log2Floor
{
  def apply(x : Int): Int = floor(log(x)/log(2.0)).toInt
}


object isPow2
{
  def apply(in: Int): Boolean = in > 0 && ((in & (in-1)) == 0)
}

object foldR
{
  def apply[T <: Bits](x: Seq[T])(f: (T, T) => T): T =
    if (x.length == 1) x(0) else f(x(0), foldR(x.slice(1, x.length))(f))
}

/** linear feedback shift register
  */
object LFSR16
{
  def apply(increment: Bool = Bool(true)): UInt =
  {
    val width = 16
    val lfsr = Reg(init=UInt(1, width))
    when (increment) { lfsr := Cat(lfsr(0)^lfsr(2)^lfsr(3)^lfsr(5), lfsr(width-1,1)) }
    lfsr
  }
}

/** Returns the number of bits set (i.e value is 1) in the input signal.
  */
object PopCount
{
  def apply(in: Iterable[Bool]): UInt = {
    if (in.size == 0) {
      UInt(0)
    } else if (in.size == 1) {
      in.head
    } else {
      apply(in.slice(0, in.size/2)) + Cat(UInt(0), apply(in.slice(in.size/2, in.size)))
    }
  }
  def apply(in: Bits): UInt = apply((0 until in.getWidth).map(in(_)))
}

/** Litte/big bit endian convertion: reverse the order of the bits in a UInt.
*/
object Reverse
{
  private def doit(in: UInt, length: Int): UInt = {
    if (length == 1) {
      in
    } else if (isPow2(length) && length >= 8 && length <= 64) {
      // Do it in logarithmic time to speed up C++.  Neutral for real HW.
      var res = in
      var shift = length >> 1
      var mask = UInt((BigInt(1) << length) - 1, length)
      do {
        mask = mask ^ (mask(length-shift-1,0) << UInt(shift))
        res = ((res >> UInt(shift)) & mask) | (res(length-shift-1,0) << UInt(shift) & ~mask)
        shift = shift >> 1
      } while (shift > 0)
      res
    } else {
      val half = (1 << log2Up(length))/2
      Cat(doit(in(half-1,0), half), doit(in(length-1,half), length-half))
    }
  }
  def apply(in: UInt): UInt = doit(in, in.getWidth)
}


object RegEnable
{
  def apply[T <: Data](updateData: T, enable: Bool) = {
    val r = Reg(updateData)
    when (enable) { r := updateData }
    r
  }
  def apply[T <: Data](updateData: T, resetData: T, enable: Bool) = {
    val r = RegInit(resetData)
    when (enable) { r := updateData }
    r
  }
}

/** Returns the n-cycle delayed version of the input signal.
  */
object ShiftRegister
{
  def apply[T <: Data](in: T, n: Int) : T = apply(in, n, Bool(true))
  def apply[T <: Data](in: T, n: Int, en: Bool) : T =
  {
    // The order of tests reflects the expected use cases.
    if (n == 1) {
      RegEnable(in, en)
    } else if (n != 0) {
      RegNext(apply(in, n-1, en))
    } else {
      in
    }
  }
  
  def apply[T <: Data](in: T, init: T, n: Int) : T = apply(in, init, n, Bool(true))
  def apply[T <: Data](in: T, init: T, n: Int, en: Bool) : T =
  {
    // The order of tests reflects the expected use cases.
    if (n == 1) {
      RegEnable(in, init, en)
    } else if (n != 0) {
      RegNext(apply(in, init, n-1, en), init)
    } else {
      in
    }
  }
}

/** Returns the one hot encoding of the input UInt.
  */
object UIntToOH
{
  def apply(in: UInt, width: Int = -1): UInt =
    if (width == -1) UInt(1) << in
    else (UInt(1) << in(log2Up(width)-1,0))(width-1,0)
}

/** Builds a Mux tree out of the input signal vector using a one hot encoded
  select signal. Returns the output of the Mux tree.
  */
object Mux1H
{
  def apply[T <: Data](sel: Iterable[Bool], in: Iterable[T]): T = {
    if (in.tail.isEmpty) in.head
    else {
      val masked = (sel, in).zipped map ((s, i) => Mux(s, i.toBits, Bits(0)))
      in.head.fromBits(masked.reduceLeft(_|_))
    }
  }
  def apply[T <: Data](in: Iterable[(Bool, T)]): T = {
    val (sel, data) = in.unzip
    apply(sel, data)
  }
  def apply[T <: Data](sel: Bits, in: Iterable[T]): T =
    apply((0 until in.size).map(sel(_)), in)
  def apply(sel: Bits, in: Bits): Bool = (sel & in).orR
}

class ValidIO[+T <: Data](gen: T) extends Bundle
{
  val valid = Bool(OUTPUT)
  val bits = gen.cloneType.asOutput
  def fire(dummy: Int = 0): Bool = valid
  override def cloneType: this.type = new ValidIO(gen).asInstanceOf[this.type]
}

/** Adds a valid protocol to any interface. The standard used is
  that the consumer uses the flipped interface.
*/
object Valid {
  def apply[T <: Data](gen: T): ValidIO[T] = new ValidIO(gen)
}

class DecoupledIO[+T <: Data](gen: T) extends Bundle
{
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val bits  = gen.cloneType.asOutput
  def fire(dummy: Int = 0): Bool = ready && valid
  override def cloneType: this.type = new DecoupledIO(gen).asInstanceOf[this.type]
}

/** Adds a ready-valid handshaking protocol to any interface.
  The standard used is that the consumer uses the flipped
  interface.
  */
object Decoupled {
  def apply[T <: Data](gen: T): DecoupledIO[T] = new DecoupledIO(gen)
}

class EnqIO[T <: Data](gen: T) extends DecoupledIO(gen)
{
  def enq(dat: T): T = { valid := Bool(true); bits := dat; dat }
  valid := Bool(false);
  for (io <- bits.flatten.map(x => x._2))
    io := UInt(0)
  override def cloneType: this.type = new EnqIO(gen).asInstanceOf[this.type]
}

class DeqIO[T <: Data](gen: T) extends DecoupledIO(gen)
{
  flip()
  ready := Bool(false);
  def deq(b: Boolean = false): T = { ready := Bool(true); bits }
  override def cloneType: this.type = new DeqIO(gen).asInstanceOf[this.type]
}


class DecoupledIOC[+T <: Data](gen: T) extends Bundle
{
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val bits  = gen.cloneType.asOutput
}


class ArbiterIO[T <: Data](gen: T, n: Int) extends Bundle {
  val in  = Vec(n,  Decoupled(gen) ).flip
  val out = Decoupled(gen)
  val chosen = UInt(OUTPUT, log2Up(n))
}

object ArbiterCtrl
{
  def apply(request: Seq[Bool]): Seq[Bool] = {
    Bool(true) +: (1 until request.length).map(i => !request.slice(0, i).foldLeft(Bool(false))(_ || _))
  }
}

abstract class LockingArbiterLike[T <: Data](gen: T, n: Int, count: Int, needsLock: Option[T => Bool] = None) extends Module {
  require(isPow2(count))
  def grant: Seq[Bool]
  val io = new ArbiterIO(gen, n)
  val locked  = if(count > 1) Reg(init=Bool(false)) else Bool(false)
  val lockIdx = if(count > 1) Reg(init=UInt(n-1)) else UInt(n-1)
  val chosen = Wire(UInt(width = log2Up(n)))

  for ((g, i) <- grant.zipWithIndex)
    io.in(i).ready := Mux(locked, lockIdx === UInt(i), g) && io.out.ready
  io.out.valid := io.in(chosen).valid
  io.out.bits := io.in(chosen).bits
  io.chosen := chosen

  if(count > 1){
    val cnt = Reg(init=UInt(0, width = log2Up(count)))
    val cnt_next = cnt + UInt(1)
    when(io.out.fire()) {
      when(needsLock.map(_(io.out.bits)).getOrElse(Bool(true))) {
        cnt := cnt_next
        when(!locked) {
          locked := Bool(true)
          lockIdx := Vec(io.in.map{ in => in.fire()}).indexWhere{i: Bool => i}
        }
      }
      when(cnt_next === UInt(0)) {
        locked := Bool(false)
      }
    }
  }
}

class LockingRRArbiter[T <: Data](gen: T, n: Int, count: Int, needsLock: Option[T => Bool] = None) extends LockingArbiterLike[T](gen, n, count, needsLock) {
  lazy val last_grant = Reg(init=UInt(0, log2Up(n)))
  override def grant: Seq[Bool] = {
    val ctrl = ArbiterCtrl((0 until n).map(i => io.in(i).valid && UInt(i) > last_grant) ++ io.in.map(_.valid))
    (0 until n).map(i => ctrl(i) && UInt(i) > last_grant || ctrl(i + n))
  }

  var choose = UInt(n-1)
  for (i <- n-2 to 0 by -1)
    choose = Mux(io.in(i).valid, UInt(i), choose)
  for (i <- n-1 to 1 by -1)
    choose = Mux(io.in(i).valid && UInt(i) > last_grant, UInt(i), choose)
  chosen := Mux(locked, lockIdx, choose)

  when (io.out.fire()) { last_grant := chosen }
}

class LockingArbiter[T <: Data](gen: T, n: Int, count: Int, needsLock: Option[T => Bool] = None) extends LockingArbiterLike[T](gen, n, count, needsLock) {
  def grant: Seq[Bool] = ArbiterCtrl(io.in.map(_.valid))

  var choose = UInt(n-1)
  for (i <- n-2 to 0 by -1) {
    choose = Mux(io.in(i).valid, UInt(i), choose)
  }
  chosen := Mux(locked, lockIdx, choose)
}

/** Hardware module that is used to sequence n producers into 1 consumer.
  Producers are chosen in round robin order.

  Example usage:
    val arb = new RRArbiter(2, UInt())
    arb.io.in(0) <> producer0.io.out
    arb.io.in(1) <> producer1.io.out
    consumer.io.in <> arb.io.out
  */
class RRArbiter[T <: Data](gen:T, n: Int) extends LockingRRArbiter[T](gen, n, 1)

/** Hardware module that is used to sequence n producers into 1 consumer.
 Priority is given to lower producer

 Example usage:
   val arb = Module(new Arbiter(2, UInt()))
   arb.io.in(0) <> producer0.io.out
   arb.io.in(1) <> producer1.io.out
   consumer.io.in <> arb.io.out
 */
class Arbiter[T <: Data](gen: T, n: Int) extends LockingArbiter[T](gen, n, 1)


object FillInterleaved
{
  def apply(n: Int, in: Bits): UInt = apply(n, in.toBools)
  def apply(n: Int, in: Seq[Bool]): UInt = Vec(in.map(Fill(n, _))).toBits
}

class Counter(val n: Int) {
  val value = if (n == 1) UInt(0) else Reg(init=UInt(0, log2Up(n)))
  def inc(): Bool = {
    if (n == 1) Bool(true)
    else {
      val wrap = value === UInt(n-1)
      value := Mux(Bool(!isPow2(n)) && wrap, UInt(0), value + UInt(1))
      wrap
    }
  }
}

object Counter
{
  def apply(n: Int): Counter = new Counter(n)
  def apply(cond: Bool, n: Int): (UInt, Bool) = {
    val c = new Counter(n)
    var wrap: Bool = null
    when (cond) { wrap = c.inc() }
    (c.value, cond && wrap)
  }
}

class QueueIO[T <: Data](gen: T, entries: Int) extends Bundle
{
  val enq   = Decoupled(gen.cloneType).flip
  val deq   = Decoupled(gen.cloneType)
  val count = UInt(OUTPUT, log2Up(entries + 1))
}

class Queue[T <: Data](gen: T, val entries: Int, pipe: Boolean = false, flow: Boolean = false, _reset: Bool = null) extends Module(_reset=_reset)
{
  val io = new QueueIO(gen, entries)

  val ram = Mem(gen, entries)
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = Reg(init=Bool(false))

  val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val maybe_flow = Bool(flow) && empty
  val do_flow = maybe_flow && io.deq.ready

  val do_enq = io.enq.ready && io.enq.valid && !do_flow
  val do_deq = io.deq.ready && io.deq.valid && !do_flow
  when (do_enq) {
    ram(enq_ptr.value) := io.enq.bits
    enq_ptr.inc()
  }
  when (do_deq) {
    deq_ptr.inc()
  }
  when (do_enq != do_deq) {
    maybe_full := do_enq
  }

  io.deq.valid := !empty || Bool(flow) && io.enq.valid
  io.enq.ready := !full || Bool(pipe) && io.deq.ready
  io.deq.bits := Mux(maybe_flow, io.enq.bits, ram(deq_ptr.value))

  val ptr_diff = enq_ptr.value - deq_ptr.value
  if (isPow2(entries)) {
    io.count := Cat(maybe_full && ptr_match, ptr_diff)
  } else {
    io.count := Mux(ptr_match, Mux(maybe_full, UInt(entries), UInt(0)), Mux(deq_ptr.value > enq_ptr.value, UInt(entries) + ptr_diff, ptr_diff))
  }
}

/** Generic hardware queue. Required parameter entries controls
  the depth of the queues. The width of the queue is determined
  from the inputs.

  Example usage:
    val q = new Queue(UInt(), 16)
    q.io.enq <> producer.io.out
    consumer.io.in <> q.io.deq
  */
object Queue
{
  def apply[T <: Data](enq: DecoupledIO[T], entries: Int = 2, pipe: Boolean = false): DecoupledIO[T]  = {
    val q = Module(new Queue(enq.bits.cloneType, entries, pipe))
    q.io.enq.valid := enq.valid // not using <> so that override is allowed
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    q.io.deq
  }
}

class AsyncFifo[T<:Data](gen: T, entries: Int, enq_clk: Clock, deq_clk: Clock) extends Module {
  val io = new QueueIO(gen, entries)
  val asize = log2Up(entries)

  val s1_rptr_gray = Reg(init=UInt(0, asize+1), clock=enq_clk)
  val s2_rptr_gray = Reg(init=UInt(0, asize+1), clock=enq_clk)
  val s1_rst_deq = Reg(init=Bool(false), clock=enq_clk)
  val s2_rst_deq = Reg(init=Bool(false), clock=enq_clk)

  val s1_wptr_gray = Reg(init=UInt(0, asize+1), clock=deq_clk)
  val s2_wptr_gray = Reg(init=UInt(0, asize+1), clock=deq_clk)
  val s1_rst_enq = Reg(init=Bool(false), clock=deq_clk)
  val s2_rst_enq = Reg(init=Bool(false), clock=deq_clk)

  val wptr_bin = Reg(init=UInt(0, asize+1), clock=enq_clk)
  val wptr_gray = Reg(init=UInt(0, asize+1), clock=enq_clk)
  val not_full = Reg(init=Bool(false), clock=enq_clk)

  val wptr_bin_next = wptr_bin + (io.enq.valid & not_full)
  val wptr_gray_next = (wptr_bin_next >> UInt(1)) ^ wptr_bin_next
  val not_full_next = !(wptr_gray_next === Cat(~s2_rptr_gray(asize,asize-1), s2_rptr_gray(asize-2,0)))

  val rptr_bin = Reg(init=UInt(0, asize+1), clock=deq_clk)
  val rptr_gray = Reg(init=UInt(0, asize+1), clock=deq_clk)
  val not_empty = Reg(init=Bool(false), clock=deq_clk)

  val rptr_bin_next = rptr_bin + (io.deq.ready & not_empty)
  val rptr_gray_next = (rptr_bin_next >> UInt(1)) ^ rptr_bin_next
  val not_empty_next = !(rptr_gray_next === s2_wptr_gray)

  s2_rptr_gray := s1_rptr_gray; s1_rptr_gray := rptr_gray
  s2_rst_deq := s1_rst_deq; s1_rst_deq := enq_clk.getReset
  s2_wptr_gray := s1_wptr_gray; s1_wptr_gray := wptr_gray
  s2_rst_enq := s1_rst_enq; s1_rst_enq := deq_clk.getReset

  wptr_bin := wptr_bin_next
  wptr_gray := wptr_gray_next
  not_full := not_full_next && !s2_rst_deq

  rptr_bin := rptr_bin_next
  rptr_gray := rptr_gray_next
  not_empty := not_empty_next && !s2_rst_enq

  io.enq.ready := not_full
  io.deq.valid := not_empty

  val mem = Mem(gen, entries, clock=enq_clk)
  when (io.enq.valid && io.enq.ready) {
    mem(wptr_bin(asize-1,0)) := io.enq.bits
  }
  io.deq.bits := mem(rptr_bin(asize-1,0))
}

class Pipe[T <: Data](gen: T, latency: Int = 1) extends Module
{
  val io = new Bundle {
    val enq = Valid(gen).flip
    val deq = Valid(gen)
  }

  io.deq <> Pipe(io.enq, latency)
}

/** A hardware module that delays data coming down the pipeline
  by the number of cycles set by the latency parameter. Functionality
  is similar to ShiftRegister but this exposes a Pipe interface.

  Example usage:
    val pipe = new Pipe(UInt())
    pipe.io.enq <> produce.io.out
    consumer.io.in <> pipe.io.deq
  */
object Pipe
{
  def apply[T <: Data](enqValid: Bool, enqBits: T, latency: Int): ValidIO[T] = {
    if (latency == 0) {
      val out = Valid(enqBits)
      out.valid <> enqValid
      out.bits <> enqBits
      out.setIsTypeNode
      out
    } else {
      val v = Reg(Bool(), next=enqValid, init=Bool(false))
      val b = RegEnable(enqBits, enqValid)
      apply(v, b, latency-1)
    }
  }
  def apply[T <: Data](enqValid: Bool, enqBits: T): ValidIO[T] = apply(enqValid, enqBits, 1)
  def apply[T <: Data](enq: ValidIO[T], latency: Int = 1): ValidIO[T] = apply(enq.valid, enq.bits, latency)
}

/** Builds a Mux tree under the assumption that multiple select signals
  can be enabled. Priority is given to the first select signal.

  Returns the output of the Mux tree.
  */
object PriorityMux
{
  def apply[T <: Bits](in: Iterable[(Bool, T)]): T = {
    if (in.size == 1) {
      in.head._2
    } else {
      Mux(in.head._1, in.head._2, apply(in.tail))
    }
  }
  def apply[T <: Bits](sel: Iterable[Bool], in: Iterable[T]): T = apply(sel zip in)
  def apply[T <: Bits](sel: Bits, in: Iterable[T]): T = apply((0 until in.size).map(sel(_)), in)
}

/** Returns a bit vector in which only the least-significant 1 bit in
  the input vector, if any, is set.
  */
object PriorityEncoderOH
{
  private def encode(in: Seq[Bool]): UInt = {
    val outs = Vec.tabulate(in.size)(i => UInt(BigInt(1) << i, in.size))
    PriorityMux(in :+ Bool(true), outs :+ UInt(0, in.size))
  }
  def apply(in: Seq[Bool]): Vec[Bool] = {
    val enc = encode(in)
    Vec.tabulate(in.size)(enc(_))
  }
  def apply(in: Bits): UInt = encode((0 until in.getWidth).map(i => in(i)))
}

/** Chisel3 - Wrap a Chisel data type with a `Wire`.
  *
  * This sets the isWired state. It will be required for Chisel 3.0
  * The logic is:
  *  - for each element in a module:
  *    - is that element assigned to?
  *    - is that element defined with only a type (no compute logic)?
  *    If so, the element's definition must be wrapped in a Wire.
  */
object Wire
{
  def apply[T <: Data](t: T = null, init: T = null): T = {
    val mType = if (t == null) init else t
    val res = if(mType == null) {
      ChiselError.error("cannot infer type of Init.")
      UInt().asInstanceOf[T]
    } else {
      if (t != null && !t.isTypeOnly) {
        ChiselError.error("Wire() must not wrap a node with data %s".format(t))
      }
      if (init != null) {
        val x = mType.cloneType
        // Should this be part of 'cloneType'
        // x.component = mType.component
        x := init
        x
      } else {
        t.cloneType
      }
    }
    res.setIsWired(true)
    res.asDirectionless
  }
}
