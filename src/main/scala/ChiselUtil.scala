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
import scala.math._
import Literal._

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

object foldR
{
  def apply[T <: Bits](x: Seq[T])(f: (T, T) => T): T =
    if (x.length == 1) x(0) else f(x(0), foldR(x.slice(1, x.length))(f))
}

object LFSR16
{
  def apply(increment: Bool = Bool(true)) =
  {
    val width = 16
    val lfsr = RegReset(UFix(1, width))
    when (increment) { lfsr := Cat(lfsr(0)^lfsr(2)^lfsr(3)^lfsr(5), lfsr(width-1,1)) }
    lfsr
  }
}

/** Counts the number of 1s in a sequence of *Bool*.
*/
object PopCount
{
  def apply(in: Seq[Bool]): UFix = {
    if (in.size == 0) {
      UFix(0)
    } else if (in.size == 1) {
      in(0)
    } else {
      apply(in.slice(0, in.size/2)) + Cat(UFix(0), apply(in.slice(in.size/2, in.size)))
    }
  }
  def apply(in: Bits): UFix = apply((0 until in.getWidth).map(in(_)))
}

/** Litte/big bit endian convertion: reverse the order of the bits in a UFix.
*/
object Reverse
{
  def doit(in: UFix, base: Int, length: Int): UFix =
  {
    val half = (1 << log2Up(length))/2
    if (length == 1) {
      in(base)
    } else {
      Cat(doit(in, base, half), doit(in, base + half, length - half))
    }
  }
  def apply(in: UFix) = doit(in, 0, in.getWidth)
}


object ShiftRegister
{
  def apply[T <: Data](n: Int, in: T, en: Bool = Bool(true)): T =
  {
    if (n == 1)
    {
      val res = Reg(in)
      when (en)
      {
        res := in
      }
      res
    }
    else
    {
      RegUpdate(apply(n-1, in, en))
    }
  }
}

object UFixToOH
{
  def apply(in: UFix, width: Int = -1): UFix =
  {
    if (width == -1) {
      UFix(1) << in
    } else {
      UFix(1) << in(log2Up(width)-1,0)
    }
  }
}

object Mux1H
{
  def apply[T<: Bits](sel: Seq[Bool], in: Seq[T]): T = {
    if (in.size == 1) {
      in(0)
    } else {
      val zero: T = in(0).fromInt(0)
      sel.zip(in).map { case(s,x) => Mux(s, x, zero) }.reduce(_|_)
    }
  }
  def apply[T <: Bits](sel: Bits, in: Seq[T]): T = apply((0 until in.size).map(sel(_)), in)
}


object OHToUFix
{
  def apply(in: Seq[Bool]): UFix = {
    if (in.size <= 1) return UFix(0)
    if (in.size == 2) return in(1)
    val hi = in.slice(in.size/2, in.size)
    val lo = in.slice(0, in.size/2)
    Cat(hi.reduceLeft((s1, s2) => {s1 || s2}),
      apply(hi zip lo map { case (x, y) => (x || y) }))
  }
  def apply(in: Bits): UFix = apply((0 until in.getWidth).map(in(_)))
}


class PipeIO[+T <: Data](gen: T) extends Bundle
{
  val valid = Bool(OUTPUT)
  val bits = gen.clone.asOutput
  def fire(dummy: Int = 0) = valid
  override def clone =
    try {
      super.clone()
    } catch {
      case e: java.lang.Exception => {
        new PipeIO(gen).asInstanceOf[this.type]
      }
    }
}

class FIFOIO[T <: Data](gen: T) extends Bundle
{
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val bits  = gen.clone.asOutput
  def fire(dummy: Int = 0) = ready && valid
  override def clone =
    try {
      super.clone()
    } catch {
      case e: java.lang.Exception => {
        new FIFOIO(gen).asInstanceOf[this.type]
      }
    }
}

object FIFOIO {
  def apply[T <: Data](gen: T) = {new FIFOIO(gen)}
}

class EnqIO[T <: Data](gen: T) extends FIFOIO(gen)
{
  def enq(dat: T): T = { valid := Bool(true); bits := dat; dat }
  valid := Bool(false);
  for (io <- bits.flatten.map(x => x._2))
    io := UFix(0)
  override def clone = { new EnqIO(gen).asInstanceOf[this.type]; }
}

class DeqIO[T <: Data](gen: T) extends FIFOIO(gen)
{
  flip()
  ready := Bool(false);
  def deq(b: Boolean = false): T = { ready := Bool(true); bits }
  override def clone = { new DeqIO(gen).asInstanceOf[this.type]; }
}


class FIFOIOC[+T <: Data](gen: T) extends Bundle
{
  val ready = Bool(INPUT)
  val valid = Bool(OUTPUT)
  val bits  = gen.clone.asOutput
}


class ioArbiter[T <: Data](gen: T, n: Int) extends Bundle {
  val in  = Vec.fill(n){ new FIFOIO(gen) }.flip
  val out = new FIFOIO(gen)
  val chosen = UFix(OUTPUT, log2Up(n))
}

object ArbiterCtrl
{
  def apply(request: Seq[Bool]) = {
    Bool(true) +: (1 until request.length).map(i => !request.slice(0, i).foldLeft(Bool(false))(_ || _))
  }
}

abstract class LockingArbiterLike[T <: Data](gen: T, n: Int, count: Int, needsLock: Option[T => Bool] = None) extends Mod {
  require(isPow2(count))
  val io = new ioArbiter(gen, n)
  val locked  = if(count > 1) RegReset(Bool(false)) else Bool(false)
  val lockIdx = if(count > 1) RegReset(UFix(n-1)) else UFix(n-1)
  val grant = List.fill(n)(Bool())
  val chosen = UFix(width = log2Up(n))

  (0 until n).map(i => io.in(i).ready := Mux(locked, lockIdx === UFix(i), grant(i)) && io.out.ready)
  io.out.valid := io.in(chosen).valid
  io.out.bits := io.in(chosen).bits
  io.chosen := chosen

  if(count > 1){
    val cnt = RegReset(UFix(0, width = log2Up(count)))
    val cnt_next = cnt + UFix(1)
    when(io.out.fire()) {
      when(needsLock.map(_(io.out.bits)).getOrElse(Bool(true))) {
        cnt := cnt_next
        when(!locked) {
          locked := Bool(true)
          lockIdx := Vec(io.in.map{ in => in.fire()}).indexWhere{i: Bool => i}
        }
      }
      when(cnt_next === UFix(0)) {
        locked := Bool(false)
      }
    }
  }
}

class LockingRRArbiter[T <: Bits](gen: T, n: Int, count: Int, needsLock: Option[T => Bool] = None) extends LockingArbiterLike[T](gen, n, count, needsLock) {
  val last_grant = RegReset(UFix(0, log2Up(n)))
  val ctrl = ArbiterCtrl((0 until n).map(i => io.in(i).valid && UFix(i) > last_grant) ++ io.in.map(_.valid))
  (0 until n).map(i => grant(i) := ctrl(i) && UFix(i) > last_grant || ctrl(i + n))

  var choose = UFix(n-1)
  for (i <- n-2 to 0 by -1)
    choose = Mux(io.in(i).valid, UFix(i), choose)
  for (i <- n-1 to 1 by -1)
    choose = Mux(io.in(i).valid && UFix(i) > last_grant, UFix(i), choose)
  chosen := Mux(locked, lockIdx, choose)

  when (io.out.fire()) { last_grant := chosen }
}

class LockingArbiter[T <: Bits](gen: T, n: Int, count: Int, needsLock: Option[T => Bool] = None) extends LockingArbiterLike[T](gen, n, count, needsLock) {
  val ctrl = ArbiterCtrl(io.in.map(_.valid))
  grant zip ctrl map { case(g, c) => g := c }

  var choose = UFix(n-1)
  for (i <- n-2 to 0 by -1) {
    choose = Mux(io.in(i).valid, UFix(i), choose)
  }
  chosen := Mux(locked, lockIdx, choose)
}

class RRArbiter[T <: Bits](gen:T, n: Int) extends LockingRRArbiter[T](gen, n, 1)

class Arbiter[T <: Bits](gen: T, n: Int) extends LockingArbiter[T](gen, n, 1)


object FillInterleaved
{
  def apply(n: Int, in: Bits): UFix =
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
    val c = RegReset(UFix(0, log2Up(n)))
    val wrap = c === UFix(n-1)
    when (cond) {
      c := Mux(Bool(!isPow2(n)) && wrap, UFix(0), c + UFix(1))
    }
    (c, wrap && cond)
  }
}

class ioQueue[T <: Bits](gen: T, entries: Int) extends Bundle
{
  val enq   = new FIFOIO(gen.clone).flip
  val deq   = new FIFOIO(gen.clone)
  val count = UFix(OUTPUT, log2Up(entries + 1))
}

class Queue[T <: Bits](gen: T, val entries: Int, pipe: Boolean = false, flow: Boolean = false, resetSignal: Bool = null) extends Mod(resetSignal)
{
  val io = new ioQueue(gen, entries)

  val do_flow = Bool()
  val do_enq = io.enq.ready && io.enq.valid && !do_flow
  val do_deq = io.deq.ready && io.deq.valid && !do_flow

  var enq_ptr = UFix(0)
  var deq_ptr = UFix(0)

  if (entries > 1) {
    enq_ptr = Counter(do_enq, entries)._1
    deq_ptr = Counter(do_deq, entries)._1
  }

  val maybe_full = RegReset(Bool(false))
  when (do_enq != do_deq) {
    maybe_full := do_enq
  }

  val ram = Mem(entries, gen)
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
  if (isPow2(entries)) {
    io.count := Cat(maybe_full && ptr_match, ptr_diff)
  } else {
    io.count := Mux(ptr_match, Mux(maybe_full, UFix(entries), UFix(0)), Mux(deq_ptr > enq_ptr, UFix(entries) + ptr_diff, ptr_diff))
  }
}

object Queue
{
  def apply[T <: Bits](enq: FIFOIO[T], entries: Int = 2, pipe: Boolean = false) = {
    val q = new Queue(enq.bits.clone, entries, pipe)
    q.io.enq.valid := enq.valid // not using <> so that override is allowed
    q.io.enq.bits := enq.bits
    enq.ready := q.io.enq.ready
    q.io.deq
  }
}

object Log2 {
  def apply (mod: Bits, n: Int): UFix = {
    Mod.backend match {
      case x: CppBackend => {
        val log2 = new Log2()
        log2.init("", fixWidth(sizeof(n-1)), mod)
        UFix().fromNode(log2)
      }
      case _ => {
        var res = UFix(0);
        for (i <- 1 until n)
          res = Mux(mod(i), UFix(i, sizeof(n-1)), res);
        res
      }
    }
  }
}

class Log2 extends Node {
  override def toString: String = "LOG2(" + inputs(0) + ")";
}

class Pipe[T <: Bits](gen: T, latency: Int = 1) extends Mod
{
  val io = new Bundle {
    val enq = new PipeIO(gen).flip
    val deq = new PipeIO(gen)
  }

  io.deq <> Pipe(io.enq, latency)
}

object Pipe
{
  def apply[T <: Bits](enqValid: Bool, enqBits: T, latency: Int): PipeIO[T] = {
    if (latency == 0) {
      val out = new PipeIO(enqBits.clone)
      out.valid <> enqValid
      out.bits <> enqBits
      out.setIsTypeNode
      out
    } else {
      val v = Reg(Bool(), updateVal=enqValid, resetVal=Bool(false))
      val b = Reg(enqBits)
      when (enqValid) { b := enqBits }
      apply(v, b, latency-1)
    }
  }
  def apply[T <: Bits](enqValid: Bool, enqBits: T): PipeIO[T] = apply(enqValid, enqBits, 1)
  def apply[T <: Bits](enq: PipeIO[T], latency: Int = 1): PipeIO[T] = apply(enq.valid, enq.bits, latency)
}

object PriorityMux
{
  def apply[T <: Bits](in: Seq[(Bool, T)]): T = {
    if (in.size == 1) {
      in.head._2
    } else {
      Mux(in.head._1, in.head._2, apply(in.tail))
    }
  }
  def apply[T <: Bits](sel: Seq[Bool], in: Seq[T]): T = apply(sel zip in)
  def apply[T <: Bits](sel: Bits, in: Seq[T]): T = apply((0 until in.size).map(sel(_)), in)
}

object PriorityEncoder
{
  def apply(in: Seq[Bool]): UFix = PriorityMux(in, (0 until in.size).map(UFix(_)))
  def apply(in: Bits): UFix = apply((0 until in.getWidth).map(in(_)))
}

object PriorityEncoderOH
{
  def apply(in: Bits): UFix = Vec(apply((0 until in.getWidth).map(in(_)))).toBits
  def apply(in: Seq[Bool]): Seq[UFix] = {
    var none_hot = Bool(true)
    val out = collection.mutable.ArrayBuffer[UFix]()
    for (i <- 0 until in.size) {
      out += (none_hot && in(i))
      none_hot = none_hot && !in(i)
    }
    out
  }
}


