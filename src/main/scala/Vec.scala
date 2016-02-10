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

object VecMux {
  def apply(addr: UInt, elts: Seq[Data]): Data = {
    def doit(elts: Seq[Data], pos: Int): Data = {
      if (elts.length == 1) {
        elts(0)
      } else {
        val newElts = (0 until elts.length/2).map(i => Mux(addr(pos), elts(2*i + 1), elts(2*i)))
        doit(newElts ++ elts.slice(elts.length/2*2, elts.length), pos + 1)
      }
    }
    doit(elts, 0)
  }
}

object Vec {
  @deprecated("Vec(gen: => T, n:Int) is deprecated. Please use Vec(n:Int, gen: => T) instead.", "2.29")
  def apply[T <: Data](gen: => T, n: Int): Vec[T] = {
    if (Driver.minimumCompatibility > "2") {
      ChiselError.error("Vec(gen: => T, n:Int) is deprecated. Please use Vec(n:Int, gen: => T) instead.")
    }
    apply(n, gen)
  }

  /** Returns a new *Vec* from a sequence of *Data* nodes.
    */
  def apply[T <: Data](elts: Iterable[T]): Vec[T] = {
    val res =
      if (!elts.isEmpty && elts.forall(_.isLit)) ROM(elts)
      else new Vec[T](i => elts.head.cloneType, elts)
    res
  }

  /** Returns a new *Vec* from the concatenation of a *Data* node
    and a sequence of *Data* nodes.
    */
  def apply[T <: Data](elt0: T, elts: T*): Vec[T] = apply(elt0 +: elts.toSeq)

  /** Returns an array that contains the results of some element computation
    a number of times.

    Note that this means that elem is computed a total of n times.
    */
  def fill[T <: Data](n: Int)(gen: => T): Vec[T] = tabulate(n){ i => gen }

  /** Returns an array containing values of a given function over
    a range of integer values starting from 0.
    */
  def tabulate[T <: Data](n: Int)(gen: (Int) => T): Vec[T] =
    apply((0 until n).map(i => gen(i)))

  def tabulate[T <: Data](n1: Int, n2: Int)(f: (Int, Int) => T): Vec[Vec[T]] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))

  def apply[T <: Data](n: Int, gen: => T): Vec[T] = fill(n)(gen)
}

class VecProc(enables: Iterable[Bool], elms: Iterable[Data]) extends proc {
  override def procAssign(src: Node): Unit = {
    for ((en, elm) <- enables zip elms) when (en) {
      elm.comp match {
        case None => elm.asInstanceOf[Bits] procAssign src
        case Some(p) => p procAssign src
      }
    }
  }
}

class Vec[T <: Data](val gen: (Int) => T, elts: Iterable[T]) extends Aggregate with VecLike[T] with Cloneable {
  val self = elts.toVector
  if (self != null && !self.isEmpty && self(0).getNode.isInstanceOf[Reg] && Driver.minimumCompatibility > "2") {
    ChiselError.error("Vec(Reg) is deprecated. Please use Reg(Vec)")
  }
  val readPorts = new HashMap[UInt, T]
  override def apply(idx: Int): T = self(idx)

  // TODO: better way to generated this structure?
  lazy val sortedElements: Array[ArrayBuffer[Data]] = {
    // create buckets for each elm in data type
    val buckets = (for (i <- 0 until this(0).flatten.length) yield (new ArrayBuffer[Data])).toArray
    // fill out buckets
    for (elm <- this ; ((n, io), i) <- elm.flatten.zipWithIndex) buckets(i) += io

    buckets
  }

  def apply(ind: UInt): T =
    read(ind)

  def write(addr: UInt, data: T): Unit =
    read(addr) := data

  def read(addr: UInt): T = {
    if (readPorts contains addr) {
      readPorts(addr)
    } else {
      val iaddr = UInt(width = log2Up(length))
      iaddr.setIsWired(true)
      iaddr assign addr
      val enables = (UInt(1) << iaddr).toBools
      val res = this(0).cloneType
      for(((n, io), sortedElm) <- res.flatten zip sortedElements) {
        io assign VecMux(iaddr, sortedElm)
        // setup the comp for writes
        io.comp = Some(new VecProc(enables, sortedElm))
      }
      readPorts(addr) = res
      res.setIsTypeNode
      res.setIsWired(true)
      res
    }
  }

  override def flatten: Array[(String, Bits)] = {
    // Todo: why reverse?
    (self.zipWithIndex.reverse foldLeft Array[(String, Bits)]()){(res, x) =>
      val (elm, idx) = x
      res ++ (if (elm.name != "") elm.flatten else elm match {
        case b: Bits => Array((idx.toString, b))
        case _ => elm.flatten map (x => (idx.toString + "_" + x._1, x._2))
      })
    }
  }

  override def <>(src: Node) {
    src match {
      case other: Vec[T] => (self zip other.self) foreach {case (s, o) => s <> o}
      case _ =>
    }
  }
  def <>(src: Vec[T]) { (self zip src) foreach {case (s, o) => s <> o} }
  def <>(src: Iterable[T]) { (self zip src) foreach {case (s, o) => s <> o} }

  override protected def colonEquals[T <: Data](that: Iterable[T]): Unit = comp match {
    case Some(p) => p procAssign Vec(that)
    case None => {
      def unidirectional[U <: Data](who: Iterable[(String, Bits)]) =
        who.forall(_._2.dir == who.head._2.dir)

      assert(size == that.size,
        ChiselError.error("Can't wire together Vecs of mismatched lengths"))
      assert(unidirectional(flatten),
        ChiselError.error("Cannot mix directions on left hand side of :="))
      assert(unidirectional(that.flatMap(_.flatten)),
        ChiselError.error("Cannot mix directions on left hand side of :="))
      for ((me, other) <- this zip that)
        me := other
    }
  }

  override protected def colonEquals(that: Bits): Unit = {
    for (i <- 0 until length) this(i) := that(i)
  }
  // We need this special := because Iterable[T] is not a Data.
  def :=[T <: Data](that: Iterable[T]): Unit = colonEquals(that)
  override def removeTypeNodes { self foreach (_.removeTypeNodes) }
  override def flip: this.type = { self foreach (_.flip) ; this }

  override def nameIt (path: String, isNamingIo: Boolean) {
    if( !named && (name.isEmpty || (!path.isEmpty && name != path)) ) {
      val prevPrefix = if (name.length > 0) name + "_" else ""
      name = path
      val prefix = if (name.length > 0) name + "_" else ""
      for( (elm, i) <- self.zipWithIndex ) {
        val prevElmPrefix = prevPrefix + i
        val suffix = if( elm.name.startsWith(prevElmPrefix) ) {
          /* XXX Cludgy! We remove the previous prefix and regenerate
          the _elm_ name with a new prefix. */
          elm.name.substring(prevElmPrefix.length)
        } else {
          elm.name
        }
        elm.nameIt(prefix + i + suffix, isNamingIo)
      }
    } else {
      /* We are trying to rename a Vec that has a fixed name. */
    }
  }

  override def cloneType: this.type = Vec.tabulate(size)(gen).asInstanceOf[this.type] //Vec(this: Seq[T]).asInstanceOf[this.type]
  override def asDirectionless: this.type = { self.foreach(_.asDirectionless) ; this }
  override def asOutput: this.type = { self.foreach(_.asOutput) ; this }
  override def asInput: this.type = { self.foreach(_.asInput) ; this }
  override def setIsTypeNode { isTypeNode = true ; self foreach (_.setIsTypeNode) }

  def length: Int = self.size
  override val hashCode: Int = _id
  override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

  // Don't return 0 for getwidth - #247
  // Return the sum of our constituent widths.
  override def getWidth(): Int = self.map(_.getWidth).foldLeft(0)(_ + _)

  // Chisel3 - type-only nodes (no data - initialization or assignment) - used for verifying Wire() wrapping
  override def isTypeOnly: Boolean = { self forall (_.isTypeOnly) }
}

trait VecLike[T <: Data] extends collection.IndexedSeq[T] {
  def read(idx: UInt): T
  def write(idx: UInt, data: T): Unit
  def apply(idx: UInt): T

  def forall(p: T => Bool): Bool = (this map p).fold(Bool(true))(_&&_)
  def exists(p: T => Bool): Bool = (this map p).fold(Bool(false))(_||_)
  def contains(x: T) (implicit evidence: T <:< Bits): Bool = this.exists(_ === x)
  def count(p: T => Bool): UInt = PopCount((this map p).toSeq)

  private def indexWhereHelper(p: T => Bool) = this map p zip (0 until length).map(i => UInt(i))
  def indexWhere(p: T => Bool): UInt = PriorityMux(indexWhereHelper(p))
  def lastIndexWhere(p: T => Bool): UInt = PriorityMux(indexWhereHelper(p).reverse)
  def onlyIndexWhere(p: T => Bool): UInt = Mux1H(indexWhereHelper(p))
}
