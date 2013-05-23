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
import Component._
import ChiselError._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.BufferProxy
import scala.collection.mutable.Stack
import scala.math._
import Vec._
import Node._

object VecUFixToOH
{
  def apply(in: UFix, width: Int): UFix =
  {
    if(chiselOneHotMap.contains((in, width))) {
      chiselOneHotMap((in, width))
    } else {
      val out = UFix(1, width)
      val res = (out << in)(width-1,0)
      chiselOneHotMap += ((in, width) -> res)
      res
    }
  }
}

object VecMux {
  def apply(addr: UFix, elts: Seq[Data]): Data = {
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

object VecBuf{
  def apply[T <: Data](n: Int)(gen: => Vec[T]): ArrayBuffer[Vec[T]] = {
    val res = new ArrayBuffer[Vec[T]]
    for(i <- 0 until n)
      res += gen
    res
  }
}

object Vec {
  def apply[T <: Data](n: Int)(gen: => T): Vec[T] = {
    val res = new Vec[T](() => gen);
    for(i <- 0 until n){
      val t   = gen;
      res    += t;
    }
    res
  }

  def apply[T <: Data](elts: Seq[T])(gen: => T): Vec[T] = {
    val res = if (elts.forall(_.litOf != null) && gen.getWidth > 0) {
      new ROM(elts.map(_.litOf), () => gen)
    } else {
      new Vec[T](() => gen)
    }
    elts.zipWithIndex.foreach{ case (e,i) =>
      res += e
    }
    res
  }

  def apply[T <: Data](elt0: T, elts: T*)(gen: => T): Vec[T] =
    apply(elt0 +: elts.toSeq)(gen)

  def getEnable(onehot: UFix, i: Int): Bool = {
    var enable: Bool = null
      if(chiselOneHotBitMap.contains(onehot, i)){
        enable = chiselOneHotBitMap(onehot, i)
      } else {
        enable = onehot(i)
        chiselOneHotBitMap += ((onehot, i) -> enable)
      }
    enable
  }
}

class VecProc extends proc {
  var addr: UFix = null
  var elms: ArrayBuffer[Bits] = null

  override def genMuxes(default: Node) {}

  def procAssign(src: Node) = {
    val onehot = VecUFixToOH(addr, elms.length)
    searchAndMap = true
    for(i <- 0 until elms.length){
      when (getEnable(onehot, i)) {
        if(elms(i).comp != null) {
          elms(i).comp procAssign src
        } else {
          elms(i) procAssign src
        }
      }
    }
    searchAndMap = false
  }
}

class Vec[T <: Data](val gen: () => T) extends CompositeData with Cloneable with BufferProxy[T] {
  val self = new ArrayBuffer[T]
  val readPortCache = new HashMap[UFix, T]
  var sortedElementsCache: ArrayBuffer[ArrayBuffer[Data]] = null
  var flattenedVec: Node = null
  override def apply(idx: Int): T = {
    super.apply(idx)
  };

  def sortedElements: ArrayBuffer[ArrayBuffer[Data]] = {
    if (sortedElementsCache == null) {
      sortedElementsCache = new ArrayBuffer[ArrayBuffer[Data]]

      // create buckets for each elm in data type
      for(i <- 0 until this(0).flatten.length)
        sortedElementsCache += new ArrayBuffer[Data]

      // fill out buckets
      for(elm <- this) {
        for(((n, io), i) <- elm.flatten zip elm.flatten.indices) {
          //val bits = io.toBits
          //bits.comp = io.comp
          sortedElementsCache(i) += io.asInstanceOf[Data]
        }
      }
    }
    sortedElementsCache
  }

  def apply(ind: UFix): T =
    read(ind)

  def write(addr: UFix, data: T) {
    if(data.isInstanceOf[Node]){

      val onehot = VecUFixToOH(addr, length)
      searchAndMap = true
      for(i <- 0 until length){
        when (getEnable(onehot, i)) {
          this(i).comp procAssign data.toNode
        }
      }
      searchAndMap = false
    }
  }

  def read(addr: UFix): T = {
    if(readPortCache.contains(addr)) {
      return readPortCache(addr)
    }

    val res = this(0).clone
    val iaddr = UFix(width=log2Up(length))
    iaddr.inputs += addr
    for(((n, io), sortedElm) <- res.flatten zip sortedElements) {
      io assign VecMux(iaddr, sortedElm)

      // setup the comp for writes
      val io_comp = new VecProc()
      io_comp.addr = iaddr
      io_comp.elms = sortedElm.asInstanceOf[ArrayBuffer[Bits]] // XXX ?
      io.comp = io_comp
    }
    readPortCache += (addr -> res)
    res.setIsTypeNode
    res
  }

  override def flatten: Array[(String, Bits)] = {
    val res = new ArrayBuffer[(String, Bits)]
    for (elm <- self)
      res ++= elm.flatten
    res.toArray
  }

  // override def getWidth(): Int = {
  //   var w = 0
  //   for ((name, io) <- this.flatten)
  //     w += io.getWidth
  //   println(w)
  //   w
  // }

  override def <>(src: Node) = {
    src match {
      case other: Vec[T] => {
        for((b, o) <- self zip other.self)
          b <> o
      }
    }
  }

  override def ^^(src: Node) = {
    src match {
      case other: Vec[T] =>
        for((b, o) <- self zip other.self)
          b ^^ o
    }
  }

  def <>(src: Vec[T]) = {
    for((b, e) <- self zip src)
      b <> e;
  }

  def <>(src: Iterable[T]) = {
    for((b, e) <- self zip src)
      b <> e;
  }

  def :=[T <: Data](src: Iterable[T]): Unit = {

    // Check matching size
    assert(this.size == src.size, {
      ChiselError.error("Can't wire together Vecs of mismatched lengths")
    })

    // Check LHS to make sure unidirection
    val dirLHS = this.flatten(0)._2.dir
    this.flatten.map(x => {assert(x._2.dir == dirLHS, {
      ChiselError.error("Cannot mix directions on left hand side of :=")
    })
    })

    // Check RHS to make sure unidirection
    val dirRHS = src.head.flatten(0)._2.dir
    for (elm <- src) {
      elm.flatten.map(x => {assert(x._2.dir == dirRHS, {
        ChiselError.error("Cannot mix directions on right hand side of :=")
      })
      })
    }

    for((me, other) <- this zip src){
      me match {
        case bundle: Bundle =>
          bundle := other.asInstanceOf[Bundle]
        case v: Vec[_] =>
          v := other.asInstanceOf[Vec[Data]]
        case _ =>
          me := other
      }
    }
  }

  def := (src: UFix) = {
    for(i <- 0 until length)
      this(i) := src(i)
  }

  override def removeTypeNodes() {
    for(bundle <- self)
      bundle.removeTypeNodes
  }

  override def traceableNodes = self.toArray

  override def traceNode(c: Component, stack: Stack[() => Any]) {
    for((n, i) <- flatten) {
      stack.push(() => i.traceNode(c, stack))
    }
    stack.push(() => super.traceNode(c, stack))
  }

  override def flip(): this.type = {
    for(b <- self)
      b.flip();
    this
  }

  override def nameIt (path: String) {
    if( !named
      && (name.isEmpty
        || (!path.isEmpty && name != path)) ) {
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
        elm.nameIt(prefix + i + suffix)
      }
    } else {
      /* We are trying to rename a Vec that has a fixed name. */
    }
  }

  override def clone(): this.type = {
    val res = Vec(size){ gen() };
    res.asInstanceOf[this.type]
  }

  override def toNode: Node = {
    if(flattenedVec == null){
      val nodes = flatten.map{case(n, i) => i};
      flattenedVec = Concatenate(nodes.head, nodes.tail.toList: _*)
    }
    flattenedVec
  }

  override def fromNode(n: Node): this.type = {
    val res = this.clone();
    var ind = 0;
    for((name, io) <- res.flatten.toList.reverse) {
      io.asOutput();
      if(io.width > 1) {
        io assign NodeExtract(n, ind + io.width-1, ind)
      } else {
        io assign NodeExtract(n, ind);
      }
      ind += io.width;
    }
    res
  }

  override def asDirectionless(): this.type = {
    self.foreach(_.asDirectionless)
    this
  }

  override def asOutput(): this.type = {
    self.foreach(_.asOutput)
    this
  }

  override def asInput(): this.type = {
    self.foreach(_.asInput)
    this
  }

  override def setIsTypeNode() {
    isTypeNode = true;
    for(elm <- self)
      elm.setIsTypeNode
  }

  override def toBits(): UFix = {
    val reversed = this.reverse.map(_.toBits)
    Cat(reversed.head, reversed.tail: _*)
  }

  def forall(p: T => Bool): Bool = (this map p).fold(Bool(true))(_&&_)
  def exists(p: T => Bool): Bool = (this map p).fold(Bool(false))(_||_)
  def contains[T <: Bits](x: T): Bool = this.exists(_ === x)
  def count(p: T => Bool): UFix = PopCount(this map p)

  private def indexWhereHelper(p: T => Bool) = this map p zip (0 until size).map(i => UFix(i))
  def indexWhere(p: T => Bool): UFix = PriorityMux(indexWhereHelper(p))
  def lastIndexWhere(p: T => Bool): UFix = PriorityMux(indexWhereHelper(p).reverse)
}
