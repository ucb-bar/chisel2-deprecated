package Chisel {

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.BufferProxy;
import scala.math._;

object log2up
{
  def apply(in: Int) = if (in == 1) 1 else ceil(log(in)/log(2)).toInt
}

object UFixToOH
{
  def apply(in: UFix, width: Int): Bits =
  {
    val out = Bits(1, width)
    (out << in)(width-1,0)
  }
}

class Mux1H_(n: Int, w: Int) extends Component
{
  val io = new Bundle {
    val sel = Vec(n) { Bool(dir = INPUT) }
    val in = Vec(n) { Bits(width = w, dir = INPUT) }
    val out = Bits(width = w, dir = OUTPUT)
  }

  if (n > 1) {
    var out = io.in(0) & Fill(w, io.sel(0))
    for (i <- 1 to n-1)
      out = out | (io.in(i) & Fill(w, io.sel(i)))
    io.out := out
  } else {
    io.out := io.in(0)
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
    val res = new Vec[T]();
    res.gen = () => gen;
    println("MAKE VEC");
    for(i <- 0 until n){
      val t   = gen;
      res    += t;
      t.name += i;
    }
    res.eltWidth = res(0).getWidth
    res
  }
}

class Vec[T <: Data]() extends Data with Cloneable with BufferProxy[T] { 
  var eltWidth = 0;
  val self = new ArrayBuffer[T];
  var gen: () => T = () => self(0);
  var flattenedVec: Node = null;
  override def apply(idx: Int): T = {
    super.apply(idx)
  };
  
  def apply(ind: UFix): T = {
    var res = this(0);
    for(i <- 1 until length)
      res = Mux(UFix(i) === ind, self(i), res)
    res
  }

  def write(addr: UFix, data: T) = {
    if(data.isInstanceOf[Node]){

      val onehot = UFixToOH(addr, length)
      for(i <- 0 until length){
        when (onehot(i).toBool) {
          this(i).comp procAssign data.toNode
        }
      }
    }
  }

  def write(addr: Bits, data: T): Unit = {
    write(addr.toUFix, data)
  }

  def read(addr: UFix): T = {
    val mux1h = new Mux1H_(length, eltWidth)
    val onehot = UFixToOH(addr, length)
    for(i <- 0 until length){
      mux1h.io.sel(i) := onehot(i).toBool
      mux1h.io.in(i)  assign this(i)
    }
    val res = this(0).clone
    res.setIsCellIO
    res assign mux1h.io.out
    res
  }

  override def flatten: Array[(String, IO)] = {
    val res = new ArrayBuffer[(String, IO)];
    for (elm <- self)
      elm match {
	case bundle: Bundle => res ++= bundle.flatten;
	case io: IO         => res += ((io.name, io));
      }
    res.toArray
  }

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


  // TODO: CHECK FOR ALL OUT
  def :=[T <: Data](src: Vec[T]) = {
    for((src, dest) <- this zip src){
      src := dest
    }
  }

  def := (src: Bits) = {
    for(i <- 0 until length)
      this(i) := src(i)
  }

  override def traceableNodes = self.toArray

  override def removeCellIOs() = {
    for(bundle <- self)
      bundle.removeCellIOs
  }

  override def flip(): this.type = {
    for(b <- self)
      b.flip();
    this
  }

  override def name_it (path: String, named: Boolean = true) = {
    for (i <- self) {
      i.name = (if (path.length > 0) path + "_" else "") + i.name;
      i.name_it(i.name, named);
      // println("  ELT " + n + " " + i);
    }
  }

  override def clone(): this.type = {
    val res = Vec(size){ gen() };
    res.asInstanceOf[this.type]
  }

  override def toNode: Node = {
    if(flattenedVec == null){
      val nodes = flatten.map{case(n, i) => i};
      flattenedVec = Concatanate(nodes.head, nodes.tail.toList: _*)
    }
    flattenedVec
  }

  override def fromNode(n: Node): this.type = {
    val res = this.clone();
    var ind = 0;
    for((name, io) <- res.flatten.toList.reverse) {
      io.asOutput();
      if(io.width > 1)
	io assign NodeExtract(n, ind + io.width-1, ind)
      else
	io assign NodeExtract(n, ind);
      ind += io.width;
    }
    res
  }

  override def asOutput(): this.type = {
    for(elm <- self)
      elm.asOutput;
    this
  }

  override def asInput(): this.type = {
    for(elm <- self)
      elm.asInput
    this
  }

  override def setIsCellIO() = {
    isCellIO = true;
    for(elm <- self)
      elm.setIsCellIO
  }

  override def toBits(): Bits = {
    var res: Bits = null
    for(i <- 0 until length)
      res = Cat(this(i), res)
    res
  }
}

}
