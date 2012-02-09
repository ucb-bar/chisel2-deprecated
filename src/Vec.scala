package Chisel {

import scala.collection.mutable.ArrayBuffer

object Vec {
/*
  def apply[T <: Data](n: Int)(gen: => T): Vec[T] = {
    val res = new Vec[T]();
    for(i <- 0 until n){
      val t = gen;
      res += t;
      t.name += i;
    }
    res
  }
*/
  def apply[T <: Data](n: Int): (=> T) => Vec[T] = {
   gen(n, _);
  }

  def gen[T <: Data](n: Int, gen: => T): Vec[T] = {
    val res = new Vec[T]();
    res.size = n;
    res.gen = () => gen;
    for(i <- 0 until n){
      val t = gen;
      res += t;
      t.name += i;
    }
    res
  }
  
  def apply[T <: Data: Manifest](n: Int, fixed: T): Vec[T] = {
    val res = new Vec[T]();
    res.size = n;
    res.gen = () => fixed.clone;
    for(i <- 0 until n){
        res += fixed
    }
    res
  }

}

class Vec[T <: Data]() extends Data with Cloneable {
  var size = 0;
  var gen: () => T = () => bundleVector(0);
  var flattenedVec: Node = null;
  val bundleVector = new ArrayBuffer[T];
  def +=(b: T) = bundleVector += b;
  def apply(ind: Int): T = {
    bundleVector(ind)
  };
  def apply(ind: UFix): T = {
    var res = bundleVector(0);
    for(i <- 1 until bundleVector.length)
      res = Mux(UFix(i) === ind, bundleVector(i), res)
    res
  }
  override def flatten: Array[(String, IO)] = {
    val res = new ArrayBuffer[(String, IO)];
    for (elm <- bundleVector)
      elm match {
	case bundle: Bundle => res ++= bundle.flatten;
	case io: IO => res += ((io.name, io));
      }
    res.toArray
  }

  override def <>(src: Node) = {
    src match {
      case other: Vec[T] => {
	for((b, o) <- bundleVector zip other.bundleVector)
	  b <> o
      }
    }
  }

  override def ^^(src: Node) = {
    src match {
      case other: Vec[T] => 
	for((b, o) <- bundleVector zip other.bundleVector)
	  b ^^ o
    }
  }

  def <>(src: Iterable[T]) = {
    for((b, e) <- bundleVector zip src)
      b <> e;
  }

  override def findNodes(depth: Int, c: Component): Unit = {
    for(bundle <- bundleVector)
      bundle.findNodes(depth, c);
  }

  override def flip(): this.type = {
    for(b <- bundleVector)
      b.flip();
    this
  }

  override def name_it (path: String, named: Boolean = true) = {
    for (i <- bundleVector) {
      i.name = (if (path.length > 0) path + "_" else "") + i.name;
      i.name_it(i.name, named);
      // println("  ELT " + n + " " + i);
    }
  }

  override def clone(): this.type = {
    val res = Vec(size){gen()};
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
    for(elm <- bundleVector)
      elm.asOutput;
    this
  }

  override def asInput(): this.type = {
    for(elm <- bundleVector)
      elm.asInput
    this
  }

  override def setIsCellIO() = {
    isCellIO = true;
    for(elm <- bundleVector)
      elm.setIsCellIO
  }
}

}
