package Chisel {

import scala.collection.mutable.ArrayBuffer

object BundleVec {
/*
  def apply[T <: dat_t](n: Int)(gen: => T): BundleVec[T] = {
    val res = new BundleVec[T]();
    for(i <- 0 until n){
      val t = gen;
      res += t;
      t.name += i;
    }
    res
  }
*/
  def apply[T <: dat_t](n: Int): (=> T) => BundleVec[T] = {
   gen(n, _);
  }

  def gen[T <: dat_t](n: Int, gen: => T): BundleVec[T] = {
    val res = new BundleVec[T]();
    for(i <- 0 until n){
      val t = gen;
      res += t;
      t.name += i;
    }
    res
  }

  def apply[T <: dat_t: Manifest](n: Int, args: Any*): BundleVec[T] = {
    val res = new BundleVec[T]();
    for(i <- 0 until n) {
      val t = (if(args == List(None)) Fab[T]() else Fab[T](args: _*));
      res += t;
      t.name += i;
    }
    res
  }

}

class BundleVec[T <: dat_t]() extends dat_t {
  val bundleVector = new ArrayBuffer[T];
  def +=(b: T) = bundleVector += b;
  override def apply(ind: Int): T = {
    bundleVector(ind)
  };
  def apply(ind: Fix): T = {
    var res = bundleVector(0).clone;
    for(i <- 0 until bundleVector.length)
      res = Mux(Lit(i) === ind, bundleVector(i), res)
    res
  }
  override def flatten: Array[(String, IO)] = {
    val res = new ArrayBuffer[(String, IO)];
    for (elm <- bundleVector)
      elm match {
	case bundle: bundle_t => res ++= bundle.flatten;
	case io: IO => res += ((io.name, io));
      }
    res.toArray
  }

  override def <>(src: Node) = {
    src match {
      case other: BundleVec[T] => {
	for((b, o) <- bundleVector zip other.bundleVector)
	  b <> o
      }
    }
  }

  override def ^^(src: Node) = {
    src match {
      case other: BundleVec[T] => 
	for((b, o) <- bundleVector zip other.bundleVector)
	  b ^^ o
    }
  }

  def <>(src: List[Node]) = {
    for((b, e) <- bundleVector zip src)
      e match {
	case other: bundle_t =>
	  b <> e;
      }
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

}

}
