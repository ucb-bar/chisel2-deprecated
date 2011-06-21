package Chisel {

import scala.collection.mutable.ArrayBuffer

object BundleVec {
  def apply[T <: Interface](count: Int)(bundleGen: => T): BundleVec[T] = {
    val res = new BundleVec[T]();
    for(i <- 0 until count){
      val t = bundleGen;
      res += t;
      t match {
	case b: Bundle => b.elements.map{case(n, io) => io.name += i};
	case io: IO => io.name += i;
      }
    }
    res
  }

}

class BundleVec[T <: Interface]() extends Interface {
  val bundleVector = new ArrayBuffer[T];
  def +=(b: T) = bundleVector += b;
  override def apply(ind: Int): T = {
    bundleVector(ind)
  };
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
  override def findNodes(depth: Int, c: Component): Unit = {
    if(c == null) println("THIS IS A NULL COMPONENT");
    for(bundle <- bundleVector)
      bundle.findNodes(depth, c);
  }

}

}
