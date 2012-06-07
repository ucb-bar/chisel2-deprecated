package Tutorial {

import Chisel._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Mul extends Component {
  val io = new Bundle {
    val x   = UFix(4, INPUT)
    val y   = UFix(4, INPUT)
    val z   = UFix(8, OUTPUT)
  }
  val muls = new ArrayBuffer[UFix]()
  for (i <- 0 until 16)
    for (j <- 0 until 16)
      muls += UFix(i * j)
  val tbl = Vec(muls){ UFix(width = 8) }
  io.z := tbl((io.x << UFix(4)) | io.y)

  defTests(io) {
    var allGood = true
    val vars    = new HashMap[Node, Node]()
    val rnd     = new Random()
    val maxInt  = 1 << 4
    for (i <- 0 until 10) {
      val x = rnd.nextInt(maxInt)
      val y = rnd.nextInt(maxInt)
      vars(io.x) = UFix(x)
      vars(io.y) = UFix(y)
      vars(io.z) = UFix(x * y)
      allGood = test(vars) && allGood
    }
    allGood
  }
}

}
