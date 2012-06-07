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

  // fill in to form 4x4 bit multiplication lookup table

  io.z := UFix(0)

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
