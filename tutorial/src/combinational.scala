package Tutorial {

import Chisel._
import scala.collection.mutable.HashMap
import scala.util.Random

class Combinational extends Component {
  val io = new Bundle {
    val x   = UFix(16, INPUT)
    val y   = UFix(16, INPUT)
    val z   = UFix(16, OUTPUT)
  }
  io.z := io.x + io.y

  defTests(io) {
    var allGood = true
    val vars   = new HashMap[Node, Node]()
    val rnd    = new Random()
    val maxInt = 1 << 16
    for (i <- 0 until 10) {
      vars.clear()
      val x = rnd.nextInt(maxInt)
      val y = rnd.nextInt(maxInt)
      vars(io.x) = UFix(x)
      vars(io.y) = UFix(y)
      vars(io.z) = UFix((x + y)&(maxInt-1))
      allGood = test(vars) && allGood
    }
    allGood
  }
}

}
