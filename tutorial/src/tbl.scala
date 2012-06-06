package Tutorial {

import Chisel._
import scala.collection.mutable.HashMap
import util.Random

class Tbl extends Component {
  val io = new Bundle {
    val addr = UFix(8, INPUT)
    val out  = UFix(8, OUTPUT)
  }
  val r = ROM(Range(0, 256).map(UFix(_))){ UFix(width = 8) }
  io.out := r(io.addr)

  defTests(io) {
    var allGood = true
    val vars    = new HashMap[Node, Node]()
    val rnd     = new Random()
    for (t <- 0 until 16) {
      vars.clear()
      val addr      = rnd.nextInt(256)
      vars(io.addr) = UFix(addr)
      vars(io.out)  = UFix(addr)
      allGood       = test(vars) && allGood
    }
    allGood
  }
}

}
