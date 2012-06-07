package Tutorial {

import Chisel._
import scala.math._
import scala.collection.mutable.HashMap

class Mux2 extends Component {
  val io = new Bundle {
    val sel = Bits(width = 1, dir = INPUT)
    val in0 = Bits(width = 1, dir = INPUT)
    val in1 = Bits(width = 1, dir = INPUT)
    val out = Bits(width = 1, dir = OUTPUT)
  }
  io.out := (io.sel & io.in1) | (~io.sel & io.in0)
  
  defTests(io) {
    var allGood = true
    val n = pow(2, 3).toInt
    val vars = new HashMap[Node, Node]()
    for (s <- 0 until 2) {
      for (i0 <- 0 until 2) {
        for (i1 <- 0 until 2) {
          vars(io.sel) = Bits(s)
          vars(io.in1) = Bits(i1)
          vars(io.in0) = Bits(i0)
          vars(io.out) = Bits(if (s == 1) i1 else i0)
          allGood = test(vars) && allGood
        }
      }
    }
    allGood
  }
}

}
