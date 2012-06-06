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
    for (i <- 0 until n) {
      vars.clear()
      val k  = Bits(i, width = log2up(n)) 
      vars(io.sel) = k(0) 
      vars(io.in1) = k(1) 
      vars(io.in0) = k(2) 
      vars(io.out) = Mux(k(0), k(1), k(2)) 
      allGood = test(vars) && allGood
    }
    allGood
  }
}

}
