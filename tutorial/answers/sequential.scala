package Tutorial {

import Chisel._
import scala.collection.mutable.HashMap
import util.Random

class Sequential extends Component {
  val io = new Bundle {
    val in  = Bool(INPUT)
    val out = UFix(8, OUTPUT)
  }
  val c = Reg(resetVal = UFix(0, 8))
  when (io.in) {
    c := c + UFix(1)
  }
  io.out := c
}

}
