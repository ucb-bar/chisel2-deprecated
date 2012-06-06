package Tutorial {

import Chisel._
import scala.collection.mutable.HashMap

class GCDIO extends Bundle {
  val a = UFix(16, INPUT)
  val b = UFix(16, INPUT)
  val z = UFix(16, OUTPUT)
  val v = Bool(OUTPUT)
}

class GCD extends Component {
  val io = new GCDIO()
  val x  = Reg(resetVal = io.a)
  val y  = Reg(resetVal = io.b)
  when   (x > y) { x := x - y } 
  unless (x > y) { y := y - x }
  io.z := x
  io.v := y === UFix(0)

  defTests(io) {
    val (a, b, z) = (64, 48, 16)
    val vars = new HashMap[Node, Node]()
    var t = 0
    do {
      vars.clear()
      vars(io.a) = UFix(a)
      vars(io.b) = UFix(b)
      test(vars)
      t += 1
    } while (t <= 1 || vars(io.v).litValue() == 0)
    vars(io.z).litValue() == z
  }
}

}
