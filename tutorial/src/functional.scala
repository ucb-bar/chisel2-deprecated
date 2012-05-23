package Tutorial {

import Chisel._

class Functional extends Component {
  val io = new Bundle {
    val x   = UFix(16, INPUT)
    val y   = UFix(16, INPUT)
    val z   = UFix(16, OUTPUT)
  }
  def clb(a: Bits, b: BIts, c: Bits, d: Bits) =
    (a & b) | (~c & d)
  io.z := clb(io.x, io.y, io.x, io.y)
}

}
