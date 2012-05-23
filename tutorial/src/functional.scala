package Tutorial {

import Chisel._

class Functional extends Component {
  val io = new Bundle {
    val x   = Bits(16, INPUT)
    val y   = Bits(16, INPUT)
    val z   = Bits(16, OUTPUT)
  }
  def clb(a: Bits, b: Bits, c: Bits, d: Bits) =
    (a & b) | (~c & d)
  io.z := clb(io.x, io.y, io.x, io.y)
}

}
