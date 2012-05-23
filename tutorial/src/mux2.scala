package Tutorial {

import Chisel._

class Mux2 extends Component {
  val io = new Bundle{
    val sel = Bits(width = 1, dir = INPUT)
    val in0 = Bits(width = 1, dir = INPUT)
    val in1 = Bits(width = 1, dir = INPUT)
    val out = Bits(width = 1, dir = OUTPUT)
  }
  io.out := (io.sel & io.in1) | (~io.sel & io.in0)
}

}
