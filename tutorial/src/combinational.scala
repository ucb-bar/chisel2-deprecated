package Tutorial {

import Chisel._

class Combinational extends Component {
  val io = new Bundle {
    val x   = UFix(16, INPUT)
    val y   = UFix(16, INPUT)
    val z   = UFix(16, OUTPUT)
  }
  io.z := io.x + io.y
}

}
