package verify {

import Chisel._

class GCD extends Component {
  val io = new Bundle {
    val a   = UFix(16, INPUT)
    val b   = UFix(16, INPUT)
    val z   = UFix(16, OUTPUT)
    val rdy = Bool(OUTPUT)
  }
  val x  = Reg(resetVal = io.a)
  val y  = Reg(resetVal = io.b)
  when   (x > y) { x := x - y } 
  unless (x > y) { y := y - x }
  io.z   := x
  io.rdy := y === UFix(0)
}

object GCD {
  def main (args: Array[String]): Unit = {
    chiselMain(args, () => new GCD())
  }
}

}
