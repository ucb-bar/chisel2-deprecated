package Tutorial {

import Chisel._
import scala.math._
import scala.collection.mutable.HashMap

class Mux4 extends Component {
  val io = new Bundle {
    val in0 = Bits(width = 1, dir = INPUT)
    val in1 = Bits(width = 1, dir = INPUT)
    val in2 = Bits(width = 1, dir = INPUT)
    val in3 = Bits(width = 1, dir = INPUT)
    val sel = Bits(width = 2, dir = INPUT)
    val out = Bits(width = 1, dir = OUTPUT)
  }
  val m0 = new Mux2()
  m0.io.sel := io.sel(0) 
  m0.io.in0 := io.in0; m0.io.in1 := io.in1

  val m1 = new Mux2()
  m1.io.sel := io.sel(0)
  m1.io.in0 := io.in2; m1.io.in1 := io.in3

  val m3 = new Mux2()
  m3.io.sel := io.sel(1)
  m3.io.in0 := m0.io.out; m3.io.in1 := m1.io.out

  io.out := m3.io.out;
}

}
