//package ChiselTests
import Chisel._

class VecApp(n: Int, W: Int) extends Module {
  val io = new Bundle {
    val a = UInt(INPUT, n)
    val i = Vec(n, Bits(INPUT, W))
    val d = Bits(OUTPUT, W)
  }
  // for (j <- 0 until n)
  //   io.o(j) := io.i(j)
  // val w = Wire(Vec(n,  Bits(width = W) ))
  // w := io.i
  // io.o := w
  // io.d := w(io.a)
  io.d := io.i(io.a)
  // io.o := io.i
}

class VecAppTester(c: VecApp) extends Tester(c) {
}
