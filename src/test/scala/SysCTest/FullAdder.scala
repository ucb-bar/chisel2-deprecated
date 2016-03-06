package SysCTest

import Chisel._

class FullAdderInput extends Bundle {
  val a   = Bits(width = 1)
  val b   = Bits(width = 1)
  val cin = Bits(width = 1)
}

class FullAdderOutput extends Bundle {
  val sum  = Bits(width = 1)
  val cout = Bits(width = 1)
}

class FullAdder extends Module {
  val io = new Bundle {
    val in  = Decoupled(new FullAdderInput()).flip()
    val out = Decoupled(new FullAdderOutput())
  }

  val in_a = Reg(UInt(1))
  val in_b = Reg(UInt(1))
  val in_cin = Reg(UInt(1))
  val out_sum = Reg(init=UInt(0, 1))
  val out_cout = Reg(init=UInt(0, 1))

  val p = Reg(init=Bool(false))
  val q = Reg(init=Bool(true))
  io.in.ready := !p
  io.out.valid := !q

  when (io.in.valid && !p) {
    in_a := io.in.bits.a
    in_b := io.in.bits.b
    in_cin := io.in.bits.cin
    io.in.ready := Bool(false)
    io.out.valid := Bool(false)
    p := Bool(true)
    q := Bool(true)
  }

  when (io.out.ready && p) {
    // Calculate the sum
    val a_xor_b = in_a ^ in_b
    out_sum := a_xor_b ^ in_cin
    // Generate the carry
    val a_and_b = io.in.bits.a & io.in.bits.b
    val a_and_cin = io.in.bits.a & io.in.bits.cin
    val b_and_cin = io.in.bits.b & io.in.bits.cin
    out_cout := a_and_b | b_and_cin | a_and_cin
    p := Bool(false)
    q := Bool(false)
    io.out.valid := p
  }
  io.out.bits.sum := out_sum
  io.out.bits.cout := out_cout
}

class FullAdderTests(c: FullAdder) extends Tester(c) {
  var i = 0
  do {
    val a    = rnd.nextInt(2)
    val b    = rnd.nextInt(2)
    val cin  = rnd.nextInt(2)
    val res  = a + b + cin
    val sum  = res & 1
    val cout = (res >> 1) & 1
    var transfer = false
    poke(c.io.in.valid, 0)
    poke(c.io.out.ready, 0)

    do {
      transfer = (peek(c.io.in.ready) == 1)
      step(1)
    } while (t < 50 && !transfer)

    poke(c.io.in.bits.a, a)
    poke(c.io.in.bits.b, b)
    poke(c.io.in.bits.cin, cin)
    poke(c.io.in.valid, 1)
    poke(c.io.out.ready, 1)

    do {
      transfer = (peek(c.io.out.valid) == 1)
      step(1)
    } while (t < 50 && !transfer)

    expect(c.io.out.bits.sum, sum)
    expect(c.io.out.bits.cout, cout)
    i += 1
    printf("* INPUT -> a: %d  b: %d  cin: %d",a, b, cin)
    printf("  -  OUTPUT -> sum: %d  cout: %d\n", sum, cout)
  } while (t < 50 && i < 4)
  if (t >= 50) ok = false
}

object FullAdder {
  def main(mainArgs: scala.Array[String]): Unit = {
    val sysCArgs = Array[String]("--backend", "sysc", "--genHarness", "--compile", "--test")
    val args = sysCArgs ++ mainArgs
    chiselMainTest(args,
       () => Module(new FullAdder())){c => new FullAdderTests(c)}
  }
}
