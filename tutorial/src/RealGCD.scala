package Tutorial {

import Chisel._
import Node._
import scala.collection.mutable.HashMap

class RealGCDInput extends Bundle {
  val a = Bits(width = 16)
  val b = Bits(width = 16)
}


class RealGCD extends Component {
  val io  = new Bundle {
    val in  = new FIFOIO()( new RealGCDInput() ).flip()
    val out = new PipeIO()( Bits(width = 16) )
  }

  val x = Reg(){ Bits() }
  val y = Reg(){ Bits() }
  val first = Reg(resetVal = Bool(true) )

  io.in.ready := first || y === Bits(0)

  when (io.in.valid && io.in.ready) {
    x := io.in.bits.a
    y := io.in.bits.b
    first := Bool(false)
  }

  when (!first && y != Bits(0)) {
    when (x > y) { x := y; y := x}
    when (x <= y) { y := y - x }
  }

  io.out.bits := x
  io.out.valid := y === Bits(0) && !first
}

class RealGCDTests(c: RealGCD) extends Tester(c, Array(c.io)) {
  defTests {
    val inputs = List( (48, 32), (7, 3), (100, 10) )
    val outputs = List( 16, 1, 10)
    val dataVars = new HashMap[Node, Node]()
    val handshakeVars = new HashMap[Node, Node]()

    // let it spin for a bit
    for (i <- 0 until 5) {
      dataVars(c.io.in.valid) = Bool(false)
      step(dataVars, handshakeVars, false)
    }

    var i = 0
    var j = 0

    var allPassed = true

    while(i < 3 || j < 3) {
      if (i < 3) {
        dataVars(c.io.in.bits.a) = Bits(inputs(i)._1)
        dataVars(c.io.in.bits.b) = Bits(inputs(i)._2)
        dataVars(c.io.in.valid) = Bool(true)
      } else {
        dataVars(c.io.in.valid) = Bool(false)
      }

      // this advances the clock
      step(dataVars, handshakeVars)

      // bump counters and check outputs after advancing clock
      if (handshakeVars(c.io.in.ready).litValue() == 1) i += 1
      if (handshakeVars(c.io.out.valid).litValue() == 1) {
        allPassed = allPassed && handshakeVars(c.io.out.bits).litValue() == outputs(j)
        j += 1
      }

    }
    allPassed
  }
}

}
