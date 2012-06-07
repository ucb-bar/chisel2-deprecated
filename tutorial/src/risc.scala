package Tutorial {

import Chisel._
import scala.collection.mutable.HashMap

class Risc extends Component {
  val io = new Bundle {
    val isWr   = Bool(INPUT)
    val wrAddr = UFix(8, INPUT)
    val wrData = Bits(32, INPUT)
    val boot   = Bool(INPUT)
    val valid  = Bool(OUTPUT)
    val out    = Bits(32, OUTPUT)
  }
  val file = Mem(256){ Bits(width = 32) }
  val code = Mem(256){ Bits(width = 32) }
  val pc   = Reg(resetVal = UFix(0, 8))
  
  val add_op :: sub_op :: Nil = Enum(2){ Bits() }

  val inst = code(pc)
  val op   = inst(31,24)
  val rci  = inst(23,16)
  val rai  = inst(15, 8)
  val rbi  = inst( 7, 0)

  val ra = Mux(rai === Bits(0), Bits(0), file(rai))
  val rb = Mux(rbi === Bits(0), Bits(0), file(rbi))
  val rc = Bits(width = 32)

  io.valid := Bool(false)
  io.out   := Bits(0)
  rc       := Bits(0)

  when (io.isWr) {
    code(io.wrAddr) := io.wrData
  } .elsewhen (io.boot) {
    pc := UFix(0)
  } .otherwise {
    switch(op) {
      // is(add_op) { rc := ra + rb }
      is(sub_op) { rc := ra - rb }
    }
    io.out := rc
    when (rci === UFix(0)) {
      io.valid := Bool(true)
    } .otherwise {
      file(rci) := rc
    }
    pc := pc + UFix(1)
  }
  
  defTests(io, pc) {
    var allGood = true
    val svars = new HashMap[Node, Node]()
    val ovars = new HashMap[Node, Node]()
    def wr(addr: UFix, data: UFix)  = {
      svars.clear()
      svars(io.isWr)   = Bool(true)
      svars(io.wrAddr) = addr
      svars(io.wrData) = data
      test(svars, ovars)
    }
    def boot()  = {
      svars.clear()
      svars(io.boot)   = Bool(true)
      test(svars, ovars)
    }
    def step()  = {
      svars.clear()
      svars(io.boot)   = Bool(false)
      test(svars, ovars)
    }
    def I (op: Bits, rc: Int, ra: Int, rb: Int) = 
      Cat(op, Bits(rc, 8), Bits(ra, 8), Bits(rb, 8))
    val app  = Array(I(add_op, 1, 0, 0),
                     I(add_op, 1, 1, 0),
                     I(add_op, 2, 0, 0),
                     I(add_op, 2, 2, 1),
                     I(add_op, 2, 2, 1),
                     I(add_op, 0, 2, 0))
    wr(UFix(0), Bits(0)) // skip reset
    for (addr <- 0 until app.length) 
      wr(UFix(addr), app(addr))
    boot()
    do {
      step()
    } while (ovars(io.valid).litValue() == 0)
    allGood = ovars(io.out).litValue() == 2 && allGood
    allGood
  }
}

}
