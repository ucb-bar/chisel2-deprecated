package Tutorial {

import Chisel._
import scala.collection.mutable.HashMap
import Literal._

class Memo extends Component {
  val w  = 8
  val io = new Bundle {
    val isWrite = Bool(INPUT)
    val addr    = UFix(8, INPUT)
    val in      = UFix(8, INPUT)
    val out     = UFix(8, OUTPUT)
  }
  val mem = Mem(sizeof(w)){ UFix(width = 8) }
  io.out := UFix(0)
  when (io.isWrite) {
    mem(io.addr) := io.in
  } .otherwise {
    io.out := mem(io.addr)
  }

  defTests(io) {
    var allGood = true
    val vars    = new HashMap[Node, Node]()
    def initVars(wr: Bool, addr: UFix, in: UFix, out: UFix) = {
      vars.clear()
      vars(io.isWrite) = wr
      vars(io.addr)    = addr
      vars(io.in)      = in
      vars(io.out)     = out
      vars
    }
    def rd(addr: UFix, out: UFix) = 
      test(initVars(Bool(false), addr, UFix(0), out))
    def wr(addr: UFix, in: UFix)  = 
      test(initVars(Bool(true),  addr, in,      UFix(0)))
    allGood = wr(UFix(0), UFix(1))  && allGood
    allGood = rd(UFix(0), UFix(1))  && allGood
    allGood = wr(UFix(9), UFix(11)) && allGood
    allGood = rd(UFix(9), UFix(11)) && allGood
    allGood
  }
}

}
