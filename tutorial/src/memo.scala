package Tutorial {

import Chisel._
import scala.collection.mutable.HashMap

class Memo extends Component {
  val io = new Bundle {
    val isWr    = Bool(INPUT)
    val wrAddr  = UFix(8, INPUT)
    val wrData  = UFix(8, INPUT)
    val isRd    = Bool(INPUT)
    val rdAddr  = UFix(8, INPUT)
    val rdData  = UFix(8, OUTPUT)
  }
  val mem = Mem(256){ UFix(width = 8) }
  io.rdData := UFix(0)

  // fill in table logic

  defTests(io) {
    var allGood = true
    val vars    = new HashMap[Node, Node]()
    def rd(addr: UFix, data: UFix) = {
      vars.clear()
      vars(io.isRd)   = Bool(true)
      vars(io.rdAddr) = addr
      vars(io.rdData) = data
      test(vars)
    }
    def wr(addr: UFix, data: UFix)  = {
      vars.clear()
      vars(io.isWr)   = Bool(true)
      vars(io.wrAddr) = addr
      vars(io.wrData) = data
      test(vars)
    }
    allGood = wr(UFix(0), UFix(1))  && allGood
    allGood = rd(UFix(0), UFix(1))  && allGood
    allGood = wr(UFix(9), UFix(11)) && allGood
    allGood = rd(UFix(9), UFix(11)) && allGood
    allGood
  }
}

}
