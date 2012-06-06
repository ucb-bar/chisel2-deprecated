package Tutorial {

import Chisel._
import scala.collection.mutable.HashMap
import Literal._
import scala.util.Random

class LinkIO extends Bundle { 
  val data  = Bits(16, OUTPUT) 
  val ready = Bool(OUTPUT)
}

class FilterIO extends Bundle { 
  val in  = new LinkIO().flip
  val out = new LinkIO()
}

class Filter extends Component { 
  val io  = new FilterIO()

  io.out.ready := io.in.ready
  io.out.data  := io.in.data

  defTests(io) {
    var allGood = true
    val vars    = new HashMap[Node, Node]()
    val rnd     = new Random()
    val maxInt  = 1 << 16
    for (i <- 0 until 10) {
      vars.clear()
      val in             = rnd.nextInt(maxInt)
      vars(io.in.ready)  = Bool(true)
      vars(io.in.data)   = UFix(in)
      vars(io.out.ready) = Bool(true)
      vars(io.out.data)  = UFix(in)
      allGood            = test(vars) && allGood
    }
    allGood
  }  
}

}
