package Tutorial {

import Chisel._
import scala.collection.mutable.HashMap
import Literal._
import scala.util.Random

class LinkIO extends Bundle { 
  val data  = Bits(16, OUTPUT) 
  val valid = Bool(OUTPUT)
}

class FilterIO extends Bundle { 
  val in  = new LinkIO().flip
  val out = new LinkIO()
}

class Filter extends Component { 
  val io  = new FilterIO()

  io.out.data  := io.in.data
  io.out.valid := io.in.valid & (io.in.data&Bits(1))

  defTests(io) {
    var allGood = true
    val vars    = new HashMap[Node, Node]()
    val rnd     = new Random()
    val maxInt  = 1 << 16
    for (i <- 0 until 10) {
      vars.clear()
      val in              = rnd.nextInt(maxInt)
      vars(io.in.ready)   = Bool(true)
      vars(io.in.data)    = UFix(in)
      val isOdd           = (in&1)!=0
      vars(io.out.ready)  = Bool(isOdd)
      if (isOdd)
        vars(io.out.data) = UFix(in)
      allGood             = test(vars) && allGood
    }
    allGood
  }  
}

}
