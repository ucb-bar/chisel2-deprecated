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
}

class FilterTests(c: Filter) extends Tester(c, Array(c.io)) {
  defTests {
    var allGood = true
    val vars    = new HashMap[Node, Node]()
    val rnd     = new Random()
    val maxInt  = 1 << 16
    for (i <- 0 until 10) {
      vars.clear()
      val in                = rnd.nextInt(maxInt)
      vars(c.io.in.ready)   = Bool(true)
      vars(c.io.in.data)    = UFix(in)
      val isOdd             = (in&1)!=0
      vars(c.io.out.ready)  = Bool(isOdd)
      if (isOdd)
        vars(c.io.out.data) = UFix(in)
      allGood               = step(vars) && allGood
    }
    allGood
  }  
}

}
