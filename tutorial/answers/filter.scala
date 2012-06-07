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
}

}
