package Tutorial

import Chisel._
import scala.collection.mutable.HashMap
import scala.util.Random

object StoreMask                                                                
{                                                                               
  val MSK_W  = UFix(1)
  val MSK_H  = UFix(2)
  val MSK_B  = UFix(3)
  val MSK_HU = UFix(4)
  val MSK_BU = UFix(5)
  def apply(sel: UFix): Bits =                                                 
  {                                                                            
     val mask = Bits(width = 32)
     mask := Mux(sel === MSK_W                  , Bits(0xffffffff),
             Mux(sel === MSK_H || sel === MSK_HU, Bits(0xffff),
             Mux(sel === MSK_B || sel === MSK_BU, Bits(0xff),
                                                  Bits(0xffffffff, 32))))
     return mask
  }  
}  

class Combinational extends Component {
  val io = new Bundle {
    val x   = UFix(INPUT,  32)
    val y   = UFix(INPUT,  32)
    val z   = UFix(OUTPUT, 32)
  }
  // val io = new Bundle {
  //   val x   = UFix(INPUT,  16)
  //   val y   = UFix(INPUT,  16)
  //   val z   = UFix(OUTPUT, 16)
  // }
  // io.z := io.x + io.y
  io.z := StoreMask(io.x).toUFix
}

class CombinationalTests(c: Combinational) extends Tester(c, Array(c.io)) {
  defTests {
    println("WIDTH IO.Z(0) = " + c.io.z.inputs(0).width);
    var allGood = true
    val vars    = new HashMap[Node, Node]()
    val rnd     = new Random()
    val maxInt  = 1 << 16
    for (i <- 0 until 10) {
      vars.clear()
      val x = rnd.nextInt(maxInt)
      val y = rnd.nextInt(maxInt)
      vars(c.io.x) = UFix(x)
      vars(c.io.y) = UFix(y)
      vars(c.io.z) = UFix((x + y)&(maxInt-1))
      allGood = step(vars) && allGood
    }
    allGood
  }
}

