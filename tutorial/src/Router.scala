package Tutorial

import Chisel._
import Node._;
import Literal._;
import scala.collection.mutable.HashMap

class ReadCmd extends Bundle {
  val addr = UFix(width = 32);
}

class WriteCmd extends ReadCmd {
  val data = UFix(width = 32)
}

class Packet extends Bundle {
  val header = UFix(width = 8)
  val body   = Bits(width = 64)
}

class RouterIO(n: Int) extends Bundle {
  override def clone = new RouterIO(n).asInstanceOf[this.type]
  val reads   = (new DeqIO()){ new ReadCmd() }
  val replies = (new EnqIO()){ UFix(width = 8) }
  val writes  = (new DeqIO()){ new WriteCmd() }
  val in      = (new DeqIO()){ new Packet() }
  val outs    = Vec(n){ (new EnqIO()){ new Packet() } }
}

class Router extends Component {
  val depth = 32
  val n     = 4
  val io    = new RouterIO(n)
  val tbl   = Mem(depth){ UFix(width = sizeof(n)) }
  when(io.reads.valid && io.replies.ready) { 
    val cmd = io.reads.deq();  io.replies.enq(tbl(cmd.addr))  
  } .elsewhen(io.writes.valid) { 
    val cmd = io.writes.deq(); tbl(cmd.addr) := cmd.data
  } .elsewhen(io.in.valid) {
    val pkt = io.in.deq(); io.outs(tbl(pkt.header(0))).enq(pkt) 
  } 
}

