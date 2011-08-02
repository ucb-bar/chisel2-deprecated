package Chisel {
import Component._
import IOdir._;
import Node._;

abstract class Cell extends nameable{
  val io: dat_t;
  val primitiveNode: Node;
  var isReg = false;
}



object UnaryNodeCell {
  def apply(x: int_t, op: String): int_t = {
    val res = new UnaryNodeCell(op);
    res.io.In := x;
    res.io.Out
  }
}

class UnaryNodeCell(op: String) extends Cell {
  val io = new bundle_t(){val In = int_t(INPUT);
			val Out = int_t(OUTPUT);
		      }
  io.setIsCellIO;
  val primitiveNode = op match {
    case "-" => Op("-",  1, widthOf(0), io.In);
    case "~" => Op("~",  1, widthOf(0), io.In);
    case "!" => Op("!",  1, fixWidth(1), io.In);
    case any => null;
  }
  primitiveNode.name = "primitiveNode"
  primitiveNode.nameHolder = io.Out;
  io.Out := primitiveNode;
}

object BinaryNodeCell {
  def apply(x: int_t, y: int_t, op: String): int_t = {
    val res = new BinaryNodeCell(op);
    res.io.X := x;
    res.io.Y := y;
    res.io.Z
  }
}

class BinaryNodeCell(op: String) extends Cell {
  val io = new bundle_t(){val X = int_t(INPUT);
			  val Y = int_t(INPUT);
			  val Z = int_t(OUTPUT);}
  io.setIsCellIO;
  val primitiveNode = op match {
    case "<<"  => io.X.asInstanceOf[Node] <<  io.Y;
    case ">>"  => io.X.asInstanceOf[Node] >>  io.Y;
    case ">>>" => io.X.asInstanceOf[Node] >>> io.Y; 
    case "+"   => io.X.asInstanceOf[Node] +   io.Y;
    case "*"   => io.X.asInstanceOf[Node] *   io.Y;
    case "^"   => io.X.asInstanceOf[Node] ^   io.Y;
    case "?"   => io.X.asInstanceOf[Node] ?   io.Y;
    case "-"   => io.X.asInstanceOf[Node] -   io.Y;
    case "##"  => io.X.asInstanceOf[Node] ##  io.Y;
    case "===" => io.X.asInstanceOf[Node] === io.Y;
    case "!="  => io.X.asInstanceOf[Node] !=  io.Y;
    case ">"   => io.X.asInstanceOf[Node] >   io.Y;
    case "<"   => io.X.asInstanceOf[Node] <   io.Y;
    case "<="  => io.X.asInstanceOf[Node] <=  io.Y;
    case ">="  => io.X.asInstanceOf[Node] >=  io.Y;
    case "&&"  => io.X.asInstanceOf[Node] &&  io.Y;
    case "||"  => io.X.asInstanceOf[Node] ||  io.Y;
    case "&"   => io.X.asInstanceOf[Node] &   io.Y;
    case "|"   => io.X.asInstanceOf[Node] |   io.Y;
    case any   => null;
  }
  primitiveNode.name = "primitiveNode";
  primitiveNode.nameHolder = io.Z;
  io.Z := primitiveNode;
}

}

