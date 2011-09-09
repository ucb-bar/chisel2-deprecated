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
  def apply[T <: dat_t](x: T, op: String)(gen: => T): T = {
    val res = new UnaryNodeCell(op)(gen);
    res.io.In := x;
    res.io.Out
  }
}

class UnaryNodeCell[T <: dat_t](op: String)(gen: => T) extends Cell {
  val io = new bundle_t(){val In = gen.asInput;
			val Out = gen.asOutput;
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
  def apply[T <: dat_t](x: T, y: T, op: String)(gen: => T): T = {
    val res = new BinaryNodeCell(op)(gen);
    res.io.X := x;
    res.io.Y := y;
    res.io.Z
  }
}

class BinaryNodeCell[T <: dat_t](op: String)(gen: => T) extends Cell {
  val io = new bundle_t(){val X = gen.asInput;
			  val Y = gen.asInput;
			  val Z = gen.asOutput;}
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
    case "&"   => io.X.asInstanceOf[Node] &   io.Y;
    case "|"   => io.X.asInstanceOf[Node] |   io.Y;
    case any   => null;
  }
  primitiveNode.name = "primitiveNode";
  primitiveNode.nameHolder = io.Z;
  io.Z := primitiveNode;
}

object LogicalNodeCell {
  def apply[T <: dat_t](x: T, y: T, op: String)(gen: => T): Bool = {
    val res = new LogicalNodeCell(op)(gen);
    res.io.X := x;
    res.io.Y := y;
    res.io.Z
  }
}

class LogicalNodeCell[T <: dat_t](op: String)(gen: => T) extends Cell {
  val io = new bundle_t(){val X = gen.asInput;
			  val Y = gen.asOutput;
			  val Z = Bool(OUTPUT);}
  io.setIsCellIO;
  val primitiveNode = op match {
    case "===" => io.X.asInstanceOf[Node] === io.Y;
    case "!="  => io.X.asInstanceOf[Node] !=  io.Y;
    case ">"   => io.X.asInstanceOf[Node] >   io.Y;
    case "<"   => io.X.asInstanceOf[Node] <   io.Y;
    case "<="  => io.X.asInstanceOf[Node] <=  io.Y;
    case ">="  => io.X.asInstanceOf[Node] >=  io.Y;
    case "&&"  => io.X.asInstanceOf[Node] &&  io.Y;
    case "||"  => io.X.asInstanceOf[Node] ||  io.Y;
    case any   => null;
  }
  primitiveNode.name = "primitiveNode";
  primitiveNode.nameHolder = io.Z;
  io.Z := primitiveNode;

}

object ReductionNodeCell {
  def apply[T <: dat_t](x: T, op: String)(gen: => T): Bool = {
    val res = new ReductionNodeCell(op)(gen);
    res.io.In := x;
    res.io.Out
  }
}

class ReductionNodeCell[T <: dat_t](op: String)(gen: => T) extends Cell {
  val io = new bundle_t() {val In = gen.asInput;
                           val Out = Bool(OUTPUT);}
  io.setIsCellIO;
  val primitiveNode = op match {
    case "&" => Op("&",  1, fixWidth(1), io.In);
    case "|" => Op("|",  1, fixWidth(1), io.In);
    case any => null;
  }
  primitiveNode.name = "primitiveNode";
  primitiveNode.nameHolder = io.Out;
  io.Out := primitiveNode;
}


object UnaryBoolCell {
  def apply(x: Bool, op: String): Bool = {
    val res = new UnaryBoolCell(op);
    res.io.In := x;
    res.io.Out
  }
}

class UnaryBoolCell(op: String) extends Cell {
  val io = new bundle_t(){val In = Bool(INPUT);
			val Out = Bool(OUTPUT);
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


object BinaryBoolCell {
  def apply(x: Bool, y: Bool, op: String): Bool = {
    val res = new BinaryBoolCell(op);
    res.io.X := x;
    res.io.Y := y;
    res.io.Z
  }
}

class BinaryBoolCell(op: String) extends Cell {
  val io = new bundle_t(){val X = Bool(INPUT);
		      val Y = Bool(INPUT);
		      val Z = Bool(OUTPUT);
		    }
  io.setIsCellIO;
  val primitiveNode = op match {
    case "^"   => io.X.asInstanceOf[Node] ^   io.Y;
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

object or {
    def apply(x: Fix): Bool = {
		val res = new or(); // trick the compiler to keeping the class
    	ReductionNodeCell(x, "|"){Fix()}
    }
}

class or extends ReductionNodeCell[Fix]("|")(Fix()) {
}

object and {
    def apply(x: Fix): Bool = {
		val res = new and(); // trick the compiler to keeping the class
    	ReductionNodeCell(x, "&"){Fix()}
    }
}

class and extends ReductionNodeCell[Fix]("&")(Fix()) {

}
}

