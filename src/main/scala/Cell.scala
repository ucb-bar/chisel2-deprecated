package Chisel {
import Component._
import IOdir._;
import Node._;

abstract class Cell extends nameable{
  val io: Data;
  val primitiveNode: Node;
  var isReg = false;
}

object chiselCast {
  def apply[S <: Data, T <: Bits](x: S)(gen: => T): T = {
    val cell = new ConversionCell(x)(gen);
    cell.io
  }
}

class ConversionCell[S <: Data, T <: Bits](x: S)(gen: => T) extends Cell {
  val io = gen.asOutput;
  io.setIsCellIO;
  val primitiveNode = new Wire();
  //val primitiveNode = x;
  primitiveNode.nameHolder = io;
  primitiveNode.init("", widthOf(0), x);
  io := primitiveNode
}

object UnaryNodeCell {
  def apply[T <: Data](x: T, op: String)(gen: => T): T = {
    val res = new UnaryNodeCell(op)(gen);
    res.io.In := x;
    res.io.Out
  }
}

class UnaryNodeCell[T <: Data](op: String)(gen: => T) extends Cell {
  val io = new Bundle(){
    val In  = gen.asInput;
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
  def apply[T <: Data](x: T, y: T, op: String)(gen: => T): T = {
    val res = new BinaryNodeCell(op)(gen);
    res.io.X := x;
    res.io.Y := y;
    res.io.Z
  }
}

class BinaryNodeCell[T <: Data](op: String)(gen: => T) extends Cell {
  val io = new Bundle(){
    val X = gen.asInput;
    val Y = gen.asInput;
    val Z = gen.asOutput;
  }
  io.setIsCellIO;
  val primitiveNode = op match {
    case "<<"  => Op("<<", 0, lshWidthOf(0, io.Y),  io.X, io.Y );
    case ">>"  => Op(">>", 0, rshWidthOf(0, io.Y),  io.X, io.Y );
    case ">>>" => Op(">>", 0, rshWidthOf(0, io.Y),  io.X, io.Y );
    case "+"   => Op("+",  2, maxWidth _,  io.X, io.Y );
    case "*"   => Op("*",  0, sumWidth _,  io.X, io.Y );
    case "^"   => Op("^",  2, maxWidth _,  io.X, io.Y );
    case "?"   => Multiplex(io.X, io.Y, null);
    case "-"   => Op("-",  2, maxWidth _,  io.X, io.Y );
    case "##"  => Op("##", 2, sumWidth _,  io.X, io.Y );
    case "&"   => Op("&",  2, maxWidth _, io.X, io.Y );
    case "|"   => Op("|",  2, maxWidth _, io.X, io.Y );
    case any   => null;
  }
  primitiveNode.name = "primitiveNode";
  primitiveNode.nameHolder = io.Z;
  io.Z := primitiveNode;
}

object LogicalNodeCell {
  def apply[T <: Data](x: T, y: T, op: String)(gen: => T): Bool = {
    val res = new LogicalNodeCell(op)(gen);
    res.io.X := x;
    res.io.Y := y;
    res.io.Z
  }
}

class LogicalNodeCell[T <: Data](op: String)(gen: => T) extends Cell {
  val io = new Bundle(){
    val X = gen.asInput;
    val Y = gen.asOutput;
    val Z = Bool('output);
  }
  io.setIsCellIO;
  val primitiveNode = op match {
    case "===" => Op("==", 2, fixWidth(1), io.X, io.Y );
    case "!="  => Op("!=", 2, fixWidth(1), io.X, io.Y );
    case ">"   => Op(">",  2, fixWidth(1), io.X, io.Y );
    case "<"   => Op("<",  2, fixWidth(1), io.X, io.Y );
    case "<="  => Op("<=", 2, fixWidth(1), io.X, io.Y );
    case ">="  => Op(">=", 2, fixWidth(1), io.X, io.Y );
    case "&&"  => Op("&&", 2, fixWidth(1), io.X, io.Y );
    case "||"  => Op("||", 2, fixWidth(1), io.X, io.Y );
    case any   => null;
  }
  primitiveNode.name = "primitiveNode";
  primitiveNode.nameHolder = io.Z;
  io.Z := primitiveNode;

}

object ReductionNodeCell {
  def apply[T <: Data](x: T, op: String)(gen: => T): Bool = {
    val res = new ReductionNodeCell(op)(gen);
    res.io.In := x;
    res.io.Out
  }
}

class ReductionNodeCell[T <: Data](op: String)(gen: => T) extends Cell {
  val io = new Bundle() {
    val In = gen.asInput;
    val Out = Bool('output);
  }
  io.setIsCellIO;
  val primitiveNode = op match {
    case "&" => Op("&",  1, fixWidth(1), io.In);
    case "|" => Op("|",  1, fixWidth(1), io.In);
    case "^" => Op("^",  1, fixWidth(1), io.In);
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
  val io = new Bundle(){
    val In = Bool('input);
    val Out = Bool('output);
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
  val io = new Bundle(){
    val X = Bool('input);
    val Y = Bool('input);
    val Z = Bool('output);
  }
  io.setIsCellIO;
  val primitiveNode = op match {
    case "^"   => Op("^",  2, maxWidth _,  io.X, io.Y );
    case "===" => Op("==", 2, fixWidth(1), io.X, io.Y );
    case "!="  => Op("!=", 2, fixWidth(1), io.X, io.Y );
    case ">"   => Op(">",  2, fixWidth(1), io.X, io.Y );
    case "<"   => Op("<",  2, fixWidth(1), io.X, io.Y );
    case "<="  => Op("<=", 2, fixWidth(1), io.X, io.Y );
    case ">="  => Op(">=", 2, fixWidth(1), io.X, io.Y );
    case "&&"  => Op("&&", 2, fixWidth(1), io.X, io.Y );
    case "||"  => Op("||", 2, fixWidth(1), io.X, io.Y );
    case "&"   => Op("&",  2, maxWidth _, io.X, io.Y );
    case "|"   => Op("|",  2, maxWidth _, io.X, io.Y );
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
        res.io.In := x;
        res.io.Out
    }
}

class or extends ReductionNodeCell[Fix]("|")(Fix()) {
}

object and {
    def apply(x: Fix): Bool = {
		val res = new and(); // trick the compiler to keeping the class
        res.io.In := x;
        res.io.Out
    }
}
class and extends ReductionNodeCell[Fix]("&")(Fix()) {
}


object xor {
    def apply(x: Fix): Bool = {
        val res = new xor()
        res.io.In := x;
        res.io.Out
    }
}

class xor extends ReductionNodeCell("^")(Fix()) {
}

}

