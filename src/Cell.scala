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
  //val primitiveNode = new Wire();
  val primitiveNode = x;
  primitiveNode.nameHolder = io;
  //primitiveNode.init("", widthOf(0), x);
  io assign primitiveNode
}

object UnaryOp {
  def apply[T <: Data](x: T, op: String)(gen: => T): T = {
    val node = op match {
    case "-" => Op("-",  1, widthOf(0), x);
    case "~" => Op("~",  1, widthOf(0), x);
    case "!" => Op("!",  1, fixWidth(1), x);
    case any => null;
    }

    // make output
    val output = gen.asOutput
    output.setIsCellIO
    node.nameHolder = output
    output assign node
  }
}

object BinaryNodeCell {
  def apply[T <: Data](x: T, y: T, op: String)(gen: => T): T = {
    val res = new BinaryNodeCell(op, x, y)(gen);
    res.io.Z
  }
}

object BinaryOp {
  def apply[T <: Data](x: T, y: T, op: String)(gen: => T): T = {
    val node = op match {
      case "<<"  => Op("<<", 0, lshWidthOf(0, y),  x, y );
      case ">>"  => Op(">>", 0, rshWidthOf(0, y),  x, y );
      case "+"   => Op("+",  2, maxWidth _,  x, y );
      case "*"   => Op("*",  0, sumWidth _,  x, y );
      case "s*s" => Op("s*s",  0, sumWidth _,  x, y );
      case "s*u" => Op("s*u",  0, sumWidth _,  x, y );
      case "u*s" => Op("u*s",  0, sumWidth _,  x, y );
      case "^"   => Op("^",  2, maxWidth _,  x, y );
      case "?"   => Multiplex(x, y, null);
      case "-"   => Op("-",  2, maxWidth _,  x, y );
      case "##"  => Op("##", 2, sumWidth _,  x, y );
      case "&"   => Op("&",  2, maxWidth _, x, y );
      case "|"   => Op("|",  2, maxWidth _, x, y );
      case any   => null;
    }

    // make output
    val output = gen.asOutput
    output.setIsCellIO
    node match {
      case l: Literal =>
      case _          =>
        node.nameHolder = output
    }
    output assign node
    output
  }
}

class BinaryNodeCell[T <: Data](op: String, x: T, y: T)(gen: => T) extends Cell {
  val io = new Bundle(){
    val X = gen.asInput;
    val Y = gen.asInput;
    val Z = gen.asOutput;
  }
  io.X assign x;
  io.Y assign y;
  io.setIsCellIO;
  val primitiveNode = op match {
    case "<<"  => Op("<<", 0, lshWidthOf(0, io.Y),  io.X, io.Y );
    case ">>"  => Op(">>", 0, rshWidthOf(0, io.Y),  io.X, io.Y );
    case "+"   => Op("+",  2, maxWidth _,  io.X, io.Y );
    case "*"   => Op("*",  0, sumWidth _,  io.X, io.Y );
    case "s*s" => Op("s*s",  0, sumWidth _,  io.X, io.Y );
    case "s*u" => Op("s*u",  0, sumWidth _,  io.X, io.Y );
    case "u*s" => Op("u*s",  0, sumWidth _,  io.X, io.Y );
    case "^"   => Op("^",  2, maxWidth _,  io.X, io.Y );
    case "?"   => Multiplex(io.X, io.Y, null);
    case "-"   => Op("-",  2, maxWidth _,  io.X, io.Y );
    case "##"  => Op("##", 2, sumWidth _,  io.X, io.Y );
    case "&"   => Op("&",  2, maxWidth _, io.X, io.Y );
    case "|"   => Op("|",  2, maxWidth _, io.X, io.Y );
    case any   => null;
  }
  primitiveNode match {
    case l: Literal => 
    case _          => 
      primitiveNode.name = "primitiveNode";
      primitiveNode.nameHolder = io.Z;
  }
  io.Z assign primitiveNode;
}

object LogicalNodeCell {
  def apply[T <: Data](x: T, y: T, op: String)(gen: => T): Bool = {
    val res = new LogicalNodeCell(op)(gen);
    res.io.X assign x;
    res.io.Y assign y;
    res.io.Z
  }
}

object LogicalOp {
  def apply[T <: Data](x: T, y: T, op: String)(gen: => T): Bool = {
    val node = op match {
    case "===" => Op("==", 2, fixWidth(1), x, y );
    case "!="  => Op("!=", 2, fixWidth(1), x, y );
    case ">"   => Op(">",  2, fixWidth(1), x, y );
    case "<"   => Op("<",  2, fixWidth(1), x, y );
    case "<="  => Op("<=", 2, fixWidth(1), x, y );
    case ">="  => Op(">=", 2, fixWidth(1), x, y );
    case "&&"  => Op("&&", 2, fixWidth(1), x, y );
    case "||"  => Op("||", 2, fixWidth(1), x, y );
    case any   => null;
    }

    // make output
    val output = Bool(OUTPUT)
    output.setIsCellIO
    node.nameHolder = output
    output assign node
    output
  }
}

class LogicalNodeCell[T <: Data](op: String)(gen: => T) extends Cell {
  val io = new Bundle(){
    val X = gen.asInput;
    val Y = gen.asOutput;
    val Z = Bool(OUTPUT);
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
  io.Z assign primitiveNode;

}

object ReductionNodeCell {
  def apply[T <: Data](x: T, op: String)(gen: => T): Bool = {
    val res = new ReductionNodeCell(op)(gen);
    res.io.In assign x;
    res.io.Out
  }
}

class ReductionNodeCell[T <: Data](op: String)(gen: => T) extends Cell {
  val io = new Bundle() {
    val In = gen.asInput;
    val Out = Bool(OUTPUT);
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
  io.Out assign primitiveNode;
}

object ReductionOp {
  def apply[T <: Data](x: T, op: String)(gen: => T): Bool = {
    val node = op match {
      case "&" => Op("&",  1, fixWidth(1), x);
      case "|" => Op("|",  1, fixWidth(1), x);
      case "^" => Op("^",  1, fixWidth(1), x);
      case any => null;
    }
    
    // make output
    val output = Bool(output)
    output.setIsCellIO
    node.nameHolder = node
    output assign node
    output
  }
}


object UnaryBoolCell {
  def apply(x: Bool, op: String): Bool = {
    val res = new UnaryBoolCell(op);
    res.io.In assign x;
    res.io.Out
  }
}

class UnaryBoolCell(op: String) extends Cell {
  val io = new Bundle(){
    val In = Bool(INPUT);
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
  io.Out assign primitiveNode;
}

object UnaryBoolOp {
  def apply(x: Bool, op: String): Bool = {
    val node = op match {
    case "-" => Op("-",  1, widthOf(0), io.In);
    case "~" => Op("~",  1, widthOf(0), io.In);
    case "!" => Op("!",  1, fixWidth(1), io.In);
    case any => null;
    }

    //make ouput
    val output = Bool(OUTPUT)
    output.setIsCellIO
    node.nameHolder = io.out
    output assign node
    output
  }
}


object BinaryBoolCell {
  def apply(x: Bool, y: Bool, op: String): Bool = {
    val res = new BinaryBoolCell(op);
    res.io.X assign x;
    res.io.Y assign y;
    res.io.Z
  }
}

class BinaryBoolCell(op: String) extends Cell {
  val io = new Bundle(){
    val X = Bool(INPUT);
    val Y = Bool(INPUT);
    val Z = Bool(OUTPUT);
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
  io.Z assign primitiveNode;
}

object BinaryBoolOp {
  def apply(x: Bool, y: Bool, op: String): Bool = {
    val node = op match {
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

    // make output
    val output = Bool(OUTPUT)
    output.setIsCellIO
    node.nameHolder = output
    output assign node
    output
  }
}

object or {
    def apply(x: Bits): Bool = {
		val res = new or(); // trick the compiler to keeping the class
    	ReductionNodeCell(x, "|"){Bits()}
        res.io.In assign x;
        res.io.Out
    }
}

class or extends ReductionNodeCell[Bits]("|")(Bits()) {
}

object and {
    def apply(x: Bits): Bool = {
		val res = new and(); // trick the compiler to keeping the class
        res.io.In assign x;
        res.io.Out
    }
}
class and extends ReductionNodeCell[Bits]("&")(Bits()) {
}


object xor {
    def apply(x: Bits): Bool = {
        val res = new xor()
        res.io.In assign x;
        res.io.Out
    }
}

class xor extends ReductionNodeCell("^")(Bits()) {
}

}

