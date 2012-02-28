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
    val res = gen.asOutput
    res.setIsCellIO
    x.nameHolder = res
    res.inputs += x.toNode
    res
  }
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
    output
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

object ReductionOp {
  def apply[T <: Data](x: T, op: String)(gen: => T): Bool = {
    val node = op match {
      case "&" => Op("&",  1, fixWidth(1), x);
      case "|" => Op("|",  1, fixWidth(1), x);
      case "^" => Op("^",  1, fixWidth(1), x);
      case any => null;
    }
    
    // make output
    val output = Bool(OUTPUT)
    output.setIsCellIO
    node.nameHolder = output
    output assign node
    output
  }
}

object UnaryBoolOp {
  def apply(x: Bool, op: String): Bool = {
    val node = op match {
    case "-" => Op("-",  1, widthOf(0), x);
    case "~" => Op("~",  1, widthOf(0), x);
    case "!" => Op("!",  1, fixWidth(1), x);
    case any => null;
    }

    //make ouput
    val output = Bool(OUTPUT)
    output.setIsCellIO
    node.nameHolder = output
    output assign node
    output
  }
}

object BinaryBoolOp {
  def apply(x: Bool, y: Bool, op: String): Bool = {
    val node = op match {
      case "^"   => Op("^",  2, maxWidth _,  x, y );
      case "===" => Op("==", 2, fixWidth(1), x, y );
      case "!="  => Op("!=", 2, fixWidth(1), x, y );
      case ">"   => Op(">",  2, fixWidth(1), x, y );
      case "<"   => Op("<",  2, fixWidth(1), x, y );
      case "<="  => Op("<=", 2, fixWidth(1), x, y );
      case ">="  => Op(">=", 2, fixWidth(1), x, y );
      case "&&"  => Op("&&", 2, fixWidth(1), x, y );
      case "||"  => Op("||", 2, fixWidth(1), x, y );
      case "&"   => Op("&",  2, maxWidth _, x, y );
      case "|"   => Op("|",  2, maxWidth _, x, y );
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

object andR {
    def apply(x: Bits): Bool = ReductionOp(x, "&"){Bits()}
}

object orR {
    def apply(x: Bits): Bool = ReductionOp(x, "|"){Bits()}
}

object xorR {
    def apply(x: Bits): Bool = ReductionOp(x, "^"){Bits()}
}

}

