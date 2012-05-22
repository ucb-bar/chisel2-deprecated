// author: jonathan bachrach
package Chisel {

import scala.math.max;
import Node._;
import Component._;

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
    if(!node.isInstanceOf[Literal]) node.nameHolder = output
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

    if(!node.isInstanceOf[Literal]) node.nameHolder = output
    output assign node
    output
  }
}

object LogicalOp {
  def apply[T <: Data](x: T, y: T, op: String)(gen: => T): Bool = {
    if(searchAndMap && op == "&&" && chiselAndMap.contains((x, y))) {
      chiselAndMap((x, y))
    } else {
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
      if(!node.isInstanceOf[Literal]) node.nameHolder = output
      output assign node
      if(searchAndMap && op == "&&" && !chiselAndMap.contains((x, y))) {
        chiselAndMap += ((x, y) -> output)
      }
      output
    }
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
    if(!node.isInstanceOf[Literal]) node.nameHolder = output
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
    if(!node.isInstanceOf[Literal]) node.nameHolder = output
    output assign node
    output
  }
}

object BinaryBoolOp {
  def apply(x: Bool, y: Bool, op: String): Bool = {
    if(searchAndMap && op == "&&" && chiselAndMap.contains((x, y))) {
      chiselAndMap((x, y))
    } else {
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
      if(!node.isInstanceOf[Literal]) node.nameHolder = output
      output assign node
      if(searchAndMap && op == "&&" && !chiselAndMap.contains((x, y))) {
        chiselAndMap += ((x, y) -> output)
      }
      output
    }
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

object Op {
  def apply (name: String, nGrow: Int, widthInfer: (Node) => Int, a: Node, b: Node): Node = {
    val (a_lit, b_lit) = (a.litOf, b.litOf);
    if (isFolding) {
    if (a_lit != null && b_lit == null) {
      name match {
        case "&&" => return if (a_lit.value == 0) Literal(0) else b;
        case "||" => return if (a_lit.value == 0) b else Literal(1);
        case _ => ;
      }
    } else if (a_lit == null && b_lit != null) {
      name match {
        case "&&" => return if (b_lit.value == 0) Literal(0) else a;
        case "||" => return if (b_lit.value == 0) a else Literal(1);
        case _ => ;
      }
    } else if (a_lit != null && b_lit != null) {
      val (aw, bw) = (a_lit.width, b_lit.width);
      val (av, bv) = (a_lit.value, b_lit.value);
      name match {
        case "&&" => return if (av == 0) Literal(0) else b;
        case "||" => return if (bv == 0) a else Literal(1);
        case "===" => return Literal(if (av == bv) 1 else 0)
        case "!=" => return Literal(if (av != bv) 1 else 0);
        case "<"  => return Literal(if (av <  bv) 1 else 0);
        case ">"  => return Literal(if (av >  bv) 1 else 0);
        case "<=" => return Literal(if (av <= bv) 1 else 0);
        case ">=" => return Literal(if (av >= bv) 1 else 0);
        case "##" => return Literal(av << bw | bv, aw + bw);
        case "+"  => return Literal(av + bv, max(aw, bw)+1);
        case "-"  => return Literal(av - bv, max(aw, bw)+1);
        case "|"  => return Literal(av | bv, max(aw, bw));
        case "&"  => return Literal(av & bv, max(aw, bw));
        case "^"  => return Literal(av ^ bv, max(aw, bw));
        case "<<" => return Literal(av << bv.toInt, aw + bv.toInt);
        case ">>" => return Literal(av >> bv.toInt, aw - bv.toInt);
        case _ => ;
      } 
    }
    }
    val res = new Op();
    res.init("", widthInfer, a, b);
    res.op = name;
    res.nGrow = nGrow;
    res.isSigned = a.isInstanceOf[Fix] && b.isInstanceOf[Fix]
    res
  }
  def apply (name: String, nGrow: Int, widthInfer: (Node) => Int, a: Node): Node = {
    if (isFolding && a.litOf != null) {
      name match {
        case "!" => return if (a.litOf.value == 0) Literal(1) else Literal(0);
        case "-" => return Literal(-a.litOf.value, a.litOf.width);
        case "~" => return Literal((-a.litOf.value-1)&((BigInt(1) << a.litOf.width)-1), a.litOf.width);
        case _ => ;
      } 
    }
    val res = new Op();
    res.init("", widthInfer, a);
    res.op = name;
    res.nGrow = nGrow;
    res
  }
}
class Op extends Node {
  var op: String = "";
  var nGrow: Int = 0;
  override def dotName = if (op == "") "?" else op;
  override def toString: String =
    if (inputs.length == 1)
      op + inputs(0)
    else
      inputs(0) + op + inputs(1)

  def emitOpRef (k: Int): String = {
    if (op == "<<") {
      if (k == 0 && inputs(k).width < width)
	"DAT<" + width + ">(" + inputs(k).emitRef + ")"
      else
	inputs(k).emitRef
    } else if (op == "##" || op == ">>" || op == "*" ||
             op == "s*s" || op == "u*s" || op == "s*u") {
      inputs(k).emitRef
    } else {
      var w = 0;
      for (i <- 0 until nGrow)
	w = max(w, inputs(i).width);
      if (isCoercingArgs && nGrow > 0 && k < nGrow && w > inputs(k).width)
	"DAT<" + w + ">(" + inputs(k).emitRef + ")"
      else
	inputs(k).emitRef
    }
  }

  override def emitDef: String = {
    val c = component;
    "  assign " + emitTmp + " = " + 
      (if (op == "##") 
        "{" + inputs(0).emitRef + ", " + inputs(1).emitRef + "}"
       else if (inputs.length == 1)
         op + " " + inputs(0).emitRef
       else if (op == "s*s")
         "$signed(" + inputs(0).emitRef + ") * $signed(" + inputs(1).emitRef + ")"
       else if (op == "s*u")
         "$signed(" + inputs(0).emitRef + ") * " + inputs(1).emitRef
       else if (op == "u*s")
         inputs(0).emitRef + " * $signed(" + inputs(1).emitRef + ")"
       else if(isSigned)
	 emitDefSigned
       else
         inputs(0).emitRef + " " + op + " " + inputs(1).emitRef
      ) + ";\n"
  }

  def emitDefSigned: String = {
    if (op == ">>")
      "$signed(" + inputs(0).emitRef +") " + ">>>" + " " + inputs(1).emitRef
    else
      "$signed(" + inputs(0).emitRef +") " + op + " $signed(" + inputs(1).emitRef + ")"
  }

  override def emitDefLoC: String = {
    "  " + emitTmp + " = " +
      (if (op == "##") 
        "cat<" + width + ">(" + emitOpRef(0) + ", " + emitOpRef(1) + ")"
       else if (op == "s*s")
         inputs(0).emitRef + ".fix_times_fix(" + inputs(1).emitRef + ")"
       else if (op == "s*u")
         inputs(0).emitRef + ".fix_times_ufix(" + inputs(1).emitRef + ")"
       else if (op == "u*s")
         inputs(0).emitRef + ".ufix_times_fix(" + inputs(1).emitRef + ")"
       else if (inputs.length == 1)
         if (op == "|")
           "reduction_or(" + inputs(0).emitRef + ")"
         else if (op == "&")
           "reduction_and(" + inputs(0).emitRef + ")"
         else if (op == "^")
           "reduction_xor(" + inputs(0).emitRef + ")"
         else
           op + inputs(0).emitRef
       else if(isSigned)
	 emitSignedDefLoC;
       else
         emitOpRef(0) + " " + op + " " + emitOpRef(1)) +
    ";\n"
  }

  def emitSignedDefLoC: String = {
    if(op == ">>")
      emitOpRef(0) + ".rsha(" + emitOpRef(1) + ")"
    else if(op == ">")
      emitOpRef(0) + ".gt(" + emitOpRef(1) + ")"
    else if(op == ">=")
      emitOpRef(0) + ".gte(" + emitOpRef(1) + ")"
    else if(op == "<")
      emitOpRef(0) + ".lt(" + emitOpRef(1) + ")"
    else if(op == "<=")
      emitOpRef(0) + ".lt(" + emitOpRef(1) + ")"
    else 
      emitOpRef(0) + " " + op + " " + emitOpRef(1)
  }
}

}
