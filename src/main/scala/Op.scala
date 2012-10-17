package Chisel
import scala.math.max
import Node._
import Literal._
import Component._

abstract class Cell extends nameable{
  val io: Data;
  val primitiveNode: Node;
  var isReg = false;
}

object chiselCast {
  def apply[S <: Data, T <: Bits](x: S)(gen: => T): T = {
    val res = gen.asOutput
    x.nameHolder = res
    res.inputs += x.toNode
    res.setIsTypeNode
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
    node.setTypeNode(gen.asOutput)
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
    node.setTypeNode(gen.asOutput)
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
      if(searchAndMap && op == "&&" && !chiselAndMap.contains((x, y))) 
        chiselAndMap += ((x, y) -> output)
      node.setTypeNode(output)
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
    node.setTypeNode(Bool(OUTPUT))
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
    node.setTypeNode(Bool(OUTPUT))
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
      val output = Bool(OUTPUT)
      if(searchAndMap && op == "&&" && !chiselAndMap.contains((x, y))) 
        chiselAndMap += ((x, y) -> output)
      node.setTypeNode(output)
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
        case "==" => return Literal(if (av == bv) 1 else 0)
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
    if (backend.isInstanceOf[CppBackend]) {
      name match {
        case ">" | "<" | ">=" | "<=" =>
          if (a.isInstanceOf[Fix] && b.isInstanceOf[Fix]) {
            if (name != "<" || b.litOf == null || b.litOf.value != 0) {
              val fixA = a.asInstanceOf[Fix]
              val fixB = b.asInstanceOf[Fix]
              val msbA = fixA < Fix(0)
              val msbB = fixB < Fix(0)
              val ucond = LogicalOp(fixA.toUFix, fixB, name){UFix()}
              return Mux(msbA === msbB, ucond, (if (name(0) == '>') msbB else msbA).asInstanceOf[Bool])
            }
          }
        case "==" =>
          if (b.litOf != null && b.litOf.isZ) {
            val (bits, mask, swidth) = parseLit(b.litOf.name)
            return Op(name, nGrow, widthInfer, Op("&", 2, maxWidth _, a, Literal(BigInt(mask, 2))), Literal(BigInt(bits, 2)))
          }
          if (a.litOf != null && a.litOf.isZ)
            return Op(name, nGrow, widthInfer, b, a)
        case "s*s" =>
          val fixA = a.asInstanceOf[Fix]
          val signA = fixA < Fix(0)
          val absA = Mux(signA, -fixA, fixA)
          val fixB = b.asInstanceOf[Fix]
          val signB = fixB < Fix(0)
          val absB = Mux(signB, -fixB, fixB)
          val prod = absA.toUFix * absB.toUFix
          return Mux(signA ^ signB, -prod, prod)
        case "s*u" =>
          val fixA = a.asInstanceOf[Fix]
          val signA = fixA < Fix(0)
          val absA = Mux(signA, -fixA, fixA)
          val prod = absA.toUFix * b.asInstanceOf[UFix]
          return Mux(signA, -prod, prod)
        case "u*s" =>
          return Op("s*u", nGrow, widthInfer, b, a)
        case _ =>
      }
    }
    val res = new Op();
    res.init("", widthInfer, a, b);
    res.op = name;
    res.nGrow = nGrow;
    if(a.isSigned && b.isSigned) res.setIsSigned
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

  override def forceMatchingWidths = {
    if (inputs.length == 2) {
      if (List("|", "&", "^", "+", "-").contains(op)) {
        if (inputs(0).width != width) inputs(0) = inputs(0).matchWidth(width)
        if (inputs(1).width != width) inputs(1) = inputs(1).matchWidth(width)
      } else if (List("==", "!=", ">", ">=", "<", "<=").contains(op)) {
        val w = max(inputs(0).width, inputs(1).width)
        if (inputs(0).width != w) inputs(0) = inputs(0).matchWidth(w)
        if (inputs(1).width != w) inputs(1) = inputs(1).matchWidth(w)
      }
    }
  }  

}
