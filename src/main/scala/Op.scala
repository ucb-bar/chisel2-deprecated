/*
 Copyright (c) 2011, 2012, 2013 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

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
    val res = gen
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
      case "s*u" => Op("s*s",  0, mulSUWidth _,  x, Cat(Bits(0,1), y).toFix );
      case "u*s" => BinaryOp(y, x, "s*u")(gen)
      case "/"   => Op("/",  0, widthOf(0),  x, y );
      case "s/s" => Op("s/s",  0, widthOf(0),  x, y );
      case "s/u" => Op("s/s",  0, widthOf(0),  x, Cat(Bits(0,1), y).toFix );
      case "u/s" => Op("s/s",  0, divUSWidth _, Cat(Bits(0,1), x).toFix, y );
      case "%"   => Op("%",  0, minWidth _,  x, y );
      case "s%s" => Op("s%s",  0, minWidth _,  x, y );
      case "s%u" => Op("s%u",  0, modSUWidth _,  x, Cat(Bits(0,1), y).toFix );
      case "u%s" => Op("u%s",  0, modUSWidth _, Cat(Bits(0,1), x).toFix, y );
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

  // width inference functions for signed-unsigned operations
  private def mulSUWidth(x: Node) = sumWidth(x) - 1
  private def divUSWidth(x: Node) = widthOf(0)(x) - 1
  private def modUSWidth(x: Node) = x.inputs(1).width.min(x.inputs(0).width - 1)
  private def modSUWidth(x: Node) = x.inputs(0).width.min(x.inputs(1).width - 1)
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
      if(searchAndMap && op == "&&" && !chiselAndMap.contains((x, y))) {
        chiselAndMap += ((x, y) -> output)
      }
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
      if(searchAndMap && op == "&&" && !chiselAndMap.contains((x, y))) {
        chiselAndMap += ((x, y) -> output)
      }
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
    val isSigned = a.isSigned && b.isSigned
    if (isFolding && !isSigned) {
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
        case "+"  => return Literal(av + bv, max(aw, bw) + 1);
        case "-"  => return Literal(av - bv, max(aw, bw) + 1);
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
      def signAbs(x: Node) = {
        val f = x.asInstanceOf[Fix]
        val s = f < Fix(0)
        (s, Mux(s, -f, f).toUFix)
      }
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
          if (a.litOf != null && a.litOf.isZ) {
            return Op(name, nGrow, widthInfer, b, a)
          }
        case "s*s" =>
          val (signA, absA) = signAbs(a)
          val (signB, absB) = signAbs(b)
          val prod = absA * absB
          return Mux(signA ^ signB, -prod, prod)
        case "s/s" =>
          val (signA, absA) = signAbs(a)
          val (signB, absB) = signAbs(b)
          val quo = absA / absB
          return Mux(signA != signB, -quo, quo)
        case "s%s" =>
          val (signA, absA) = signAbs(a)
          val (signB, absB) = signAbs(b)
          val rem = absA % absB
          return Mux(signA, -rem, rem)
        case "%" =>
          val (au, bu) = (a.asInstanceOf[UFix], b.asInstanceOf[UFix])
          return Op("-", nGrow, widthInfer, au, au/bu*bu)
        case _ =>
      }
    }
    val res = new Op();
    res.init("", widthInfer, a, b);
    res.op = name;
    res.nGrow = nGrow;
    if (isSigned) res.setIsSigned
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

  override def toString: String =
    if (inputs.length == 1) {
      op + "(" + inputs(0) + ")"
    } else {
      "[ " + inputs(0) + "\n]\n  " + op + "\n" + "[  " + inputs(1) + "\n]"
    }

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
