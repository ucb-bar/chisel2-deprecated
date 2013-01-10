package Chisel
import scala.collection.mutable.ArrayBuffer
import scala.math.max
import scala.math.min
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
    case "-"  => Op("-",  1, widthOf(0), x);
    case "~"  => Op("~",  1, widthOf(0), x);
    case "!"  => Op("!",  1, fixWidth(1), x);
    case "f-" => Op("f-", 1, fixWidth(32), x);
    case "fsin" => Op("fsin", 1, fixWidth(32), x);
    case "fcos" => Op("fcos", 1, fixWidth(32), x);
    case "flog" => Op("flog", 1, fixWidth(32), x);
    case "ffloor" => Op("ffloor", 1, fixWidth(32), x);
    case "fceil" => Op("fceil", 1, fixWidth(32), x);
    case "fround" => Op("fround", 1, fixWidth(32), x);
    case "d-" => Op("d-", 1, fixWidth(64), x);
    case "dsin" => Op("dsin", 1, fixWidth(64), x);
    case "dcos" => Op("dcos", 1, fixWidth(64), x);
    case "dlog" => Op("dlog", 1, fixWidth(64), x);
    case "dfloor" => Op("dfloor", 1, fixWidth(64), x);
    case "dceil" => Op("dceil", 1, fixWidth(64), x);
    case "dround" => Op("dround", 1, fixWidth(64), x);
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
      case "/"   => Op("/",  0, widthOf(0),  x, y );
      case "s/s" => Op("s/s",  0, widthOf(0),  x, y );
      case "s/u" => Op("s/u",  0, widthOf(0),  x, y );
      case "u/s" => Op("u/s",  0, widthOf(0),  x, y );
      case "%"   => Op("%",  0, minWidth _,  x, y );
      case "s%s" => Op("s%s",  0, minWidth _,  x, y );
      case "s%u" => Op("s%u",  0, minWidth _,  x, y );
      case "u%s" => Op("u%s",  0, minWidth _,  x, y );
      case "^"   => Op("^",  2, maxWidth _,  x, y );
      case "?"   => Multiplex(x, y, null);
      case "-"   => Op("-",  2, maxWidth _,  x, y );
      case "##"  => Op("##", 2, sumWidth _,  x, y );
      case "&"   => Op("&",  2, maxWidth _, x, y );
      case "|"   => Op("|",  2, maxWidth _, x, y );
      case "f+"  => Op("f+", 2, fixWidth(32), x, y );
      case "f-"  => Op("f-", 2, fixWidth(32), x, y );
      case "f*"  => Op("f*", 0, fixWidth(32), x, y );
      case "f/"  => Op("f/", 0, fixWidth(32), x, y );
      case "f%"  => Op("f%", 0, fixWidth(32), x, y );
      case "fpow"  => Op("fpow", 0, fixWidth(32), x, y );
      case "d+"  => Op("d+", 2, fixWidth(64), x, y );
      case "d-"  => Op("d-", 2, fixWidth(64), x, y );
      case "d*"  => Op("d*", 0, fixWidth(64), x, y );
      case "d/"  => Op("d/", 0, fixWidth(64), x, y );
      case "d%"  => Op("d%", 0, fixWidth(64), x, y );
      case "dpow"  => Op("dpow", 0, fixWidth(64), x, y );
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
        case "f==" => Op("f==", 2, fixWidth(1), x, y );
        case "f!=" => Op("f!=", 2, fixWidth(1), x, y );
        case "f>"  => Op("f>",  2, fixWidth(1), x, y );
        case "f<"  => Op("f<",  2, fixWidth(1), x, y );
        case "f<=" => Op("f<=", 2, fixWidth(1), x, y );
        case "f>=" => Op("f>=", 2, fixWidth(1), x, y );
        case "d==" => Op("d==", 2, fixWidth(1), x, y );
        case "d!=" => Op("d!=", 2, fixWidth(1), x, y );
        case "d>"  => Op("d>",  2, fixWidth(1), x, y );
        case "d<"  => Op("d<",  2, fixWidth(1), x, y );
        case "d<=" => Op("d<=", 2, fixWidth(1), x, y );
        case "d>=" => Op("d>=", 2, fixWidth(1), x, y );
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
    // println("REDUCTION OP " + op)
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
  var sn_depth = 0;
  def apply (name: String, nGrow: Int, widthInfer: (Node) => Int, a: Node, b: Node): Node = {
    val (a_lit, b_lit) = (a.litOf, b.litOf);
    if (isFolding) {
    if (a_lit != null && b_lit == null) {
      name match {
        case "&&" => return if (a_lit.value == 0) Literal(0) else b;
        case "||" => return if (a_lit.value == 0) b else Literal(1);
        case "&" => if (a_lit.value == 0) return Literal(0);
        case "|" => if (a_lit.value == 0) return b;
        case _ => ;
      }
    } else if (a_lit == null && b_lit != null) {
      name match {
        case "&&" => return if (b_lit.value == 0) Literal(0) else a;
        case "||" => return if (b_lit.value == 0) a else Literal(1);
        case "&" => if (b_lit.value == 0) return Literal(0);
        case "|" => if (b_lit.value == 0) return a;
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
    if (a.isInstanceOf[Flo] && b.isInstanceOf[Flo]) {
      val (fa, fb) = (a.asInstanceOf[Flo], b.asInstanceOf[Flo]);
      if (fa.floLitOf != null && fb.floLitOf != null) { 
      val (fa_val, fb_val) = (fa.floLitOf.floValue, fb.floLitOf.floValue);
      name match {
        case "f+" => return FloLit(fa_val + fb_val);
        case "f-" => return FloLit(fa_val - fb_val);
        case "f*" => return FloLit(fa_val * fb_val);
        case "f/" => return FloLit(fa_val / fb_val);
        case "f%" => return FloLit(fa_val % fb_val);
        case "f==" => return Bool(fa_val == fb_val);
        case "f!=" => return Bool(fa_val != fb_val);
        case "f>" => return Bool(fa_val > fb_val);
        case "f<" => return Bool(fa_val < fb_val);
        case "f>=" => return Bool(fa_val >= fb_val);
        case "f<=" => return Bool(fa_val <= fb_val);
        case _ => ;
      }
      } else if (fa.floLitOf != null) { 
        val fa_val = fa.floLitOf.floValue;
        if (fa_val == 0.0) {
          name match {
            case "f+" => return b;
            case "f*" => return FloLit(0.0.toFloat);
            case "f/" => return FloLit(0.0.toFloat);
            case _ => ;
          }
        } else if (fa_val == 1.0) {
          name match {
            case "f*" => return b;
            case _ => ;
          }
        }        
      } else if (fb.floLitOf != null) { 
        val fb_val = fb.floLitOf.floValue;
        if (fb_val == 0.0) {
          name match {
            case "f+" => return a;
            case "f*" => return FloLit(0.0.toFloat);
            case _ => ;
          }
        } else if (fb_val == 1.0) {
          name match {
            case "f*" => return a;
            case "f/" => return a;
            case "f%" => return a;
            case _ => ;
          }
        }        
      }
    }
      
    if (a.isInstanceOf[Dbl] && b.isInstanceOf[Dbl]) {
      val (fa, fb) = (a.asInstanceOf[Dbl], b.asInstanceOf[Dbl]);
      // println("TRYING TO FOLD " + name + " FAL " + (if (fa.dblLitOf == null) fa.toString else fa.dblLitOf.dblValue.toString) + " FBL " + (if (fb.dblLitOf == null) fb.toString else fb.dblLitOf.dblValue.toString))
      if (fa.dblLitOf != null && fb.dblLitOf != null) {
      val (fa_val, fb_val) = (fa.dblLitOf.dblValue, fb.dblLitOf.dblValue);
        // println(" FOLDING " + name + " " + fa_val + " " + fb_val);
      name match {
        case "d+" => return DblLit(fa_val + fb_val);
        case "d-" => return DblLit(fa_val - fb_val);
        case "d*" => return DblLit(fa_val * fb_val);
        case "d/" => return DblLit(fa_val / fb_val);
        case "d%" => return DblLit(fa_val % fb_val);
        case "d==" => return Bool(fa_val == fb_val);
        case "d!=" => return Bool(fa_val != fb_val);
        case "d>" => return Bool(fa_val > fb_val);
        case "d<" => return Bool(fa_val < fb_val);
        case "d>=" => return Bool(fa_val >= fb_val);
        case "d<=" => return Bool(fa_val <= fb_val);
        case _ => ;
      }
    } else if (fa.dblLitOf != null) { 
      val fa_val = fa.dblLitOf.dblValue;
      // println("FA " + fa_val + " NAME " + name);
      if (fa_val == 0.0) {
        // println("FOLDING " + name);
        name match {
          case "d+" => return b;
          case "d*" => return DblLit(0.0);
          case "d/" => return DblLit(0.0);
          case "d%" => return DblLit(0.0);
          case _ => ;
        }
      } else if (fa_val == 1.0) {
        // println("FOLDING " + name);
        name match {
          case "d*" => return b;
          case _ => ;
        }
      }        
    } else if (fb.dblLitOf != null) { 
      val fb_val = fb.dblLitOf.dblValue;
      // println("FB " + fb_val + " NAME " + name);
      if (fb_val == 0.0) {
        // println("FOLDING " + name);
        name match {
          case "d+" => return a;
          case "d*" => return DblLit(0.0);
          case _ => ;
        }
      } else if (fb_val == 1.0) {
        // println("FOLDING " + name);
        name match {
          case "d*" => return a;
          case "d/" => return a;
          case "d%" => return a;
          case _ => ;
        }
      }        
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
          if (a.litOf != null && a.litOf.isZ)
            return Op(name, nGrow, widthInfer, b, a)
        case "s*s" =>
          val (signA, absA) = signAbs(a)
          val (signB, absB) = signAbs(b)
          val prod = absA * absB
          return Mux(signA ^ signB, -prod, prod)
        case "s*u" =>
          val (signA, absA) = signAbs(a)
          val prod = absA * b.asInstanceOf[UFix]
          return Mux(signA, -prod, prod)
        case "u*s" =>
          return Op("s*u", nGrow, widthInfer, b, a)
        case "s/s" =>
          val (signA, absA) = signAbs(a)
          val (signB, absB) = signAbs(b)
          val quo = absA / absB
          return Mux(signA != signB, -quo, quo)
        case "s/u" =>
          val (signA, absA) = signAbs(a)
          val quo = absA / b.asInstanceOf[UFix]
          return Mux(signA, -quo, quo)
        case "u/s" =>
          val (signB, absB) = signAbs(b)
          val quo = a.asInstanceOf[UFix] / absB
          return Mux(signB, -quo, quo)
        case "s%s" =>
          val (signA, absA) = signAbs(a)
          val (signB, absB) = signAbs(b)
          val rem = absA % absB
          return Mux(signA, -rem, rem)
        case "u%s" =>
          return a.asInstanceOf[UFix] / signAbs(b)._2
        case "s%u" =>
          val (signA, absA) = signAbs(a)
          val rem = absA % b.asInstanceOf[UFix]
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
    if(a.isSigned && b.isSigned) res.setIsSigned
    res
  }
  def apply (name: String, nGrow: Int, widthInfer: (Node) => Int, a: Node): Node = {
    if (isFolding) {
    if (a.litOf != null) {
      name match {
        case "!" => return if (a.litOf.value == 0) Literal(1) else Literal(0);
        case "-" => return Literal(-a.litOf.value, a.litOf.width);
        case "~" => return Literal((-a.litOf.value-1)&((BigInt(1) << a.litOf.width)-1), a.litOf.width);
        case _ => ;
      } 
    }
    if (a.isInstanceOf[Dbl]) { 
      val fa = a.asInstanceOf[Dbl];
      if (fa.dblLitOf != null) {
      val fa_val = fa.dblLitOf.dblValue;
      name match {
        case "dsin" => return DblLit(Math.sin(fa_val));
        case "dlog" => return DblLit(Math.log(fa_val));
        case "dfloor" => return DblLit(Math.floor(fa_val));
        case "dceil" => return DblLit(Math.ceil(fa_val));
        case "dround" => return DblLit(Math.round(fa_val));
        case "dToFix" => return Literal(fa_val.toInt);
        case _ => ;
      }
      }
    }
    if (a.isInstanceOf[Flo]) {
      val fa = a.asInstanceOf[Flo];
      if (fa.floLitOf != null) {
      val fa_val = fa.floLitOf.floValue;
      name match {
        case "fsin" => return FloLit(Math.sin(fa_val).toFloat);
        case "flog" => return FloLit(Math.log(fa_val).toFloat);
        case "ffloor" => return DblLit(Math.floor(fa_val).toFloat);
        case "fceil" => return DblLit(Math.ceil(fa_val).toFloat);
        case "fround" => return DblLit(Math.round(fa_val).toFloat);
        case "fToFix" => return Literal(fa_val.toLong);
        case _ => ;
      }
      }
    }
    }
    val res = new Op();
    res.init("", widthInfer, a);
    res.op = name;
    res.nGrow = nGrow;
    res
  }
  def apply (op: String, width: Int, a: Node): Node = {
    val res = Op(op, 0, fixWidth(width), a)
    res.width_ = width
    res
  }
  def apply (op: String, width: Int, a: Node, b: Node): Node = {
    val res = Op(op, 0, fixWidth(width), a, b)
    res.width_ = width
    res
  }
}

object Trunc {
  def apply(x: Node) = {
    val bpw = backend.wordBits;
    val nw  = backend.words(x)
    val nfw = backend.fullWords(x)
    if (nw != nfw) {
      val w = (x.width-bpw*nfw)
      x.setSubNode(nw-1, Op("&", w, x.getSubNode(nw-1), Literal((1L << w)-1, w)))
    }
  }
}    

import Op._

class Op extends Node {
  var op: String = "";
  var nGrow: Int = 0;
  override def dotName = if (op == "") "?" else op;
  override def toString: String =
    if (inputs.length == 1)
      op + "(" + inputs(0) + ")"
    else
      inputs(0) + " " + op + " " + inputs(1)
      // "[ " + inputs(0) + "\n]\n  " + op + "\n" + "[  " + inputs(1) + "\n]"

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

  def maskVal(x: Node, i: Int) = {
    val bpw = backend.wordBits;
    if ((x.width - i*bpw) < bpw) {
      Literal((1L << (x.width - i*bpw))-1)
    } else {
      Literal(-1, bpw)
    }
  }
    
  override def genSubNodes: Unit = {
    val bpw = backend.wordBits;
    sn_depth += 1;
    // for (i <- 0 until sn_depth) print("  ")
    // println("EXPAND SUBWORDS " + this + " EXISTING SUBWORDS " + subnodes.length)
    if (inputs.length == 1) {
      val maxWordWidth = 
      if (op == "!") {
        setSubNode(0, Op("!", 1, inputs(0).getSubNode(0)))
      } else if (op == "|") {
        setSubNode(0, Op("!=", 1, Literal(0), (0 until backend.words(inputs(0))).map(inputs(0).getSubNode(_)).reduceLeft(Op("|", backend.thisWordBits(inputs(0), 0), _, _))))
      } else if (op == "&") { 
        setSubNode(0, (0 until backend.words(inputs(0))).map(i => Op("==", 1, inputs(0).getSubNode(i), maskVal(inputs(0), i))).reduceLeft(Op("&", 1, _, _)))
      } else if (op == "^") { 
        var x = (0 until backend.words(inputs(0))).map(inputs(0).getSubNode(_)).reduceLeft(Op("^", backend.thisWordBits(inputs(0), 0), _, _))
        for (i <- log2Up(min(bpw, inputs(0).width))-1 to 0 by -1)
          x = Op("^", bpw, Op(">>", bpw, x, Literal(1L << i)), x)
        setSubNode(0, Op("&", 1, x, Literal(1)))
      } else if (op == "~") { 
        for (i <- 0 until backend.words(this))
          setSubNode(i, Op("~", backend.thisWordBits(inputs(0), i), inputs(0).getSubNode(i)))
        Trunc(this)
      } else if (op == "-") { 
        setSubNode(0, Op("-", backend.thisWordBits(this, 0), inputs(0).getSubNode(0)))
        var borrow: Node = Literal(0);
        for (i <- 1 until backend.words(this)) {
          val neg   = Op("-", backend.thisWordBits(this, 0), inputs(0).getSubNode(i))
          setSubNode(i, Op("-", backend.thisWordBits(this, i-1), neg, borrow))
          borrow    = Op("||", 1, inputs(0).getSubNode(i), this.getSubNode(i))
        } 
        Trunc(this)
      } else 
        super.genSubNodes
    } else if (inputs.length == 2) {
      if (op == "==") {
        setSubNode(0, (0 until backend.words(inputs(0))).map(i => Op("==", 1, inputs(0).getSubNode(i), inputs(1).getSubNode(i))).reduceLeft(Op("&&", 1, _, _)))
      } else if (op == "!=") {
        setSubNode(0, (0 until backend.words(inputs(0))).map(i => Op("!=", 1, inputs(0).getSubNode(i), inputs(1).getSubNode(i))).reduceLeft(Op("||", 1, _, _)))
      } else if (op == "<<") { // TODO: 
        if (width <= bpw)
          setSubNode(0, Op("<<", width, inputs(0).getSubNode(0), inputs(1).getSubNode(0)))
        else {
          val amount         = inputs(1).getSubNode(0)
          var carry          = Literal(0)
          val nShiftBits     = RawExtract(amount, log2Up(bpw)-1, 0); 
          val nShiftWords    = Op(">>", bpw, amount, Literal(log2Up(bpw))); 
          val nRevShiftBits  = Op("-",  bpw, bpw, nShiftBits); 
          val isZeroCarry    = Op("==", 1, nShiftBits, Literal(0)); 
          val nWords         = backend.words(this)
          val lookups        = new ArrayBuffer[Node]()
          for (i <- 0 until nWords) {
            var lookup: Node = inputs(0).getSubNode(i) 
            // println("LOOKUP" + i + " " + lookup)
            for (del <- 1 until nWords) {
              // println("  DEL " + del + " I-DEL " + (i-del) + " VAL " + inputs(0).getSubNode(i-del))
              val res = if ((i-del) < 0) Literal(0) else inputs(0).getSubNode(i-del)
              lookup  = Multiplex(Op("==", 1, nShiftWords, Literal(del)), res, lookup)
            }
            lookups += lookup
          }
          for (i <- 0 until nWords) {
            val x     = Op("<<", bpw, lookups(i), nShiftBits)
            val c     = if (i == 0)
                          Literal(0)
                        else {
                          // println("NWORDS " + nWords + " LOOKUPS LENGTH " + lookups.length + " I+1 " + (i+1))
                          Multiplex(isZeroCarry, Literal(0), Op(">>", bpw, lookups(i-1), nRevShiftBits)) 
                        }
            setSubNode(i, Op("|", bpw, x, c))
          }
        }
      } else if (op == ">>") { 
        if (width <= bpw) {
          if (isSigned) {
            val x = Op("<<", bpw, inputs(0).getSubNode(0), Literal(bpw - inputs(0).width));  x.isSigned = true;
            setSubNode(0, Op(">>", width, x, Op("+", bpw, Literal(bpw - inputs(0).width), inputs(1).getSubNode(0))))
            Trunc(this)
          } else 
            setSubNode(0, Op(">>", width, inputs(0).getSubNode(0), inputs(1).getSubNode(0)))
        } else {
          // println(">> IS SIGNED " + isSigned)
          val amount        = inputs(1).getSubNode(0)
          val revAmount     = Op("-", bpw, Literal(width-1), amount)
          var carry         = Literal(0)
          val nShiftBits    = RawExtract(amount, log2Up(bpw)-1, 0)
          val nShiftWords   = Op(">>", bpw, amount, Literal(log2Up(bpw))) 
          val nRevShiftBits = Op("-",  bpw, bpw, nShiftBits)
          val isZeroCarry   = Op("==", 1,   nShiftBits, Literal(0))
          val nWords        = backend.words(this)
          val lookups       = new ArrayBuffer[Node]()
          for (i <- 0 until nWords) {
            var lookup: Node  = inputs(0).getSubNode(i) 
            for (del <- 1 until nWords) {
              val res = if (del >= (nWords-i)) Literal(0) else inputs(0).getSubNode(i+del)
              lookup = Multiplex(Op("==", 1, nShiftWords, Literal(del)), res, lookup)
            }
            lookups += lookup
          }
          lookups += Literal(0)
          val msw  = inputs(0).getSubNode(backend.words(this)-1); // most significant word
          val tb   = (if (width % bpw != 0) Op("<<", bpw, msw, Literal(bpw-width%bpw)) else msw);  tb.isSigned = true;  // top bit
          val msb  = Op(">>", bpw, tb, Literal(bpw-1));           // move msb all the way to fill entire word using rsha
          val fill = Op("<<", bpw, msb, RawExtract(revAmount, log2Up(bpw)-1, 0)); // fill top bits -- revAmount % bpw
          for (i <- 0 until nWords) {
            val x     = Op(">>", bpw, lookups(i), nShiftBits)
            val c     = Multiplex(isZeroCarry, Literal(0), Op("<<", bpw, lookups(i+1), nRevShiftBits)) 
            setSubNode(i, Op("|", bpw, x, c))
            if (isSigned) {
              // println("SIGNED")
              setSubNode(i, Op("|", bpw, subnodes(i),
                              Op("|", bpw, Mask(Op(">", 1, Literal((i+1)*bpw), revAmount), fill), // last word?
                                 Mask(Op(">=", 1, Literal(i*bpw), revAmount), msb))))             // mid words
            }
          }
          if (isSigned) {
            setSubNode(nWords-1, Op("|", bpw, subnodes(nWords-1), Mask(Op(">", 1, Literal(bpw), revAmount), fill)))
            Trunc(this)
          }
        }
      } else if (op == "##") { // TODO: check 
        val lsh = inputs(1).width
        for (i <- 0 until backend.fullWords(inputs(1)))
          setSubNode(i, inputs(1).getSubNode(i))
        if (lsh%bpw != 0) {
          val idx = backend.fullWords(inputs(1));
          if (isInObject) {
            val i1 = inputs(1).getSubNode(idx);
            i1.width_ = lsh % bpw;
            setSubNode(idx, Op("##", bpw, inputs(0).getSubNode(0), i1))
          } else
            setSubNode(idx, Op("|", bpw, Op("<<", bpw, inputs(0).getSubNode(0), Literal(lsh % bpw)), inputs(1).getSubNode(idx)));
        }
        for (i <- backend.words(inputs(1)) until backend.words(this)) {
          val sni = (bpw*i-lsh)/bpw
          val a   = inputs(0).getSubNode(sni)
          val aw  = backend.thisWordBits(inputs(0), sni)
          if (lsh % bpw != 0) {
            val rsh = Op(">>", aw, a, bpw - lsh % bpw)
            if  ((bpw*i-lsh)/bpw+1 < backend.words(inputs(0))) {
              setSubNode(i, Op("<<", backend.wordBits, Op("|", aw, rsh, inputs(0).getSubNode(sni)), (lsh%bpw)))
            } else
              setSubNode(i, rsh)
          } else
            setSubNode(i, a)
        }
        // println("  EXPANDED INTO " + subnodes.length + " SUBNODES " + subnodes(0) + " NAME " + subnodes(0).name)
      } else if (op == "&" || op == "|" || op == "^" || op == "||" || op == "&&") {
        for (i <- 0 until backend.words(this)) 
          setSubNode(i, Op(op, backend.thisWordBits(this, i), inputs(0).getSubNode(i), inputs(1).getSubNode(i)))
      } else if (op == "<" && isSigned) {
        require(!isFolding || inputs(1).litOf.value == 0)
        val shamt = (inputs(0).width-1) % bpw
        setSubNode(0, Op("&", 1, Op(">>", bpw, inputs(0).getSubNode(backend.words(inputs(0))-1), Literal(shamt)), Literal(1)))
      } else if (op == "<" || op == ">" || op == "<=" || op == ">=") {
        require(!isSigned)
        var res = Op(op, backend.thisWordBits(inputs(0), 0), inputs(0).getSubNode(0), inputs(1).getSubNode(0))
        // println("*** THIS " + this + " WIDTH " + this.width + " WORDS " + backend.words(this))
        for (i <- 1 until backend.words(inputs(0))) {
          val a = inputs(0).getSubNode(i);
          val b = inputs(1).getSubNode(i);
          val w = backend.thisWordBits(inputs(0), i);
          res = Op("&&", 1, res, Op("||", 1, Op("==", w, a, b), Op(op, w, a, b)))
        }
        setSubNode(0, res)
      } else if (op == "-") { // TODO: MERGE WITH BELOW
        var prev   = Op(op, backend.thisWordBits(this, 0), inputs(0).getSubNode(0), inputs(1).getSubNode(0))
        setSubNode(0, prev)
        var prevWithCarry: Node = null
        for (i <- 1 until backend.words(this)) {
          var carry = Op(">", backend.thisWordBits(this, i-1), prev, inputs(0).getSubNode(i-1))
          if (i > 1) {
            val carry2 = Op("<", backend.thisWordBits(this, i-1), prev, prevWithCarry)
            carry = Op("||", 1, carry, carry2)
          }
          prev          = Op(op, backend.thisWordBits(this, i), inputs(0).getSubNode(i), inputs(1).getSubNode(i))
          prevWithCarry = Op(op, backend.thisWordBits(this, i), prev, carry)
          setSubNode(i, prevWithCarry)
        }
        Trunc(this)
      } else if (op == "+") { 
        var prev = Op(op, backend.thisWordBits(this, 0), inputs(0).getSubNode(0), inputs(1).getSubNode(0))
        setSubNode(0, prev)
        var prevWithCarry: Node = null
        for (i <- 1 until backend.words(this)) {
          var carry = Op("<", backend.thisWordBits(this, i-1), prev, inputs(0).getSubNode(i-1))
          if (i > 1) {
            val carry2 = Op(">", backend.thisWordBits(this, i-1), prevWithCarry, prev)
            carry = Op("||", 1, carry, carry2)
          }
          prev          = Op(op, backend.thisWordBits(this, i), inputs(0).getSubNode(i), inputs(1).getSubNode(i))
          prevWithCarry = Op(op, backend.thisWordBits(this, i), prev, carry)
          setSubNode(i, prevWithCarry)
        }
        Trunc(this)
      } else if (op == "*") {
        // TODO: CHECK THIS OUT
        val bph = bpw/2
        val m = backend.words(inputs(0))*2;
        val n = backend.words(inputs(1))*2;
        val u = new Array[Node](m);
        val v = new Array[Node](n);
        for (i <- 0 until m/2) { 
          u(2*i)   = RawExtract(inputs(0).getSubNode(i), bph-1, 0);
          u(2*i+1) = RawExtract(inputs(0).getSubNode(i), bpw-1, bph);
        }
        for (i <- 0 until n/2) { 
          v(2*i)   = RawExtract(inputs(1).getSubNode(i), bph-1, 0);
          v(2*i+1) = RawExtract(inputs(1).getSubNode(i), bpw-1, bph);
        }
        val w = new Array[Node](n*m+1);
        // println("W SIZE " + w.length)
        for (i <- 0 until n*m) w(i) = Literal(0);
        for (j <- 0 until n) {
          var k: Node = Literal(0);
          for (i <- 0 until m) {
            val t = Op("+", bpw, w(i*j), Op("+", bpw, Op("*", bpw, u(i), v(j)), k))
            w(i+j) = t;
            k = Op(">>", bpw, t, bph);
          }
          // println("J " + j + " M " + m)
          w(j+m) = k
        }
        for (i <- 0 until (n*m)/2)
          setSubNode(i, RawCat(RawExtract(w(2*i), bph-1, 0), RawExtract(w(2*i+1), bph-1, 0)))
      } else 
        super.genSubNodes
    } else
      super.genSubNodes
    // for (i <- 0 until sn_depth) print("  ")
    // println("-> " + subnodes.length + " SUBNODES " + this)
    sn_depth -= 1;
  }
}
