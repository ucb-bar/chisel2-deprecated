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
  def apply (op: String, width: Int, a: Node): Node = {
    val res = new Op
    res.init(op, width, a)
    res.op = op
    res
  }
  def apply (op: String, width: Int, a: Node, b: Node): Node = {
    val res = new Op
    // if (op == ">>") println("RSH WIDTH " + width + " ON " + op + " A " + a + " B " + b)
    res.init(op, width, a, b)
    res.op = op
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
      x.subnodes(nw-1) = Op("&", w, x.getSubNode(nw-1), Literal((1L << w)-1, w))
    }
  }
}    

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
    if (inputs(0).width - i*bpw < bpw) 
      Literal((1L << (inputs(0).width - i*bpw)-1))
    else
      Literal(-1)
  }
    
  override def genSubNodes: Unit = {
    val bpw = backend.wordBits;
    if (inputs.length == 1) {
      val maxWordWidth = 
      if (op == "!") {
        subnodes += Op("!", 1, inputs(0).getSubNode(0))
      } else if (op == "|") {
        subnodes += (0 until backend.words(this)).map(inputs(0).getSubNode(_)).reduceLeft(Op("|", backend.thisWordBits(inputs(0), 0), _, _))
      } else if (op == "&") { 
        subnodes += (0 until backend.words(this)).map(i => Op("==", 1, inputs(0).getSubNode(i), maskVal(inputs(0), i))).reduceLeft(Op("&", backend.thisWordBits(inputs(0), 0),_, _))
      } else if (op == "^") { 
        var x = (0 until backend.words(this)).map(inputs(0).getSubNode(_)).reduceLeft(Op("^", backend.thisWordBits(inputs(0), 0), _, _))
        for (i <- log2Up(min(bpw, inputs(0).width))-1 to 0 by -1)
          x = Op("^", bpw, Op(">>", bpw, x, Literal(1L << i)), x)
        subnodes += Op("&", 1, x, Literal(1))
      } else if (op == "~") { 
        subnodes ++= (0 until backend.words(this)).map(i => Op("~", backend.thisWordBits(inputs(0), i), inputs(0).getSubNode(i)))
        Trunc(this)
      } else if (op == "-") { 
        val first = Op("-", backend.thisWordBits(this, 0), inputs(0).getSubNode(0))
        subnodes += first

        var borrow: Node = Literal(0);
        for (i <- 1 until backend.words(this)) {
          val neg   = Op("-", backend.thisWordBits(this, 0), inputs(0).getSubNode(i))
          subnodes += Op("-", backend.thisWordBits(this, i-1), neg, borrow)
          borrow    = Op("||", 1, inputs(0).getSubNode(i), this.getSubNode(i))
        } 
        Trunc(this)
      } else 
        super.genSubNodes
    } else if (inputs.length == 2) {
      if (op == "==") {
        subnodes += (0 until backend.words(inputs(0))).map(i => Op("==", 1, inputs(0).getSubNode(i), inputs(1).getSubNode(i))).reduceLeft(Op("&&", 1, _, _))
      } else if (op == "!=") {
        subnodes += (0 until backend.words(inputs(0))).map(i => Op("==", 1, inputs(0).getSubNode(i), inputs(1).getSubNode(i))).reduceLeft(Op("||", 1, _, _))
      } else if (op == "<<") { // TODO: 
        if (width <= bpw)
          subnodes += Op("<<", width, inputs(0).getSubNode(0), inputs(1).getSubNode(0))
        else {
          val amount         = inputs(1).getSubNode(0)
          var carry          = Literal(0)
          val nShiftBits     = RawExtract(amount, log2Up(bpw)-1, 0); // nShiftBits.setName("nsb");
          val nShiftWords    = Op(">>", bpw, amount, Literal(log2Up(bpw))); // nShiftWords.setName("nsw");  // println("NSW " + nShiftWords)
          val nRevShiftBits  = Op("-",  bpw, bpw, nShiftBits); // nRevShiftBits.setName("nrsb");
          val isZeroCarry    = Op("==", 1, nShiftBits, Literal(0)); // isZeroCarry.setName("izc");
          val nWords         = backend.words(this)
          val lookups        = new ArrayBuffer[Node]()
          for (i <- 0 until nWords) {
            var lookup: Node = inputs(0).getSubNode(i) 
            println("LOOKUP" + i + " " + lookup)
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
                          Multiplex(isZeroCarry, Literal(0), Op(">>", bpw, lookups(i-1), nRevShiftBits)) // TODO: NEED MASK
                        }
            subnodes += Op("|", bpw, x, c)
          }
        }
      } else if (op == ">>") { 
        if (width <= bpw)
          subnodes += Op(">>", width, inputs(0).getSubNode(0), inputs(1).getSubNode(0))
        else {
          val amount         = inputs(1).getSubNode(0)
          var carry          = Literal(0)
          val nShiftBits     = RawExtract(amount, log2Up(bpw)-1, 0)
          val nShiftWords    = Op(">>", bpw, amount, Literal(log2Up(bpw))) 
          val nRevShiftBits  = Op("-",  bpw, bpw, nShiftBits)
          val isZeroCarry    = Op("==", 1,   nShiftBits, Literal(0))
          val nWords         = backend.words(this)
          val lookups        = new ArrayBuffer[Node]()
          for (i <- 0 until nWords) {
            var lookup: Node  = inputs(0).getSubNode(i) 
            for (del <- 1 until nWords) {
              val res = if (del >= (nWords-i)) Literal(0) else inputs(0).getSubNode(i+del)
              lookup = Multiplex(Op("==", 1, nShiftWords, Literal(del)), res, lookup)
            }
            lookups += lookup
          }
          lookups += Literal(0)
          for (i <- 0 until nWords) {
            val x     = Op(">>", bpw, lookups(i), nShiftBits)
            val c     = Multiplex(isZeroCarry, Literal(0), Op("<<", bpw, lookups(i+1), nRevShiftBits)) // TODO: NEED MASK
            subnodes += Op("|", bpw, x, c)
          }
        }
      } else if (op == "##") { // TODO: check 
        val lsh = inputs(1).width
        subnodes ++= (0 until backend.fullWords(inputs(1))).map(inputs(1).getSubNode(_))
        println("EXPANDING ## " + lsh + " FULLWORDS " + backend.fullWords(inputs(1)))
        if (lsh%bpw != 0) {
          println("LSH%BPW != 0 " + inputs(1).getSubNode(backend.fullWords(inputs(1))))
          subnodes += Op("|", bpw, inputs(1).getSubNode(backend.fullWords(inputs(1))),  Op("<<", bpw, inputs(0).getSubNode(0), Literal(lsh % bpw)));
        }
        for (i <- backend.words(inputs(1)) until backend.words(this)) {
          println("LOOPING")
          val sni = (bpw*i-lsh)/bpw
          val a   = inputs(0).getSubNode(sni)
          val aw  = backend.thisWordBits(inputs(0), sni)
          if (lsh % bpw != 0) {
            val rsh = Op(">>", aw, a, bpw - lsh % bpw)
            if  ((bpw*i-lsh)/bpw+1 < backend.words(inputs(0))) {
              subnodes += Op("<<", backend.wordBits, Op("|", aw, rsh, inputs(0).getSubNode(sni)), (lsh%bpw))
            } else
              subnodes += rsh
          } else
            subnodes += a
        }
      } else if (op == "&" || op == "|" || op == "^" || op == "||" || op == "&&") {
        for (i <- 0 until backend.words(this)) 
          subnodes += Op(op, backend.thisWordBits(this, i), inputs(0).getSubNode(i), inputs(1).getSubNode(i));
      } else if (op == "<" && isSigned) {
        require(!isFolding || inputs(1).litOf.value == 0)
        val shamt = (inputs(0).width-1) % bpw
        subnodes += Op("&", 1, Op(">>", bpw, inputs(0).getSubNode(backend.words(inputs(0))-1), Literal(shamt)), Literal(1))
      } else if (op == "<" || op == ">" || op == "<=" || op == ">=") {
        require(!isSigned)
        var res = Op(op, backend.thisWordBits(inputs(0), 0), inputs(0).getSubNode(0), inputs(1).getSubNode(0))
        println("*** THIS " + this + " WIDTH " + this.width + " WORDS " + backend.words(this))
        for (i <- 1 until backend.words(inputs(0))) {
          val a = inputs(0).getSubNode(i);
          val b = inputs(1).getSubNode(i);
          val w = backend.thisWordBits(inputs(0), i);
          res = Op("&&", 1, res, Op("||", 1, Op("==", w, a, b), Op(op, w, a, b)))
        }
        subnodes += res
      } else if (op == "-") { // TODO: MERGE WITH BELOW
        var prev   = Op(op, backend.thisWordBits(this, 0), inputs(0).getSubNode(0), inputs(1).getSubNode(0))
        subnodes += prev
        var prevWithCarry: Node = null
        for (i <- 1 until backend.words(this)) {
          var carry = Op(">", backend.thisWordBits(this, i-1), prev, inputs(0).getSubNode(i-1))
          if (i > 1) {
            val carry2 = Op("<", backend.thisWordBits(this, i-1), prev, prevWithCarry)
            carry = Op("||", 1, carry, carry2)
          }
          prev          = Op(op, backend.thisWordBits(this, i), inputs(0).getSubNode(i), inputs(1).getSubNode(i))
          prevWithCarry = Op(op, backend.thisWordBits(this, i), prev, carry)
          subnodes     += prevWithCarry
        }
        Trunc(this)
      } else if (op == "+") { 
        var prev = Op(op, backend.thisWordBits(this, 0), inputs(0).getSubNode(0), inputs(1).getSubNode(0))
        subnodes += prev
        var prevWithCarry: Node = null
        for (i <- 1 until backend.words(this)) {
          var carry = Op("<", backend.thisWordBits(this, i-1), prev, inputs(0).getSubNode(i-1))
          if (i > 1) {
            val carry2 = Op(">", backend.thisWordBits(this, i-1), prevWithCarry, prev)
            carry = Op("||", 1, carry, carry2)
          }
          prev          = Op(op, backend.thisWordBits(this, i), inputs(0).getSubNode(i), inputs(1).getSubNode(i))
          prevWithCarry = Op(op, backend.thisWordBits(this, i), prev, carry)
          subnodes     += prevWithCarry
        }
        Trunc(this)
      } else if (op == "*") {
        val bph = bpw/2
        val m = backend.halfWords(inputs(0));
        val n = backend.halfWords(inputs(1));
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
        val w = new Array[Node](n*m);
        for (i <- 0 until n*m) w(i) = Literal(0);
        for (j <- 0 until n) {
          var k: Node = Literal(0);
          for (i <- 0 until m) {
            val t = Op("+", bpw, w(i*j), Op("+", bpw, Op("*", bpw, u(i), v(j)), k))
            w(i+j) = t;
            k = Op(">>", bpw, t, bph);
          }
          w(j+m) = k
        }
        for (i <- 0 until (n*m))
          subnodes += RawCat(RawExtract(w(2*i), bph-1, 0), RawExtract(w(2*i+1), bph-1, 0))
      } else 
        super.genSubNodes
    } else
      super.genSubNodes
  }
}
