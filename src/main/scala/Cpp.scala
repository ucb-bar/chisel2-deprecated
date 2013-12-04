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

import scala.collection.mutable.ArrayBuffer
import scala.math._
import java.io.InputStream
import java.io.OutputStream
import java.io.PrintStream
import scala.sys.process._
import sys.process.stringSeqToProcess
import Reg._
import ChiselError._
import Literal._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

object CString {
  def apply(s: String): String = {
    val cs = new StringBuilder("\"")
    for (c <- s) {
      if (c == '\n') {
        cs ++= "\\n"
      } else if (c == '\\' || c == '"') {
        cs ++= "\\" + c
      } else {
        cs += c
      }
    }
    cs + "\""
  }
}


/** Insures each node such that it has a unique name accross the whole
  hierarchy by prefixing its name by a component path (except for "reset"
  and all nodes in *c*). */
class RenameNodes(topModule: Module) extends GraphVisitor {

  override def start( node: Node ): Unit = {
    node match {
      case l: Literal => ;
      case _         =>
        if (node.name != "" && !(node == Module.scope.implicitReset) && !(node.component == null)) {
          // only modify name if it is not the reset signal or not in top component
          if(node.name != "reset" || !(node.component == topModule)) {
            node.name = node.component.getPathName + "__" + node.name;
          }
        }
    }
  }

}

class ClockedInDomain(val clock: Update) extends GraphVisitor {

  val items = new ArrayBuffer[Node]()

  override def finish( node: Node ): Unit = {
    if( node.isInstanceOf[StateWrite]
      && (clock == node.asInstanceOf[StateWrite].getClock) ) {
      if( !items.contains(node) ) {
        items += node
      }
    }
  }

}


class OnlyLogic extends EdgeFilter {

  override def apply(source: Node, target: Node): Boolean = {
    !target.isInstanceOf[Delay]
//      || (clock == target.asInstanceOf[Delay].clock))
  }

}


class CppBackend extends Backend {
  val keywords = new HashSet[String]();

  override def emitTmp(node: Node): String = {
    require(false)
    if (node.isInObject) {
      emitRef(node)
    } else {
      "dat_t<" + node.width + "> " + emitRef(node)
    }
  }
/*
  override def emitRef(node: Node): String = {
    node match {
      case _ =>
        super.emitRef(node)
    }
  }
 */
  def wordMangle(x: Node, w: Int): String = {
    if (w >= words(x)) {
      "0L"
    } else if (x.isInstanceOf[Literal]) {
      var hex = x.asInstanceOf[Literal].value.toString(16)
      if (hex.length > bpw/4*w) "0x" + hex.slice(hex.length-bpw/4*(w + 1), hex.length-bpw/4*w) + "L" else "0L"
    } else if (x.isInObject) {
      emitRef(x) + ".values[" + w + "]"
    } else {
      emitRef(x) + "__w" + w
    }
  }
  def emitWordRef(node: Node, w: Int): String = {
    node match {
      case x: IOBound =>
        (if (!node.isInObject && node.inputs.length > 0)
          emitWordRef(node.inputs(0), w) else wordMangle(node, w))
      case _ =>
        wordMangle(node, w)
    }
  }

  override def emitDec(node: Node): String = {
    node match {
      case x: Literal =>
        ""
      case x: RegDelay =>
        "  dat_t<" + node.width + "> " + emitRef(node) + ";\n" +
        "  dat_t<" + node.width + "> " + emitRef(node) + "_shadow;\n";
      case m: MemDelay =>
        "  mem_t<" + m.width + "," + m.depth + "> " + emitRef(m) + ";\n"
      case c: Update =>
        "  int " + emitRef(node) + ";\n" +
        "  int " + emitRef(node) + "_cnt;\n";
      // case f: AsyncFIFO =>
      //   "  async_fifo_t<" + f.width + ",32> " + emitRef(f) + ";\n"
      case _ =>
        "  dat_t<" + node.width + "> " + emitRef(node) + ";\n"
    }
  }

  val bpw = 64
  def words(node: Node): Int = (node.width - 1) / bpw + 1
  def fullWords(node: Node): Int = node.width/bpw
  def emitLoWordRef(node: Node): String = emitWordRef(node, 0)

  def emitTmpDec(node: Node): String = {
    if (!node.isInObject) {
      "  val_t " + (0 until words(node)).map(emitRef(node) + "__w" + _).reduceLeft(_ + ", " + _) + ";\n"
    } else {
      ""
    }
  }
  def block(s: Seq[String]): String = "  {" + s.map(" " + _ + ";").reduceLeft(_ + _) + " }\n"
  def makeArray(s: String, x: Node): List[String] = List("val_t " + s + "[" + words(x) + "]")
  def toArray(s: String, x: Node): List[String] = makeArray(s, x) ++ (0 until words(x)).map(i => s + "[" + i + "] = " + emitWordRef(x, i))
  def fromArray(s: String, x: Node) =
    (0 until words(x)).map(i => emitWordRef(x, i) + " = " + s + "[" + i + "]")
  def trunc(x: Node): String = {
    if (words(x) != fullWords(x)) {
      "  " + emitWordRef(x, words(x)-1) + " = " + emitWordRef(x, words(x)-1) + " & " + ((1L << (x.width-bpw*fullWords(x)))-1) + ";\n"
    } else {
      ""
    }
  }
  def opFoldLeft(o: Op, initial: (String, String) => String, subsequent: (String, String, String) => String) =
    (1 until words(o.inputs(0))).foldLeft(initial(emitLoWordRef(o.inputs(0)), emitLoWordRef(o.inputs(1))))((c, i) => subsequent(c, emitWordRef(o.inputs(0), i), emitWordRef(o.inputs(1), i)))


  /** Emit code for sign and absolute value
    */
  def signAbs(opand: Node): (Node, Node) = {
    val signOpand = new LtnSOp(opand, UInt(0).node)
    emitDefLo(signOpand)
    val signRev = new SignRevOp(opand)
    emitDefLo(signRev)
    val absOpand = new MuxOp(signOpand, signRev, opand)
    emitDefLo(absOpand)
    (signOpand, absOpand)
  }

  def emitLogicalOpDef(op: Op): String = {
    emitTmpDec(op) +
    (block((0 until words(op)).map(
      i => emitWordRef(op, i) + " = " + emitWordRef(op.inputs(0), i)
        + op.opInfix + emitWordRef(op.inputs(1), i))))
  }

  def emitOrderOpDef(o: Op): String = {
    val initial = (a: String, b: String) => a + o.opInfix + b
    val subsequent = (i: String, a: String, b: String) => ("(" + i + ") & "
      + a + " == " + b + " || " + a + o.slug(0) + b)
    val cond = opFoldLeft(o, initial, subsequent)
    (emitTmpDec(o) +
      "  " + emitLoWordRef(o) + " = "
      + opFoldLeft(o, initial, subsequent) + ";\n")
  }

  def emitDefLo(node: Node): String = {
    node match {
      case x: MuxOp =>
        emitTmpDec(x) +
        block(List("val_t __mask = -" + emitLoWordRef(x.inputs(0))) ++
          (0 until words(x)).map(i => if (x.inputs.length > 2) {emitWordRef(x, i) + " = " + emitWordRef(x.inputs(2), i) + " ^ ((" + emitWordRef(x.inputs(2), i) + " ^ " + emitWordRef(x.inputs(1), i) + ") & __mask)"} else {
            emitWordRef(x, i) + " = " + emitWordRef(x.inputs(1), i)
          }))
      case o: AddOp => {
        val res = ArrayBuffer[String]()
        res += (emitLoWordRef(o) + " = " + emitLoWordRef(o.inputs(0))
          + o.opInfix + emitLoWordRef(o.inputs(1)))
        for (i <- 1 until words(o)) {
          var carry = (emitWordRef(o.inputs(0), i-1)
            + o.opInfix + emitWordRef(o.inputs(1), i-1))
          carry += (" < " + emitWordRef(o.inputs(0), i-1)
            + (if (i > 1) " || " + emitWordRef(o, i-1) + " < __c" else ""))
          res += (if (i == 1) "val_t " else "") + "__c = " + carry
          res += (emitWordRef(o, i) + " = "
            + emitWordRef(o.inputs(0), i)
            + o.opInfix + emitWordRef(o.inputs(1), i) + o.opInfix + "__c")
        }
        emitTmpDec(o) + block(res) + trunc(o)
      }

      case o: SubOp => {
        val res = ArrayBuffer[String]()
        res += (emitLoWordRef(o) + " = " + emitLoWordRef(o.inputs(0))
          + o.opInfix + emitLoWordRef(o.inputs(1)))
        for (i <- 1 until words(o)) {
          var carry = (emitWordRef(o.inputs(0), i-1)
            + o.opInfix + emitWordRef(o.inputs(1), i-1))
          carry += (" > " + emitWordRef(o.inputs(0), i-1)
            + (if (i > 1) " || " + carry + " < " + emitWordRef(o, i-1)
            else ""))
          res += (if (i == 1) "val_t " else "") + "__c = " + carry
          res += (emitWordRef(o, i) + " = "
            + emitWordRef(o.inputs(0), i)
            + o.opInfix + emitWordRef(o.inputs(1), i) + o.opInfix + "__c")
        }
        emitTmpDec(o) + block(res) + trunc(o)
      }

      case o: BitwiseRevOp =>
        (emitTmpDec(o) + block((0 until words(o)).map(
          i => emitWordRef(o, i) + " = ~" + emitWordRef(o.inputs(0), i)))
          + trunc(o))

      case o: CatOp => {
        val lsh = o.inputs(1).width
        emitTmpDec(o) +
        block((0 until fullWords(o.inputs(1))).map(i => emitWordRef(o, i) + " = " + emitWordRef(o.inputs(1), i)) ++
          (if (lsh % bpw != 0) List(emitWordRef(o, fullWords(o.inputs(1))) + " = " + emitWordRef(o.inputs(1), fullWords(o.inputs(1))) + " | " + emitLoWordRef(o.inputs(0)) + " << " + (lsh % bpw)) else List()) ++
          (words(o.inputs(1)) until words(o)).map(i => emitWordRef(o, i)
            + " = " + emitWordRef(o.inputs(0), (bpw*i-lsh)/bpw)
            + (
              if (lsh % bpw != 0) {
                " >> " + (bpw - lsh % bpw) + (
                  if ((bpw*i-lsh)/bpw + 1 < words(o.inputs(0))) {
                    " | " + emitWordRef(o.inputs(0), (bpw*i-lsh)/bpw + 1) + " << " + (lsh%bpw)
                  } else {
                    ""
                  })
              } else {
                ""
              })))
      }

      case o: LeftShiftOp => {
        (emitTmpDec(o) + (if (o.width <= bpw) {
          "  " + emitLoWordRef(o) + " = " + emitLoWordRef(o.inputs(0)) + " << " + emitLoWordRef(o.inputs(1)) + ";\n"
        } else {
          var shb = emitLoWordRef(o.inputs(1))
          val res = ArrayBuffer[String]()
          res ++= toArray("__x", o.inputs(0))
          res += "val_t __c = 0"
          res += "val_t __w = " + emitLoWordRef(o.inputs(1)) + " / " + bpw
          res += "val_t __s = " + emitLoWordRef(o.inputs(1)) + " % " + bpw
          res += "val_t __r = " + bpw + " - __s"
          for (i <- 0 until words(o)) {
            res += "val_t __v" + i + " = MASK(__x[CLAMP(" + i + "-__w,0," + (words(o.inputs(0)) - 1) + ")]," + i + ">=__w&&" + i + "<__w+" + words(o.inputs(0)) + ")"
            res += emitWordRef(o, i) + " = __v" + i + " << __s | __c"
            res += "__c = MASK(__v" + i + " >> __r, __s != 0)"
          }
          block(res) + trunc(o)
        }))
      }

      case o: LogicalNegOp =>
        (emitTmpDec(o) + "  " + emitLoWordRef(o) + " = !"
          + emitLoWordRef(o.inputs(0)) + ";\n")

      case o: ReduceAndOp =>
        (emitTmpDec(o) + "  " + emitLoWordRef(o) + " = "
          + (0 until words(o.inputs(0))).map(
            i => "(" + emitWordRef(o.inputs(0), i) + " == "
              + (if (o.inputs(0).width - i*bpw < bpw)
                (1L << (o.inputs(0).width - i*bpw))-1
              else "(val_t)-1") + ")").reduceLeft(_ + " & " + _) + ";\n")

      case o: ReduceOrOp =>
        (emitTmpDec(o) + "  " + emitLoWordRef(o) + " = ("
          + (0 until words(o.inputs(0))).map(
            emitWordRef(o.inputs(0), _)).reduceLeft(_ + " | " + _)
          + ") != 0;\n")

      case o: ReduceXorOp =>
        (emitTmpDec(o) + {
          val res = ArrayBuffer[String]()
          res += "val_t __x = " + (0 until words(o.inputs(0))).map(
            emitWordRef(o.inputs(0), _)).reduceLeft(_ + " ^ " + _)
          for (i <- log2Up(min(bpw, o.inputs(0).width))-1 to 0 by -1)
            res += "__x = (__x >> " + (1L << i) + ") ^ __x"
          res += emitLoWordRef(o) + " = __x & 1"
          block(res)
        })

      case o: OrOp => emitLogicalOpDef(o)
      case o: AndOp => emitLogicalOpDef(o)
      case o: XorOp => emitLogicalOpDef(o)
      case o: LogicalOrOp => emitLogicalOpDef(o)
      case o: LogicalAndOp => emitLogicalOpDef(o)

      case o: SignRevOp =>
        (emitTmpDec(o) + block((0 until words(o)).map(
          i => emitWordRef(o, i) + " = -" + emitWordRef(o.inputs(0), i)
            + (if (i > 0) " - __borrow"
            else if (words(o) > 1) "; val_t __borrow" else "")
            + (if (i < words(o)-1) "; __borrow = "
              + emitWordRef(o.inputs(0), i) + " || " + emitWordRef(o, i)
            else ""))) + trunc(o))

      case o: RightShiftOp => {
        (emitTmpDec(o) + (if (o.inputs(0).width <= bpw) {
          if (o.isSigned) {
            ("  " + emitLoWordRef(o) + " = (sval_t)("
              + emitLoWordRef(o.inputs(0)) + " << "
              + (bpw - o.inputs(0).width) + ") >> ("
              + (bpw - o.inputs(0).width) + " + "
              + emitLoWordRef(o.inputs(1)) + ");\n" + trunc(o))
          } else {
            ("  " + emitLoWordRef(o) + " = "
              + emitLoWordRef(o.inputs(0)) + " >> "
              + emitLoWordRef(o.inputs(1)) + ";\n")
          }
        } else {
          var shb = emitLoWordRef(o.inputs(1))
          val res = ArrayBuffer[String]()
          res ++= toArray("__x", o.inputs(0))
          res += "val_t __c = 0"
          res += "val_t __w = " + emitLoWordRef(o.inputs(1)) + " / " + bpw
          res += "val_t __s = " + emitLoWordRef(o.inputs(1)) + " % " + bpw
          res += "val_t __r = " + bpw + " - __s"
          if (o.isSigned) {
            res += "val_t __msb = (sval_t)" + emitWordRef(o.inputs(0), words(o)-1) + (if (o.width % bpw != 0) " << " + (bpw-o.width%bpw) else "") + " >> " + (bpw-1)
          }
          for (i <- words(o)-1 to 0 by -1) {
            res += "val_t __v" + i + " = MASK(__x[CLAMP(" + i + "+__w,0," + (words(o.inputs(0))-1) + ")],__w+" + i + "<" + words(o.inputs(0)) + ")"
            res += emitWordRef(o, i) + " = __v" + i + " >> __s | __c"
            res += "__c = MASK(__v" + i + " << __r, __s != 0)"
            if (o.isSigned) {
              res += emitWordRef(o, i) + " |= MASK(__msb << ((" + (o.width-1) + "-" + emitLoWordRef(o.inputs(1)) + ") % " + bpw + "), " + ((i + 1) * bpw) + " > " + (o.width-1) + "-" + emitLoWordRef(o.inputs(1)) + ")"
              res += emitWordRef(o, i) + " |= MASK(__msb, " + (i*bpw) + " >= " + (o.width-1) + "-" + emitLoWordRef(o.inputs(1)) + ")"
            }
          }
          if (o.isSigned) {
            res += emitLoWordRef(o) + " |= MASK(__msb << ((" + (o.width-1) + "-" + emitLoWordRef(o.inputs(1)) + ") % " + bpw + "), " + bpw + " > " + (o.width-1) + "-" + emitLoWordRef(o.inputs(1)) + ")"
          }
          block(res) + (if (o.isSigned) trunc(o) else "")
        }))
      }

      case o: EqlOp => {
        val res = new StringBuilder
        val a = o.inputs(0)
        val b = o.inputs(1)
        if (b.isInstanceOf[Literal] && b.asInstanceOf[Literal].isZ) {
          val (bits, mask, swidth) = parseLit(b.asInstanceOf[Literal].name)
          val and = new AndOp(a, Literal(BigInt(mask, 2)))
          res.append(emitDefLo(and))
          res.append(emitDefLo(new EqlOp(and, Literal(BigInt(bits, 2)))))
        } else if (a.isInstanceOf[Literal] && a.asInstanceOf[Literal].isZ) {
          val (bits, mask, swidth) = parseLit(b.asInstanceOf[Literal].name)
          val and = new AndOp(b, Literal(BigInt(mask, 2)))
          res.append(emitDefLo(and))
          res.append(emitDefLo(new EqlOp(and, Literal(BigInt(bits, 2)))))
        } else {
          val initial = (a: String, b: String) => a + " == " + b
          val subsequent = (i: String, a: String, b: String) => "(" + i + ") & (" + a + " == " + b + ")"
          res.append("  " + emitLoWordRef(o) + " = "
            + opFoldLeft(o, initial, subsequent) + ";\n")
        }
        emitTmpDec(o) + res.toString
      }

      case o: NeqOp => {
        val initial = (a: String, b: String) => a + " != " + b
        val subsequent = (i: String, a: String, b: String) => "(" + i + ") | (" + a + " != " + b + ")"
        (emitTmpDec(o) + "  " + emitLoWordRef(o) + " = "
          + opFoldLeft(o, initial, subsequent) + ";\n")
      }

      case o: GtrSOp => {
        val left = o.inputs(0)
        val right = o.inputs(1)
        val (signLeft, absLeft) = signAbs(left)
        val (signRight, absRight) = signAbs(right)
        val ucond = new GtrOp(absLeft, absRight)
        val result = new MuxOp(
          new EqlOp(signLeft, signRight), ucond, signRight)
        "XXX Generate code for gtrs"
      }

      case o: GteSOp => {
        val left = o.inputs(0)
        val right = o.inputs(1)
        val (signLeft, absLeft) = signAbs(left)
        val (signRight, absRight) = signAbs(right)
        val ucond = new GtrOp(absLeft, absRight)
        val result = new MuxOp(
          new EqlOp(signLeft, signRight), ucond, signRight)
        "XXX Generate code for gtes"
      }

      case o: LteSOp => {
        val left = o.inputs(0)
        val right = o.inputs(1)
        val (signLeft, absLeft) = signAbs(left)
        val (signRight, absRight) = signAbs(right)
        val ucond = new LteOp(absLeft, absRight)
        val result = new MuxOp(
            new EqlOp(signLeft, signRight), ucond, signLeft)
        "XXX Generate code for ltes"
      }

      case o: LtnSOp => {
        val left = o.inputs(0)
        val right = o.inputs(1)
        val (signLeft, absLeft) = signAbs(left)
        val (signRight, absRight) = signAbs(right)
        val ucond = new LteOp(absLeft, absRight)
        val result = new MuxOp(
          new EqlOp(signLeft, signRight), ucond, signLeft)
        val shamt = (result.width-1) % bpw
        ("  " + emitLoWordRef(o) + " = ("
          + emitWordRef(result, words(result)-1) + " >> " + shamt + ") & 1;\n")
      }

      case o: GteOp => emitOrderOpDef(o)
      case o: GtrOp => emitOrderOpDef(o)
      case o: LteOp => emitOrderOpDef(o)
      case o: LtnOp => emitOrderOpDef(o)

      case o: MulSOp => {
        val res = new StringBuilder
        val left = o.inputs(0)
        val right = o.inputs(1)
        val (signLeft, absLeft) = signAbs(left)
        val (signRight, absRight) = signAbs(right)
        val prod = new MulOp(absLeft, absRight)
        res.append(emitDefLo(prod))
        val rev = new SignRevOp(prod)
        res.append(emitDefLo(rev))
        val signs = new XorOp(signLeft, signRight)
        res.append(emitDefLo(signs))
        val result = new MuxOp(signs, rev, prod)
        res.append(emitDefLo(result))
        res.toString
      }

      case o: MulSUOp => {
        val res = new StringBuilder
        val left = o.inputs(0)
        val right = o.inputs(1)
        val (signLeft, absLeft) = signAbs(left)
        val prod = new MulOp(absLeft, right)
        res.append(emitDefLo(prod))
        val rev = new SignRevOp(prod)
        res.append(emitDefLo(rev))
        res.append(emitDefLo(signLeft))
        val result = new MuxOp(signLeft, rev, prod)
        res.append(emitDefLo(result))
        res.toString
      }

      case o: MulOp =>
        (if (o.width <= bpw) {
          ("  " + emitLoWordRef(o) + " = "
            + emitLoWordRef(o.inputs(0)) + " * "
            + emitLoWordRef(o.inputs(1)) + ";\n")
          } else {
            val cmd = ("mul_n(__d, __x, __y, " + o.width
              + ", " + o.inputs(0).width + ", " + o.inputs(1).width + ")")
            (block(makeArray("__d", o)
              ++ toArray("__x", o.inputs(0))
              ++ toArray("__y", o.inputs(1))
              ++ List(cmd)
              ++ fromArray("__d", o)))
          })

      case o: DivSOp => {
        val res = new StringBuilder
        val (signA, absA) = signAbs(o.inputs(0))
        val (signB, absB) = signAbs(o.inputs(1))
        val quo = new DivOp(absA, absB)
        res.append(emitDefLo(quo))
        val quoRev = new SignRevOp(quo)
        res.append(emitDefLo(quoRev))
        val eqlSigns = new EqlOp(signA, signB)
        res.append(emitDefLo(eqlSigns))
        val result = new MuxOp(eqlSigns, quo, quoRev)
        res.append(emitDefLo(result))
        res.toString
      }

      case o: DivOp => {
          val cmd = ("div_n(__d, __x, __y, " + o.width
            + ", " + o.inputs(0).width + ", " + o.inputs(1).width + ")")
          (block(makeArray("__d", o)
            ++ toArray("__x", o.inputs(0))
            ++ toArray("__y", o.inputs(1))
            ++ List(cmd)
            ++ fromArray("__d", o)))
      }

      case o: RemSOp => {
        val res = new StringBuilder
        val (signA, absA) = signAbs(o.inputs(0))
        val (signB, absB) = signAbs(o.inputs(1))
        val rem = new RemOp(absA, absB)
        res.append(emitDefLo(rem))
        val remRev = new SignRevOp(rem)
        res.append(emitDefLo(remRev))
        val result = new MuxOp(signA, remRev, rem)
        res.append(emitDefLo(result))
        res.toString
      }

      case o: RemOp => {
        val res = new StringBuilder
        val au = o.inputs(0)
        val bu = o.inputs(1)
        val div = new DivOp(au, bu)
        res.append(emitDefLo(div))
        val mul = new MulOp(div, bu)
        res.append(emitDefLo(mul))
        val result = new SubOp(au, mul)
        res.append(emitDefLo(result))
        res.toString
      }

      case o: ExtractOp =>
        emitTmpDec(node) +
        (if (node.inputs.length < 3 || node.width == 1) {
          if (node.inputs(1).isInstanceOf[Literal]) {
            val value = node.inputs(1).asInstanceOf[Literal].value.toInt
            "  " + emitLoWordRef(node) + " = (" + emitWordRef(node.inputs(0), value/bpw) + " >> " + (value%bpw) + ") & 1;\n"
          } else if (node.inputs(0).width <= bpw) {
            "  " + emitLoWordRef(node) + " = (" + emitLoWordRef(node.inputs(0)) + " >> " + emitLoWordRef(node.inputs(1)) + ") & 1;\n"
          } else {
            block(toArray("__e", node.inputs(0)) ++ List(emitLoWordRef(node) + " = __e[" + emitLoWordRef(node.inputs(1)) + "/" + bpw + "] >> (" + emitLoWordRef(node.inputs(1)) + "%" + bpw + ") & 1"))
          }
        } else {
          val rsh = node.inputs(2).asInstanceOf[Literal].value.toInt
          if (rsh % bpw == 0) {
            block((0 until words(node)).map(i => emitWordRef(node, i) + " = " + emitWordRef(node.inputs(0), i + rsh/bpw))) + trunc(node)
          } else {
            block((0 until words(node)).map(i => emitWordRef(node, i)
              + " = " + emitWordRef(node.inputs(0), i + rsh/bpw) + " >> "
              + (rsh % bpw) + (
                if (i + rsh/bpw + 1 < words(node.inputs(0))) {
                  " | " + emitWordRef(node.inputs(0), i + rsh/bpw + 1) + " << " + (bpw - rsh % bpw)
                } else {
                  ""
                }))) + trunc(node)
          }
        })

      case o: FillOp => {
        // require(node.inputs(1).isLit)
        if( o.width != 1) {
          val res = new StringBuilder
          var out: Node = null
          var i = 0
          var cur = o.opand
          while ((1 << i) <= o.n) {
            if ((o.n & (1 << i)) != 0) {
              out = new CatOp(cur, out)
              res.append(emitDefLo(out))
            }
            cur = new CatOp(cur, cur)
            res.append(emitDefLo(cur))
            i = i + 1
          }
          res.toString
        } else {
          require(o.inputs(0).width == 1)
          emitTmpDec(o) + block((0 until words(o)).map(
            i => emitWordRef(o, i) + " = "
              + (if (o.inputs(0).isInstanceOf[Literal]) {
                0L - o.inputs(0).asInstanceOf[Literal].value.toInt
              } else {
                "-" + emitLoWordRef(o.inputs(0))
              }))) + trunc(o)
        }
      }

      case x: Update =>
        ""

      case m: MemRead =>
        emitTmpDec(m) + block((0 until words(m)).map(i => emitWordRef(m, i)
          + " = " + emitRef(m.mem) + ".get(" + emitLoWordRef(m.addr) + ", "
          + i + ")"))

      case m: MemSeqRead =>
        emitTmpDec(m) + block((0 until words(m)).map(i => emitWordRef(m, i)
          + " = " + emitRef(m.mem) + ".get(" + emitLoWordRef(m.addr) + ", "
          + i + ")"))

      case reg: RegDelay =>
        def updateData(w: Int): String = if (reg.hasReset) "TERNARY(" + emitLoWordRef(reg.reset) + ", " + emitWordRef(reg.init, w) + ", " + emitWordRef(reg.next, w) + ")" else emitWordRef(reg.next, w)

        def shadow(w: Int): String = emitRef(reg) + "_shadow.values[" + w + "]"
        block((0 until words(reg)).map(i => shadow(i) + " = " + updateData(i)))

      case x: Log2Op =>
        (emitTmpDec(x) + "  " + emitLoWordRef(x) + " = "
          + (words(x.inputs(0))-1 to 1 by -1).map(
            i => emitWordRef(x.inputs(0), i) + " != 0, "
              + (i*bpw) + " + log2_1("
              + emitWordRef(x.inputs(0), i) + ")").foldRight("log2_1("
                + emitLoWordRef(x.inputs(0)) + ")")(
            "TERNARY(" + _ + ", " + _ + ")") + ";\n")

      case a: Assert =>
        "  ASSERT(" + emitLoWordRef(a.cond) + ", " + CString(a.message) + ");\n"

      case s: Sprintf =>
        ("#if __cplusplus >= 201103L\n"
          + "  " + emitRef(s) + " = dat_format<" + s.width + ">("
          + s.args.map(emitRef _).foldLeft(CString(s.format))(_ + ", " + _)
          + ");\n"
          + "#endif\n")

      case lit: Literal =>
        ""

      case x: Node =>
        if (x.isInObject && x.inputs.length == 1) {
          emitTmpDec(x) + block((0 until words(x)).map(i => emitWordRef(x, i)
            + " = " + emitWordRef(x.inputs(0), i)))
        } else if (x.inputs.length == 0 && !x.isInObject) {
          emitTmpDec(x) + block((0 until words(x)).map(i => emitWordRef(x, i)
            + " = rand_val()")) + trunc(x)
        } else {
          ""
        }
    }
  }

  def emitDefHi(node: Node): String = {
    node match {
      case reg: RegDelay =>
        "  " + emitRef(reg) + " = " + emitRef(reg) + "_shadow;\n"
      case _ => ""
    }
  }

  def emitInit(node: Node): String = {
    node match {
      case x: Update =>
        if (x.src != null) {
          ("  " + emitRef(node) + " = " + emitRef(x.src)
            + " * " + x.mul + " / " + x.div + ";\n"
            + "  " + emitRef(node) + "_cnt = " + emitRef(node) + ";\n")
        } else
          ""
      case x: RegDelay =>
        "  if (rand_init) " + emitRef(node) + ".randomize();\n"

      case r: ROMemDelay =>
        r.inputs.zipWithIndex.map { case (lit, i) =>
          block((0 until words(r)).map(j => emitRef(r) + ".put(" + i + ", " + j + ", " + emitWordRef(lit, j) + ")"))
        }.reduceLeft(_ + _)

      case x: MemDelay =>
        "  if (rand_init) " + emitRef(node) + ".randomize();\n"

      case u: Node =>
        if (u.driveRand && u.isInObject)
          "  if (rand_init) " + emitRef(node) + ".randomize();\n"
        else
          ""
    }
  }

  def emitInitHi(node: Node): String = {
    node match {
      case m: MemWrite =>
        // schedule before Reg updates in case a MemWrite input is a Reg
        if (m.inputs.length == 2) {
          return ""
        }
        def mask(w: Int): String = "(-" + emitLoWordRef(m.cond) + (if (m.isMasked) " & " + emitWordRef(m.mask, w) else "") + ")"
        block((0 until words(m)).map(i => emitRef(m.mem)
          + ".put(" + emitLoWordRef(m.addr) + ", " + i
          + ", (" + emitWordRef(m.data, i) + " & " + mask(i)
          + ") | (" + emitRef(m.mem) + ".get(" + emitLoWordRef(m.addr)
          + ", " + i + ") & ~" + mask(i) + "))"))

      case _ =>
        ""
    }
  }

  def clkName (clock: Update): String =
    (if (clock == Module.scope.implicitClock) "" else "_" + emitRef(clock))

  def genHarness(c: Module, name: String) {
    val harness  = createOutputFile(name + "-emulator.cpp");
    harness.write("#include \"" + name + ".h\"\n");
    harness.write("int main (int argc, char* argv[]) {\n");
    harness.write("  " + name + "_t* c = new " + name + "_t();\n");
    harness.write("  int lim = (argc > 1) ? atoi(argv[1]) : -1;\n");
    harness.write("  int period;\n")
    if( c.clocks.length > 1 ) {
      for (clock <- c.clocks) {
        if (clock.src == null) {
          harness.write("  period = atoi(read_tok(stdin).c_str());\n")
          harness.write("  c->" + emitRef(clock) + " = period;\n")
          harness.write("  c->" + emitRef(clock) + "_cnt = period;\n")
        }
      }
    }
    harness.write("  c->init();\n");
    if (Module.isVCD) {
      harness.write("  FILE *f = fopen(\"" + name + ".vcd\", \"w\");\n");
    }
    harness.write("  int delta = 0;\n")
    harness.write("  for(int i = 0; i < 5; i++) {\n")
    harness.write("    dat_t<1> reset = LIT<1>(1);\n")
    harness.write("    delta += c->clock(reset);\n")
    harness.write("  }\n")
    harness.write("  for (int t = 0; lim < 0 || t < lim; t++) {\n");
    harness.write("    dat_t<1> reset = LIT<1>(0);\n");
    harness.write("    if (!c->scan(stdin)) break;\n");
    harness.write("    delta += c->clock(reset);\n")
    if( c.clocks.length > 1 ) {
      harness.write("    fprintf(stdout, \"%d\", delta);\n")
      harness.write("    fprintf(stdout, \"%s\", \" \");\n")
    }
    harness.write("    c->print(stdout);\n")
    if (Module.isVCD) {
      harness.write("    c->dump(f, t);\n");
    }
    harness.write("    delta = 0;\n")
    harness.write("  }\n");
    harness.write("}\n");
    harness.close();
  }

  override def compile(c: Module, flagsIn: String) {
    val flags = if (flagsIn == null) "-O2" else flagsIn

    val chiselENV = java.lang.System.getenv("CHISEL")
    val allFlags = flags + " -I../ -I" + chiselENV + "/csrc/"
    val dir = Module.targetDir + "/"
    def run(cmd: String) {
      val bashCmd = Seq("bash", "-c", cmd)
      val c = bashCmd.!
      ChiselError.info(cmd + " RET " + c)
    }
    def link(name: String) {
      val ac = "g++ -o " + dir + name + " " + dir + name + ".o " + dir + name + "-emulator.o"
      run(ac)
    }
    def cc(name: String) {
      val cmd = "g++ -c -o " + dir + name + ".o " + allFlags + " " + dir + name + ".cpp"
      run(cmd)
    }
    cc(c.name + "-emulator")
    cc(c.name)
    link(c.name)
  }

  def emitDefLos(c: Module): String = {
    var res = "";
    for ((n, w) <- c.wires) {
      w match {
        case io: IOBound  =>
          if (io.isDirected(INPUT)) {
            res += ("  " + emitRef(c) + "->" + n + " = "
              + emitRef(io.inputs(0)) + ";\n");
          }
      };
    }
    res += emitRef(c) + "->clock_lo(reset);\n";
    for ((n, w) <- c.wires) {
      w match {
        case io: IOBound =>
          if (io.isDirected(OUTPUT)) {
            res += ("  " + emitRef(io.consumers(0)) + " = "
              + emitRef(c) + "->" + n + ";\n");
          }
      };
    }
    res
  }

  def emitDefHis(c: Module): String = {
    var res = emitRef(c) + "->clock_hi(reset);\n";
    res
  }

  override def elaborate(c: Module): Unit = {
    super.elaborate(c)
    if( !c.clocks.contains(Module.scope.implicitClock.node) ) {
      c.clocks += Module.scope.implicitClock.node.asInstanceOf[Update]
    }

/*
    /* We flatten all signals in the toplevel component after we had
     a change to associate node and components correctly first
     otherwise we are bound for assertions popping up left and right
     in the Backend.elaborate method. */
    for (cc <- Module.components) {
      if (!(cc == c)) {
        c.debugs ++= cc.debugs
        c.nodes  ++= cc.nodes;
      }
    }
 */
    c.findOrdering(); // search from roots  -- create omods
    GraphWalker.depthFirst(findRoots(c), new RenameNodes(c))

    if (Module.isReportDims) {
      val (numNodes, maxWidth, maxDepth) = c.findGraphDims();
      ChiselError.info("NUM " + numNodes + " MAX-WIDTH " + maxWidth + " MAX-DEPTH " + maxDepth);
    }

    val clkDomains = new HashMap[Update, (StringBuilder, StringBuilder)]
    for (clock <- c.clocks) {
      val clock_lo = new StringBuilder
      val clock_hi = new StringBuilder
      clkDomains += (clock -> ((clock_lo, clock_hi)))
      clock_lo.append("void " + c.name + "_t::clock_lo" + clkName(clock) + " ( dat_t<1> reset ) {\n")
      clock_hi.append("void " + c.name + "_t::clock_hi" + clkName(clock) + " ( dat_t<1> reset ) {\n")
    }

    if (Module.isGenHarness) {
      genHarness(c, c.name);
    }
    val out_h = createOutputFile(c.name + ".h");
    val out_c = createOutputFile(c.name + ".cpp");
    out_h.write("#ifndef __" + c.name + "__\n");
    out_h.write("#define __" + c.name + "__\n\n");
    out_h.write("#include \"emulator.h\"\n\n");
    out_h.write("class " + c.name + "_t : public mod_t {\n");
    out_h.write(" public:\n");
    val vcd = new VcdTrace()
    val agg = new Reachable()
    GraphWalker.depthFirst(findRoots(c), agg)
    for( m <- agg.nodes ) {
      if(m.name != "reset") {
        if (m.isInObject) {
          out_h.write(emitDec(m));
        }
        if (m.isInVCD) {
          out_h.write(vcd.emitDec(m));
        }
      }
    }
    for (clock <- c.clocks)
      out_h.write(emitDec(clock))

    out_h.write("\n");
    out_h.write("  void init ( bool rand_init = false );\n");
    for ( clock <- c.clocks) {
      out_h.write("  void clock_lo" + clkName(clock) + " ( dat_t<1> reset );\n")
      out_h.write("  void clock_hi" + clkName(clock) + " ( dat_t<1> reset );\n")
    }
    out_h.write("  int clock ( dat_t<1> reset );\n")
    out_h.write("  void print ( FILE* f );\n");
    out_h.write("  bool scan ( FILE* f );\n");
    out_h.write("  void dump ( FILE* f, int t );\n");
    out_h.write("};\n\n");
    out_h.write("#endif\n");
    out_h.close();

    out_c.write("#include \"" + c.name + ".h\"\n");
    for(str <- Module.includeArgs) out_c.write("#include \"" + str + "\"\n");
    out_c.write("\n");
    out_c.write("void " + c.name + "_t::init ( bool rand_init ) {\n");
    for( m <- agg.nodes ) {
      out_c.write(emitInit(m));
    }
    for( clock <- c.clocks )
      out_c.write(emitInit(clock))
    out_c.write("}\n");

    /* Clocked-state variables */
    for( clock <- c.clocks ) {
      val clkDomain = new ByClassVisitor[Node]()
      val stateVars = new ClockedInDomain(clock)
      GraphWalker.depthFirst(findRoots(c), stateVars)
      val roots = stateVars.items
      if( clock == Module.scope.implicitClock.node ) {
        roots ++= c.io.nodes().filter(n => n.isInstanceOf[IOBound]
          && n.asInstanceOf[IOBound].isDirected(OUTPUT))
        roots ++= c.debugs // no writes as they are clocked.
      }
      GraphWalker.depthFirst(roots, clkDomain, new OnlyLogic())
      for (m <- clkDomain.items) {
        clkDomains(clock)._1.append(emitDefLo(m))
      }
      for (m <- clkDomain.items) {
        clkDomains(clock)._2.append(emitInitHi(m))
      }
      for (m <- clkDomain.items) {
        clkDomains(clock)._2.append(emitDefHi(m))
      }
    }

    for (clk <- clkDomains.keys) {
      clkDomains(clk)._1.append("}\n")
      clkDomains(clk)._2.append("}\n")
      out_c.write(clkDomains(clk)._1.result)
      out_c.write(clkDomains(clk)._2.result)
    }

    out_c.write("int " + c.name + "_t::clock ( dat_t<1> reset ) {\n")
    out_c.write("  uint32_t min = ((uint32_t)1<<31)-1;\n")
    for (clock <- c.clocks) {
      out_c.write("  if (" + emitRef(clock) + "_cnt < min) min = " + emitRef(clock) +"_cnt;\n")
    }
    for (clock <- c.clocks) {
      out_c.write("  " + emitRef(clock) + "_cnt-=min;\n")
    }
    for (clock <- c.clocks) {
      out_c.write("  if (" + emitRef(clock) + "_cnt == 0) clock_lo" + clkName(clock) + "( reset );\n")
    }
    for (clock <- c.clocks) {
      out_c.write("  if (" + emitRef(clock) + "_cnt == 0) clock_hi" + clkName(clock) + "( reset );\n")
    }
    for (clock <- c.clocks) {
      out_c.write("  if (" + emitRef(clock) + "_cnt == 0) " + emitRef(clock) + "_cnt = " + 
                  emitRef(clock) + ";\n")
    }
    out_c.write("  return min;\n")
    out_c.write("}\n")

    def splitFormat(s: String): Seq[String] = {
      var off = 0;
      var res: List[String] = Nil;
      for (i <- 0 until s.length) {
        if (s(i) == '%') {
          if (off < i) {
            res = s.substring(off, i) :: res;
          }
          res = "%" :: res;
          if (i == (s.length-1)) {
            ChiselError.error("Badly formed format argument kind: %");
          } else if (s(i + 1) != 'x') {
            ChiselError.error("Unsupported format argument kind: %" + s(i + 1));
          }
          off = i + 2;
        }
      }
      if (off < (s.length-1)) {
        res = s.substring(off, s.length) :: res;
      }
      res.reverse
    }
    out_c.write("void " + c.name + "_t::print ( FILE* f ) {\n");
    for (p <- Module.printfs)
      out_c.write("#if __cplusplus >= 201103L\n"
        + "  if (" + emitLoWordRef(p.cond)
        + ") dat_fprintf<" + p.width + ">(f, "
        + p.args.map(emitRef _).foldLeft(CString(p.format))(_ + ", " + _)
        + ");\n"
        + "#endif\n")
    if (Module.isTesting && Module.tester != null) {
      val nodes = Module.tester.testNonInputNodes.map(n => n.node)
      for (j <- 0 until nodes.length) {
//        out_c.write("  fprintf(stderr, \"XXX write " + (if (j > 0) " " else "") +
//          "%s ...\\n\", TO_CSTR(" + emitRef(nodes(j)) + "));\n");
        out_c.write("  fprintf(f, \"" + (if (j > 0) " " else "") +
          "%s\", TO_CSTR(" + emitRef(nodes(j)) + "));\n");
      }
      out_c.write("  fprintf(f, \"\\n\");\n");
      out_c.write("  fflush(f);\n");
    }
    out_c.write("}\n");
    def constantArgSplit(arg: String): Array[String] = arg.split('=');
    def isConstantArg(arg: String): Boolean = constantArgSplit(arg).length == 2;
    out_c.write("bool " + c.name + "_t::scan ( FILE* f ) {\n");
    if (Module.isTesting && Module.tester != null) {
      for (node <- Module.tester.testInputNodes.map(n => n.node)) {
            out_c.write("  str_to_dat(read_tok(f), " + emitRef(node) + ");\n");
//            out_c.write("  fprintf(stderr, \"XXX " + emitRef(node) + "=%s\\n\", " + emitRef(node) + ".to_str().c_str());\n");
      }
    }
    out_c.write("  return(!feof(f));\n");
    out_c.write("}\n");
    vcd.dumpVCD(c, out_c);
    out_c.close();

    /* Copy the emulator.h file into the targetDirectory. */
    val resourceStream = getClass().getResourceAsStream("/emulator.h")
    if( resourceStream != null ) {
      val classFile = createOutputFile("emulator.h")
      while(resourceStream.available > 0) {
        classFile.write(resourceStream.read())
      }
      classFile.close()
      resourceStream.close()
    }
  }

}
