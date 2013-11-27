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
import Node._
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

object CListLookup {
  def apply[T <: Data](addr: UInt, default: List[T], mapping: Array[(UInt, List[T])]): List[T] = {
    val map = mapping.map(m => (addr === m._1, m._2))
    default.zipWithIndex map { case (d, i) =>
      map.foldRight(d)((m, n) => Mux(m._1, m._2(i), n))
    }
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

  override def emitRef(node: Node): String = {
    node match {
      case x: Binding =>
        emitRef(x.inputs(0))

      case x: Bits =>
        if (!node.isInObject && node.inputs.length == 1) emitRef(node.inputs(0)) else super.emitRef(node)

      case _ =>
        super.emitRef(node)
    }
  }
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
      case x: Binding =>
        emitWordRef(x.inputs(0), w)
      case x: Bits =>
        if (!node.isInObject && node.inputs.length == 1) emitWordRef(node.inputs(0), w) else wordMangle(node, w)
      case _ =>
        wordMangle(node, w)
    }
  }

  override def emitDec(node: Node): String = {
    node match {
      case x: Binding =>
        ""
      case x: Literal =>
        ""
      case x: ListNode =>
        ""
      case x: MapNode =>
        ""
      case x: LookupMap =>
        ""
      case x: Reg =>
        "  dat_t<" + node.width + "> " + emitRef(node) + ";\n" +
        "  dat_t<" + node.width + "> " + emitRef(node) + "_shadow;\n";
      case m: Mem[_] =>
        "  mem_t<" + m.width + "," + m.n + "> " + emitRef(m) + ";\n"
      case r: ROM[_] =>
        "  mem_t<" + r.width + "," + r.lits.length + "> " + emitRef(r) + ";\n"
      case c: Clock =>
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
  def block(s: Seq[String]): String = 
    if (s.length == 0)
      ""
    else
      "  {" + s.map(" " + _ + ";").reduceLeft(_ + _) + " }\n"
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

  def emitDefLo(node: Node): String = {
    node match {
      case x: Mux =>
        emitTmpDec(x) +
        block(List("val_t __mask = -" + emitLoWordRef(x.inputs(0))) ++
              (0 until words(x)).map(i => emitWordRef(x, i) + " = " + emitWordRef(x.inputs(2), i) + " ^ ((" + emitWordRef(x.inputs(2), i) + " ^ " + emitWordRef(x.inputs(1), i) + ") & __mask)"))

      case o: Op => {
        emitTmpDec(o) +
        (if (o.inputs.length == 1) {
          (if (o.op == "|") {
            "  " + emitLoWordRef(o) + " = (" + (0 until words(o.inputs(0))).map(emitWordRef(o.inputs(0), _)).reduceLeft(_ + " | " + _) + ") != 0;\n"
          } else if (o.op == "&") {
            "  " + emitLoWordRef(o) + " = " + (0 until words(o.inputs(0))).map(i => "(" + emitWordRef(o.inputs(0), i) + " == " + (if (o.inputs(0).width - i*bpw < bpw) (1L << (o.inputs(0).width - i*bpw))-1 else "(val_t)-1") + ")").reduceLeft(_ + " & " + _) + ";\n"
          } else if (o.op == "^") {
            val res = ArrayBuffer[String]()
            res += "val_t __x = " + (0 until words(o.inputs(0))).map(emitWordRef(o.inputs(0), _)).reduceLeft(_ + " ^ " + _)
            for (i <- log2Up(min(bpw, o.inputs(0).width))-1 to 0 by -1)
              res += "__x = (__x >> " + (1L << i) + ") ^ __x"
            res += emitLoWordRef(o) + " = __x & 1"
            block(res)
          } else if (o.op == "~") {
            block((0 until words(o)).map(i => emitWordRef(o, i) + " = ~" + emitWordRef(o.inputs(0), i))) + trunc(o)
          } else if (o.op == "-") {
            block((0 until words(o)).map(i => emitWordRef(o, i) + " = -" + emitWordRef(o.inputs(0), i) + (if (i > 0) " - __borrow" else if (words(o) > 1) "; val_t __borrow" else "") + (if (i < words(o)-1) "; __borrow = " + emitWordRef(o.inputs(0), i) + " || " + emitWordRef(o, i) else ""))) + trunc(o)
          } else if (o.op == "!") {
            "  " + emitLoWordRef(o) + " = !" + emitLoWordRef(o.inputs(0)) + ";\n"
          } else {
            assert(false, "operator " + o.op + " unsupported")
            ""
          })
        } else if (o.op == "+" || o.op == "-") {
          val res = ArrayBuffer[String]()
          res += emitLoWordRef(o) + " = " + emitLoWordRef(o.inputs(0)) + o.op + emitLoWordRef(o.inputs(1))
          for (i <- 1 until words(o)) {
            var carry = emitWordRef(o.inputs(0), i-1) + o.op + emitWordRef(o.inputs(1), i-1)
            if (o.op == "+") {
              carry += " < " + emitWordRef(o.inputs(0), i-1) + (if (i > 1) " || " + emitWordRef(o, i-1) + " < __c" else "")
            } else {
              carry += " > " + emitWordRef(o.inputs(0), i-1) + (if (i > 1) " || " + carry + " < " + emitWordRef(o, i-1) else "")
            }
            res += (if (i == 1) "val_t " else "") + "__c = " + carry
            res += emitWordRef(o, i) + " = " + emitWordRef(o.inputs(0), i) + o.op + emitWordRef(o.inputs(1), i) + o.op + "__c"
          }
          block(res) + trunc(o)
        } else if (o.op == "/") {
          val cmd = "div_n(__d, __x, __y, " + o.width + ", " + o.inputs(0).width + ", " + o.inputs(1).width + ")"
          block(makeArray("__d", o) ++ toArray("__x", o.inputs(0)) ++ toArray("__y", o.inputs(1)) ++ List(cmd) ++ fromArray("__d", o))
        } else if (o.op == "*") {
          if (o.width <= bpw) {
            "  " + emitLoWordRef(o) + " = " + emitLoWordRef(o.inputs(0)) + " * " + emitLoWordRef(o.inputs(1)) + ";\n"
          } else {
            val cmd = "mul_n(__d, __x, __y, " + o.width + ", " + o.inputs(0).width + ", " + o.inputs(1).width + ")"
            block(makeArray("__d", o) ++ toArray("__x", o.inputs(0)) ++ toArray("__y", o.inputs(1)) ++ List(cmd) ++ fromArray("__d", o))
          }
        } else if (o.op == "<<") {
          if (o.width <= bpw) {
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
          }
        } else if (o.op == ">>" || o.op == "s>>") {
          val arith = o.op == "s>>"
          if (o.inputs(0).width <= bpw) {
            if (arith) {
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
            if (arith) {
              res += "val_t __msb = (sval_t)" + emitWordRef(o.inputs(0), words(o)-1) + (if (o.width % bpw != 0) " << " + (bpw-o.width%bpw) else "") + " >> " + (bpw-1)
            }
            for (i <- words(o)-1 to 0 by -1) {
              res += "val_t __v" + i + " = MASK(__x[CLAMP(" + i + "+__w,0," + (words(o.inputs(0))-1) + ")],__w+" + i + "<" + words(o.inputs(0)) + ")"
              res += emitWordRef(o, i) + " = __v" + i + " >> __s | __c"
              res += "__c = MASK(__v" + i + " << __r, __s != 0)"
              if (arith) {
                res += emitWordRef(o, i) + " |= MASK(__msb << ((" + (o.width-1) + "-" + emitLoWordRef(o.inputs(1)) + ") % " + bpw + "), " + ((i + 1) * bpw) + " > " + (o.width-1) + "-" + emitLoWordRef(o.inputs(1)) + ")"
                res += emitWordRef(o, i) + " |= MASK(__msb, " + (i*bpw) + " >= " + (o.width-1) + "-" + emitLoWordRef(o.inputs(1)) + ")"
              }
            }
            if (arith) {
              res += emitLoWordRef(o) + " |= MASK(__msb << ((" + (o.width-1) + "-" + emitLoWordRef(o.inputs(1)) + ") % " + bpw + "), " + bpw + " > " + (o.width-1) + "-" + emitLoWordRef(o.inputs(1)) + ")"
            }
            block(res) + (if (arith) trunc(o) else "")
          }
        } else if (o.op == "##") {
          val lsh = o.inputs(1).width
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
        } else if (o.op == "|" || o.op == "&" || o.op == "^" || o.op == "||" || o.op == "&&") {
          block((0 until words(o)).map(i => emitWordRef(o, i) + " = " + emitWordRef(o.inputs(0), i) + o.op + emitWordRef(o.inputs(1), i)))
        } else if (o.op == "s<") {
          require(o.inputs(1).litOf.value == 0)
          val shamt = (o.inputs(0).width-1) % bpw
          "  " + emitLoWordRef(o) + " = (" + emitWordRef(o.inputs(0), words(o.inputs(0))-1) + " >> " + shamt + ") & 1;\n"
        } else if (o.op == "<" || o.op == "<=") {
          val initial = (a: String, b: String) => a + o.op + b
          val subsequent = (i: String, a: String, b: String) => "(" + i + ") & " + a + " == " + b + " || " + a + o.op(0) + b
          val cond = opFoldLeft(o, initial, subsequent)
          "  " + emitLoWordRef(o) + " = " + opFoldLeft(o, initial, subsequent) + ";\n"
        } else if (o.op == "==") {
          val initial = (a: String, b: String) => a + " == " + b
          val subsequent = (i: String, a: String, b: String) => "(" + i + ") & (" + a + " == " + b + ")"
          "  " + emitLoWordRef(o) + " = " + opFoldLeft(o, initial, subsequent) + ";\n"
        } else if (o.op == "!=") {
          val initial = (a: String, b: String) => a + " != " + b
          val subsequent = (i: String, a: String, b: String) => "(" + i + ") | (" + a + " != " + b + ")"
          "  " + emitLoWordRef(o) + " = " + opFoldLeft(o, initial, subsequent) + ";\n"
        } else {
          assert(false, "operator " + o.op + " unsupported")
          ""
        })
      }

      case x: Extract =>
        x.inputs.tail.foreach(e => x.validateIndex(e))
        emitTmpDec(node) +
        (if (node.inputs.length < 3 || node.width == 1) {
          if (node.inputs(1).isLit) {
            val value = node.inputs(1).value.toInt
            "  " + emitLoWordRef(node) + " = (" + emitWordRef(node.inputs(0), value/bpw) + " >> " + (value%bpw) + ") & 1;\n"
          } else if (node.inputs(0).width <= bpw) {
            "  " + emitLoWordRef(node) + " = (" + emitLoWordRef(node.inputs(0)) + " >> " + emitLoWordRef(node.inputs(1)) + ") & 1;\n"
          } else {
            block(toArray("__e", node.inputs(0)) ++ List(emitLoWordRef(node) + " = __e[" + emitLoWordRef(node.inputs(1)) + "/" + bpw + "] >> (" + emitLoWordRef(node.inputs(1)) + "%" + bpw + ") & 1"))
          }
        } else {
          val rsh = node.inputs(2).value.toInt
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

      case x: Fill =>
        // require(node.inputs(1).isLit)
        require(node.inputs(0).width == 1)
        emitTmpDec(node) + block((0 until words(node)).map(
          i => emitWordRef(node, i) + " = "
            + (if (node.inputs(0).isLit) {
              0L - node.inputs(0).value.toInt
            } else {
              "-" + emitLoWordRef(node.inputs(0))
            }))) + trunc(node)

      case x: Clock =>
        ""

      case x: Bits =>
        if (x.isInObject && x.inputs.length == 1) {
          emitTmpDec(x) + block((0 until words(x)).map(i => emitWordRef(x, i)
            + " = " + emitWordRef(x.inputs(0), i)))
        } else if (x.inputs.length == 0 && !x.isInObject) {
          emitTmpDec(x) + block((0 until words(x)).map(i => emitWordRef(x, i)
            + " = rand_val()")) + trunc(x)
        } else {
          ""
        }

      case m: MemRead =>
        emitTmpDec(m) + block((0 until words(m)).map(i => emitWordRef(m, i)
          + " = " + emitRef(m.mem) + ".get(" + emitLoWordRef(m.addr) + ", "
          + i + ")"))

      case r: ROMRead[_] =>
        emitTmpDec(r) + block((0 until words(r)).map(i => emitWordRef(r, i)
          + " = " + emitRef(r.rom) + ".get(" + emitLoWordRef(r.addr) + ", "
          + i + ")"))

      case reg: Reg =>
        def updateData(w: Int): String = if (reg.isReset) "TERNARY(" + emitLoWordRef(reg.inputs.last) + ", " + emitWordRef(reg.init, w) + ", " + emitWordRef(reg.next, w) + ")" else emitWordRef(reg.next, w)

        def shadow(w: Int): String = emitRef(reg) + "_shadow.values[" + w + "]"
        block((0 until words(reg)).map(i => shadow(i) + " = " + updateData(i)))

      case x: Log2 =>
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

      case _ =>
        ""
    }
  }

  def emitDefHi(node: Node): String = {
    node match {
      case reg: Reg =>
        "  " + emitRef(reg) + " = " + emitRef(reg) + "_shadow;\n"
      case _ => ""
    }
  }

  def emitInit(node: Node): String = {
    node match {
      case x: Clock =>
        if (x.srcClock != null) {
          "  " + emitRef(node) + " = " + emitRef(x.srcClock) + x.initStr +
          "  " + emitRef(node) + "_cnt = " + emitRef(node) + ";\n"
        } else
          ""
      case x: Reg =>
        "  if (rand_init) " + emitRef(node) + ".randomize();\n"

      case x: Mem[_] =>
        "  if (rand_init) " + emitRef(node) + ".randomize();\n"

      case r: ROM[_] =>
        val res = new StringBuilder
        for (i <- 0 until r.lits.length)
          res append block((0 until words(r)).map(j => emitRef(r) + ".put(" + i + ", " + j + ", " + emitWordRef(r.lits(i), j) + ")"))
        res.toString

      case u: Bits => 
        if (u.driveRand && u.isInObject)
          "  if (rand_init) " + emitRef(node) + ".randomize();\n"
        else
          ""
      case _ =>
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

  def clkName (clock: Clock): String =
    (if (clock == Module.implicitClock) "" else "_" + emitRef(clock))

  def genHarness(c: Module, name: String) {
    val harness  = createOutputFile(name + "-emulator.cpp");
    harness.write("#include \"" + name + ".h\"\n");
    harness.write("int main (int argc, char* argv[]) {\n");
    harness.write("  " + name + "_t* c = new " + name + "_t();\n");
    harness.write("  int lim = (argc > 1) ? atoi(argv[1]) : -1;\n");
    harness.write("  int period;\n")
    if (Module.clocks.length > 1) {
      for (clock <- Module.clocks) {
        if (clock.srcClock == null) {
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
    if (Module.clocks.length > 1) {
      harness.write("    delta += c->clock(reset);\n")
    } else {
      harness.write("    c->clock_lo(reset);\n")
      harness.write("    c->clock_hi(reset);\n")
    }
    harness.write("  }\n")
    harness.write("  for (int t = 0; lim < 0 || t < lim; t++) {\n");
    harness.write("    dat_t<1> reset = LIT<1>(0);\n");
    harness.write("    if (!c->scan(stdin)) break;\n");
    if (Module.clocks.length > 1) {
      harness.write("    delta += c->clock(reset);\n")
      harness.write("    fprintf(stdout, \"%d\", delta);\n")
      harness.write("    fprintf(stdout, \"%s\", \" \");\n")
      harness.write("    c->print(stdout, stderr);\n")
      harness.write("    delta = 0;\n")
      if (Module.isVCD) { harness.write("    c->dump(f, t);\n"); }
    } else {
      harness.write("    c->clock_lo(reset);\n");
      harness.write("    c->print(stdout, stderr);\n");
      if (Module.isVCD) { harness.write("    c->dump(f, t);\n"); }
      harness.write("    c->clock_hi(reset);\n");
    }
    harness.write("  }\n");
    harness.write("}\n");
    harness.close();
  }

  override def compile(c: Module, flagsIn: String) {
    val flags = if (flagsIn == null) "-O2" else flagsIn

    val chiselENV = java.lang.System.getenv("CHISEL")
    val c11 = if(Module.printfs.size > 0) " -std=c++11 " else ""
    val allFlags = flags + c11 + " -I../ -I" + chiselENV + "/csrc/"
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
        case io: Bits  =>
          if (io.dir == INPUT) {
            res += "  " + emitRef(c) + "->" + n + " = " + emitRef(io.inputs(0)) + ";\n";
          }
      };
    }
    res += emitRef(c) + "->clock_lo(reset);\n";
    for ((n, w) <- c.wires) {
      w match {
        case io: Bits =>
          if (io.dir == OUTPUT) {
            res += "  " + emitRef(io.consumers(0)) + " = " + emitRef(c) + "->" + n + ";\n";
          }
      };
    }
    res
  }

  def emitDefHis(c: Module): String = {
    var res = emitRef(c) + "->clock_hi(reset);\n";
    res
  }

  /** Insures each node such that it has a unique name accross the whole
    hierarchy by prefixing its name by a component path (except for "reset"
    and all nodes in *c*). */
  def renameNodes(c: Module, nodes: Seq[Node]) {
    for (m <- nodes) {
      m match {
        case l: Literal => ;
        case any        =>
          if (m.name != "" && !(m == c.defaultResetPin) && !(m.component == null)) {
            // only modify name if it is not the reset signal or not in top component
            if(m.name != "reset" || !(m.component == c)) {
              m.name = m.component.getPathName + "__" + m.name;
            }
          }
      }
    }
  }

  def backendElaborate(c: Module) = super.elaborate(c)

  override def elaborate(c: Module): Unit = {
    println("CPP elaborate")
    super.elaborate(c)

    /* We flatten all signals in the toplevel component after we had
     a change to associate node and components correctly first
     otherwise we are bound for assertions popping up left and right
     in the Backend.elaborate method. */
    for (cc <- Module.components) {
      if (!(cc == c)) {
        c.debugs ++= cc.debugs
        c.mods       ++= cc.mods;
      }
    }
    c.findConsumers();
    c.verifyAllMuxes;
    ChiselError.checkpoint()

    c.collectNodes(c);
    c.findOrdering(); // search from roots  -- create omods
    renameNodes(c, c.omods);
    if (Module.isReportDims) {
      val (numNodes, maxWidth, maxDepth) = c.findGraphDims();
      ChiselError.info("NUM " + numNodes + " MAX-WIDTH " + maxWidth + " MAX-DEPTH " + maxDepth);
    }

    val clkDomains = new HashMap[Clock, (StringBuilder, StringBuilder)]
    for (clock <- Module.clocks) {
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
    if (!Params.space.isEmpty) {
      val out_p = createOutputFile(c.name + ".p");
      out_p.write(Params.toDotpStringParams);
      out_p.close();
    }
    out_h.write("#ifndef __" + c.name + "__\n");
    out_h.write("#define __" + c.name + "__\n\n");
    out_h.write("#include \"emulator.h\"\n\n");
    out_h.write("class " + c.name + "_t : public mod_t {\n");
    out_h.write(" public:\n");
    if (Module.isTesting && Module.tester != null) {
      Module.scanArgs.clear();  Module.scanArgs  ++= Module.tester.testInputNodes;    Module.scanFormat  = ""
      Module.printArgs.clear(); Module.printArgs ++= Module.tester.testNonInputNodes; Module.printFormat = ""

      for (n <- Module.scanArgs ++ Module.printArgs)
        if(!c.omods.contains(n)) c.omods += n
    }
    val vcd = new VcdBackend()
    for (m <- c.omods) {
      if(m.name != "reset") {
        if (m.isInObject) {
          out_h.write(emitDec(m));
        }
        if (m.isInVCD) {
          out_h.write(vcd.emitDec(m));
        }
      }
    }
    for (clock <- Module.clocks)
      out_h.write(emitDec(clock))

    out_h.write("\n");
    out_h.write("  void init ( bool rand_init = false );\n");
    for ( clock <- Module.clocks) {
      out_h.write("  void clock_lo" + clkName(clock) + " ( dat_t<1> reset );\n")
      out_h.write("  void clock_hi" + clkName(clock) + " ( dat_t<1> reset );\n")
    }
    out_h.write("  int clock ( dat_t<1> reset );\n")
    out_h.write("  void print ( FILE* f, FILE* e);\n");
    out_h.write("  bool scan ( FILE* f );\n");
    out_h.write("  void dump ( FILE* f, int t );\n");
    out_h.write("};\n\n");
    out_h.write(Params.toCxxStringParams);
    out_h.write("\n\n#endif\n");
    out_h.close();

    out_c.write("#include \"" + c.name + ".h\"\n");
    for(str <- Module.includeArgs) out_c.write("#include \"" + str + "\"\n");
    out_c.write("\n");
    out_c.write("void " + c.name + "_t::init ( bool rand_init ) {\n");
    for (m <- c.omods) {
      out_c.write(emitInit(m));
    }
    for (clock <- Module.clocks)
      out_c.write(emitInit(clock))
    out_c.write("}\n");

    for (m <- c.omods) {
      val clock = if (m.clock == null) Module.implicitClock else m.clock
      clkDomains(clock)._1.append(emitDefLo(m))
    }

    for (m <- c.omods) {
      val clock = if (m.clock == null) Module.implicitClock else m.clock
      clkDomains(clock)._2.append(emitInitHi(m))
    }

    for (m <- c.omods) {
      val clock = if (m.clock == null) Module.implicitClock else m.clock
      clkDomains(clock)._2.append(emitDefHi(m))
    }

    for (clk <- clkDomains.keys) {
      clkDomains(clk)._1.append("}\n")
      clkDomains(clk)._2.append("}\n")
      out_c.write(clkDomains(clk)._1.result)
      out_c.write(clkDomains(clk)._2.result)
    }

    out_c.write("int " + c.name + "_t::clock ( dat_t<1> reset ) {\n")
    out_c.write("  uint32_t min = ((uint32_t)1<<31)-1;\n")
    for (clock <- Module.clocks) {
      out_c.write("  if (" + emitRef(clock) + "_cnt < min) min = " + emitRef(clock) +"_cnt;\n")
    }
    for (clock <- Module.clocks) {
      out_c.write("  " + emitRef(clock) + "_cnt-=min;\n")
    }
    for (clock <- Module.clocks) {
      out_c.write("  if (" + emitRef(clock) + "_cnt == 0) clock_lo" + clkName(clock) + "( reset );\n")
    }
    for (clock <- Module.clocks) {
      out_c.write("  if (" + emitRef(clock) + "_cnt == 0) clock_hi" + clkName(clock) + "( reset );\n")
    }
    for (clock <- Module.clocks) {
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
    out_c.write("void " + c.name + "_t::print ( FILE* f, FILE* e ) {\n");
    for (p <- Module.printfs)
      out_c.write("#if __cplusplus >= 201103L\n"
        + "  if (" + emitLoWordRef(p.cond)
        + ") dat_fprintf<" + p.width + ">(e, "
        + p.args.map(emitRef _).foldLeft(CString(p.format))(_ + ", " + _)
        + ");\n"
        + "#endif\n")
    if (Module.printArgs.length > 0) {
      val format =
        if (Module.printFormat == "") {
          Module.printArgs.map(a => "%x").reduceLeft((y,z) => z + " " + y)
        } else {
          Module.printFormat;
        }
      val toks = splitFormat(format);
      var i = 0;
      for (tok <- toks) {
        if (tok(0) == '%') {
          val nodes = Module.printArgs(i).maybeFlatten
          for (j <- 0 until nodes.length)
            out_c.write("  fprintf(f, \"" + (if (j > 0) " " else "") +
                        "%s\", TO_CSTR(" + emitRef(nodes(j)) + "));\n");
          i += 1;
        } else {
          out_c.write("  fprintf(f, \"%s\", \"" + tok + "\");\n");
        }
      }
      out_c.write("  fprintf(f, \"\\n\");\n");
      out_c.write("  fflush(f);\n");
      out_c.write("  fflush(e);\n");
    }
    out_c.write("}\n");
    def constantArgSplit(arg: String): Array[String] = arg.split('=');
    def isConstantArg(arg: String): Boolean = constantArgSplit(arg).length == 2;
    out_c.write("bool " + c.name + "_t::scan ( FILE* f ) {\n");
    if (Module.scanArgs.length > 0) {
      val format =
        if (Module.scanFormat == "") {
          Module.scanArgs.map(a => "%x").reduceLeft((y,z) => z + y)
        } else {
          Module.scanFormat;
        }
      val toks = splitFormat(format);
      var i = 0;
      for (tok <- toks) {
        if (tok(0) == '%') {
          val nodes = c.keepInputs(Module.scanArgs(i).maybeFlatten)
          for (j <- 0 until nodes.length)
            out_c.write("  str_to_dat(read_tok(f), " + emitRef(nodes(j)) + ");\n");
          i += 1;
        } else {
          out_c.write("  fscanf(f, \"%s\", \"" + tok + "\");\n");
        }
      }
      // out_c.write("  getc(f);\n");
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
