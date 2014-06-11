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

class CppBackend extends Backend {
  val keywords = new HashSet[String]();
  private var hasPrintfs = false

  override def emitTmp(node: Node): String = {
    require(false)
    if (node.isInObject) {
      emitRef(node)
    } else {
      "dat_t<" + node.needWidth() + "> " + emitRef(node)
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
  def wordMangle(x: Node, w: String): String = x match {
    case _: Literal =>
      if (words(x) == 1) emitRef(x)
      else s"T${x.emitIndex}[${w}]"
    case _ =>
      if (x.isInObject) s"${emitRef(x)}.values[${w}]"
      else if (words(x) == 1) emitRef(x)
      else s"${emitRef(x)}[${w}]"
  }
  def emitLit(value: BigInt, w: Int = 0): String = {
    val hex = value.toString(16)
    "0x" + (if (hex.length > bpw/4*w) hex.slice(hex.length-bpw/4*(w + 1), hex.length-bpw/4*w) else 0) + "L"
  }
  def wordMangle(x: Node, w: Int): String =
    if (w >= words(x)) "0L"
    else x match {
      case l: Literal =>
        val lit = l.value
        val value = if (lit < 0) (BigInt(1) << x.needWidth()) + lit else lit
        emitLit(value, w)
      case _ => wordMangle(x, w.toString)
    }
  def isLit(node: Node): Boolean = node.isLit || node.isInstanceOf[Bits] && node.inputs.length == 1 && isLit(node.inputs.head)
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

  // Returns a list of tuples (type, name) of variables needed by a node
  def nodeVars(node: Node): List[(String, String)] = {
    node match {
      case x: Binding =>
        List()
      case x: Literal =>
        List()
      case x: Reg =>
        List((s"dat_t<${node.needWidth()}>", emitRef(node)))
      case m: Mem[_] =>
        List((s"mem_t<${m.needWidth()},${m.n}>", emitRef(m)))
      case r: ROMData =>
        List((s"mem_t<${r.needWidth()},${r.n}>", emitRef(r)))
      case c: Clock =>
        List(("int", emitRef(node)),
             ("int", emitRef(node) + "_cnt"))
      case _ =>
        List((s"dat_t<${node.needWidth()}>", emitRef(node)))
    }
  }
  
  override def emitDec(node: Node): String = {
    val out = new StringBuilder("") 
    for (varDef <- nodeVars(node)) {
      out.append(s"  ${varDef._1} ${varDef._2};\n")
    }
    out.toString()
  }

  def emitCircuitAssign(srcPrefix:String, node: Node): String = {
    val out = new StringBuilder("") 
    for (varDef <- nodeVars(node)) {
      out.append(s"  ${varDef._2} = ${srcPrefix}${varDef._2};\n")
    }
    out.toString()
  }

  val bpw = 64
  def words(node: Node): Int = (node.needWidth() - 1) / bpw + 1
  def fullWords(node: Node): Int = node.needWidth()/bpw
  def emitLoWordRef(node: Node): String = emitWordRef(node, 0)
  def emitTmpDec(node: Node): String =
    if (node.isInObject) ""
    else if (words(node) == 1) s"  val_t ${emitRef(node)};\n"
    else s"  val_t ${emitRef(node)}[${words(node)}];\n"
  def block(s: Seq[String]): String = 
    if (s.length == 0) ""
    else s"  {${s.map(" " + _ + ";").reduceLeft(_ + _)}}\n"
  def emitDatRef(x: Node): String = {
    val gotWidth = x.needWidth()
    if (x.isInObject) emitRef(x)
    else if (words(x) > 1) s"*reinterpret_cast<dat_t<${gotWidth}>*>(${emitRef(x)})"
    else if (isLit(x)) s"dat_t<${gotWidth}>(${emitRef(x)})"
    else s"*reinterpret_cast<dat_t<${gotWidth}>*>(&${emitRef(x)})"
  }
  def trunc(x: Node): String = {
    val gotWidth = x.needWidth()
    if (gotWidth % bpw == 0) ""
    else s"  ${emitWordRef(x, words(x)-1)} = ${emitWordRef(x, words(x)-1)} & ${emitLit((BigInt(1) << (gotWidth%bpw))-1)};\n"
  }
  def opFoldLeft(o: Op, initial: (String, String) => String, subsequent: (String, String, String) => String) =
    (1 until words(o.inputs(0))).foldLeft(initial(emitLoWordRef(o.inputs(0)), emitLoWordRef(o.inputs(1))))((c, i) => subsequent(c, emitWordRef(o.inputs(0), i), emitWordRef(o.inputs(1), i)))

  def emitLog2(x: Node, priEnc: Boolean = false) = {
    val (func, range) =
      if (priEnc) ("priority_encode_1", (0 until words(x.inputs(0))-1))
      else ("log2_1", (words(x.inputs(0))-1 to 1 by -1))
    val body = range.map(i => s"${emitWordRef(x.inputs(0), i)} != 0, ${(i*bpw)} + ${func}(${emitWordRef(x.inputs(0), i)})")
                    .foldRight(s"${func}(${emitLoWordRef(x.inputs(0))})")((x, y) => s"TERNARY(${x}, ${y})")
    s"  ${emitLoWordRef(x)} = ${body};\n"
  }

  def emitDefLo(node: Node): String = {
    node match {
      case x: Mux =>
        val op = if (!x.inputs.exists(isLit _)) "TERNARY_1" else "TERNARY"
        emitTmpDec(x) +
        block((0 until words(x)).map(i => s"${emitWordRef(x, i)} = ${op}(${emitLoWordRef(x.inputs(0))}, ${emitWordRef(x.inputs(1), i)}, ${emitWordRef(x.inputs(2), i)})"))

      case o: Op => {
        emitTmpDec(o) +
        (if (o.inputs.length == 1) {
          (if (o.op == "^") {
            val res = ArrayBuffer[String]()
            res += "val_t __x = " + (0 until words(o.inputs(0))).map(emitWordRef(o.inputs(0), _)).reduceLeft(_ + " ^ " + _)
            for (i <- log2Up(min(bpw, o.inputs(0).needWidth()))-1 to 0 by -1)
              res += "__x = (__x >> " + (1L << i) + ") ^ __x"
            res += emitLoWordRef(o) + " = __x & 1"
            block(res)
          } else if (o.op == "~") {
            block((0 until words(o)).map(i => emitWordRef(o, i) + " = ~" + emitWordRef(o.inputs(0), i))) + trunc(o)
          } else if (o.op == "f-")
            "  " + emitLoWordRef(o) + " = fromFloat(-(toFloat(" + emitLoWordRef(o.inputs(0)) + "));\n"
          else if (o.op == "fsin")
            "  " + emitLoWordRef(o) + " = fromFloat(sin(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "fcos")
            "  " + emitLoWordRef(o) + " = fromFloat(cos(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "ftan")
            "  " + emitLoWordRef(o) + " = fromFloat(tan(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "fasin")
            "  " + emitLoWordRef(o) + " = fromFloat(asin(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "facos")
            "  " + emitLoWordRef(o) + " = fromFloat(acos(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "fatan")
            "  " + emitLoWordRef(o) + " = fromFloat(atan(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "fsqrt")
            "  " + emitLoWordRef(o) + " = fromFloat(sqrt(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "flog")
            "  " + emitLoWordRef(o) + " = fromFloat(log(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "ffloor")
            "  " + emitLoWordRef(o) + " = fromFloat(floor(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "fceil")
            "  " + emitLoWordRef(o) + " = fromFloat(ceil(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "fround")
            "  " + emitLoWordRef(o) + " = fromFloat(round(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "fToSInt")
            "  " + emitLoWordRef(o) + " = (val_t)(toFloat(" + emitLoWordRef(o.inputs(0)) + "));\n"
          else if (o.op == "d-")
            "  " + emitLoWordRef(o) + " = fromDouble(-(toDouble(" + emitLoWordRef(o.inputs(0)) + "));\n"
          else if (o.op == "dsin")
            "  " + emitLoWordRef(o) + " = fromDouble(sin(toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "dcos")
            "  " + emitLoWordRef(o) + " = fromDouble(cos(toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "dtan")
            "  " + emitLoWordRef(o) + " = fromDouble(tan(toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "dasin")
            "  " + emitLoWordRef(o) + " = fromDouble(asin(toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "dacos")
            "  " + emitLoWordRef(o) + " = fromDouble(acos(toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "datan")
            "  " + emitLoWordRef(o) + " = fromDouble(atan(toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "dlog")
            "  " + emitLoWordRef(o) + " = fromDouble(log(toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "dsqrt")
            "  " + emitLoWordRef(o) + " = fromDouble(sqrt(toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "dfloor")
            "  " + emitLoWordRef(o) + " = fromDouble(floor(toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "dceil")
            "  " + emitLoWordRef(o) + " = fromDouble(ceil(toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "dround")
            "  " + emitLoWordRef(o) + " = fromDouble(round(toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
          else if (o.op == "dToSInt")
            "  " + emitLoWordRef(o) + " = (val_t)(toDouble(" + emitLoWordRef(o.inputs(0)) + "));\n"
          else if (o.op == "Log2")
            emitLog2(o)
          else if (o.op == "PriEnc" || o.op == "OHToUInt")
            emitLog2(o, true)
          else {
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
        } else if (o.op == "*" || o.op == "/") {
          if (o.op == "*" && o.needWidth() <= bpw) {
            s"  ${emitLoWordRef(o)} = ${emitLoWordRef(o.inputs(0))} ${o.op} ${emitLoWordRef(o.inputs(1))};\n"
          } else {
            s"  ${emitDatRef(o)} = ${emitDatRef(o.inputs(0))} ${o.op} ${emitDatRef(o.inputs(1))};\n"
          }
        } else if (o.op == "<<") {
          if (o.needWidth() <= bpw) {
            "  " + emitLoWordRef(o) + " = " + emitLoWordRef(o.inputs(0)) + " << " + emitLoWordRef(o.inputs(1)) + ";\n" + trunc(o)
          } else {
            var shb = emitLoWordRef(o.inputs(1))
            val res = ArrayBuffer[String]()
            res += s"val_t __c = 0"
            res += s"val_t __w = ${emitLoWordRef(o.inputs(1))} / ${bpw}"
            res += s"val_t __s = ${emitLoWordRef(o.inputs(1))} % ${bpw}"
            res += s"val_t __r = ${bpw} - __s"
            for (i <- 0 until words(o)) {
              val inputWord = wordMangle(o.inputs(0), s"CLAMP(${i}-__w, 0, ${words(o.inputs(0)) - 1})")
              res += s"val_t __v${i} = MASK(${inputWord}, (${i} >= __w) & (${i} < __w + ${words(o.inputs(0))}))"
              res += s"${emitWordRef(o, i)} = __v${i} << __s | __c"
              res += s"__c = MASK(__v${i} >> __r, __s != 0)"
            }
            block(res) + trunc(o)
          }
        } else if (o.op == ">>" || o.op == "s>>") {
          val arith = o.op == "s>>"
          val gotWidth = o.inputs(0).needWidth()
          if (gotWidth <= bpw) {
            if (arith) {
              s"  ${emitLoWordRef(o)} = sval_t(${emitLoWordRef(o.inputs(0))} << ${bpw - gotWidth}) >> (${bpw - gotWidth} + ${emitLoWordRef(o.inputs(1))});\n" + trunc(o)
            } else {
              s"  ${emitLoWordRef(o)} = ${emitLoWordRef(o.inputs(0))} >> ${emitLoWordRef(o.inputs(1))};\n"
            }
          } else {
            var shb = emitLoWordRef(o.inputs(1))
            val res = ArrayBuffer[String]()
            res += s"val_t __c = 0"
            res += s"val_t __w = ${emitLoWordRef(o.inputs(1))} / ${bpw}"
            res += s"val_t __s = ${emitLoWordRef(o.inputs(1))} % ${bpw}"
            res += s"val_t __r = ${bpw} - __s"
            if (arith)
              res += s"val_t __msb = (sval_t)${emitWordRef(o.inputs(0), words(o)-1)} << ${(bpw - o.needWidth() % bpw) % bpw} >> ${(bpw-1)}"
            for (i <- words(o)-1 to 0 by -1) {
              val inputWord = wordMangle(o.inputs(0), s"CLAMP(${i}+__w, 0, ${words(o.inputs(0))-1})")
              res += s"val_t __v${i} = MASK(${inputWord}, __w + ${i} < ${words(o.inputs(0))})"
              res += s"${emitWordRef(o, i)} = __v${i} >> __s | __c"
              res += s"__c = MASK(__v${i} << __r, __s != 0)"
              if (arith) {
	        val gotWidth = o.needWidth()
                res += s"${emitWordRef(o, i)} |= MASK(__msb << ((${gotWidth-1}-${emitLoWordRef(o.inputs(1))}) % ${bpw}), ${(i + 1) * bpw} > ${gotWidth-1} - ${emitLoWordRef(o.inputs(1))})"
                res += s"${emitWordRef(o, i)} |= MASK(__msb, ${i*bpw} >= ${gotWidth-1} - ${emitLoWordRef(o.inputs(1))})"
              }
            }
            if (arith) {
	      val gotWidth = o.needWidth()
              res += emitLoWordRef(o) + " |= MASK(__msb << ((" + (gotWidth-1) + "-" + emitLoWordRef(o.inputs(1)) + ") % " + bpw + "), " + bpw + " > " + (gotWidth-1) + "-" + emitLoWordRef(o.inputs(1)) + ")"
            }
            block(res) + (if (arith) trunc(o) else "")
          }
        } else if (o.op == "##") {
          val lsh = o.inputs(1).needWidth()
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
        } else if (o.op == "|" || o.op == "&" || o.op == "^") {
          block((0 until words(o)).map(i => s"${emitWordRef(o, i)} = ${emitWordRef(o.inputs(0), i)} ${o.op} ${emitWordRef(o.inputs(1), i)}"))
        } else if (o.op == "s<") {
          require(o.inputs(1).litOf.value == 0)
          val shamt = (o.inputs(0).needWidth()-1) % bpw
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
        } else if (o.op == "f-") {
            "  " + emitLoWordRef(o) + " = fromFloat(toFloat(" + emitLoWordRef(o.inputs(0)) + ") - toFloat(" + emitLoWordRef(o.inputs(1)) + "));\n"
        } else if (o.op == "f+") {
            "  " + emitLoWordRef(o) + " = fromFloat(toFloat(" + emitLoWordRef(o.inputs(0)) + ") + toFloat(" + emitLoWordRef(o.inputs(1)) + "));\n"
        } else if (o.op == "f*") {
            "  " + emitLoWordRef(o) + " = fromFloat(toFloat(" + emitLoWordRef(o.inputs(0)) + ") * toFloat(" + emitLoWordRef(o.inputs(1)) + "));\n"
        } else if (o.op == "f/") {
            "  " + emitLoWordRef(o) + " = fromFloat(toFloat(" + emitLoWordRef(o.inputs(0)) + ") / toFloat(" + emitLoWordRef(o.inputs(1)) + "));\n"
        } else if (o.op == "f%") {
            "  " + emitLoWordRef(o) + " = fromFloat(fmodf(toFloat(" + emitLoWordRef(o.inputs(0)) + "), toFloat(" + emitLoWordRef(o.inputs(1)) + ")));\n"
        } else if (o.op == "fpow") {
            "  " + emitLoWordRef(o) + " = fromFloat(pow(toFloat(" + emitLoWordRef(o.inputs(1)) + "), toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
        } else if (o.op == "f==") {
            "  " + emitLoWordRef(o) + " = toFloat(" + emitLoWordRef(o.inputs(0)) + ") == toFloat(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else if (o.op == "f!=") {
            "  " + emitLoWordRef(o) + " = toFloat(" + emitLoWordRef(o.inputs(0)) + ") != toFloat(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else if (o.op == "f>") {
            "  " + emitLoWordRef(o) + " = toFloat(" + emitLoWordRef(o.inputs(0)) + ") > toFloat(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else if (o.op == "f<=") {
            "  " + emitLoWordRef(o) + " = toFloat(" + emitLoWordRef(o.inputs(0)) + ") <= toFloat(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else if (o.op == "f>=") {
            "  " + emitLoWordRef(o) + " = toFloat(" + emitLoWordRef(o.inputs(0)) + ") >= toFloat(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else if (o.op == "d-") {
            "  " + emitLoWordRef(o) + " = fromDouble(toDouble(" + emitLoWordRef(o.inputs(0)) + ") - toDouble(" + emitLoWordRef(o.inputs(1)) + "));\n"
        } else if (o.op == "d+") {
            "  " + emitLoWordRef(o) + " = fromDouble(toDouble(" + emitLoWordRef(o.inputs(0)) + ") + toDouble(" + emitLoWordRef(o.inputs(1)) + "));\n"
        } else if (o.op == "d*") {
            "  " + emitLoWordRef(o) + " = fromDouble(toDouble(" + emitLoWordRef(o.inputs(0)) + ") * toDouble(" + emitLoWordRef(o.inputs(1)) + "));\n"
        } else if (o.op == "d/") {
            "  " + emitLoWordRef(o) + " = fromDouble(toDouble(" + emitLoWordRef(o.inputs(0)) + ") / toDouble(" + emitLoWordRef(o.inputs(1)) + "));\n"
        } else if (o.op == "d%") {
            "  " + emitLoWordRef(o) + " = fromDouble(fmod(toDouble(" + emitLoWordRef(o.inputs(0)) + "), toDouble(" + emitLoWordRef(o.inputs(1)) + ")));\n"
        } else if (o.op == "dpow") {
            "  " + emitLoWordRef(o) + " = fromDouble(pow(toDouble(" + emitLoWordRef(o.inputs(1)) + "), toDouble(" + emitLoWordRef(o.inputs(0)) + ")));\n"
        } else if (o.op == "d==") {
            "  " + emitLoWordRef(o) + " = toDouble(" + emitLoWordRef(o.inputs(0)) + ") == toDouble(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else if (o.op == "d!=") {
            "  " + emitLoWordRef(o) + " = toDouble(" + emitLoWordRef(o.inputs(0)) + ") != toDouble(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else if (o.op == "d>") {
            "  " + emitLoWordRef(o) + " = toDouble(" + emitLoWordRef(o.inputs(0)) + ") > toDouble(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else if (o.op == "d<=") {
            "  " + emitLoWordRef(o) + " = toDouble(" + emitLoWordRef(o.inputs(0)) + ") <= toDouble(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else if (o.op == "d>=") {
            "  " + emitLoWordRef(o) + " = toDouble(" + emitLoWordRef(o.inputs(0)) + ") >= toDouble(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else {
          assert(false, "operator " + o.op + " unsupported")
          ""
        })
      }

      case x: Extract =>
        x.inputs.tail.foreach(e => x.validateIndex(e))
        emitTmpDec(node) +
        (if (node.inputs.length < 3 || node.needWidth() == 1) {
          if (node.inputs(1).isLit) {
            val value = node.inputs(1).litValue().toInt
            "  " + emitLoWordRef(node) + " = (" + emitWordRef(node.inputs(0), value/bpw) + " >> " + (value%bpw) + ") & 1;\n"
          } else if (node.inputs(0).needWidth() <= bpw) {
            "  " + emitLoWordRef(node) + " = (" + emitLoWordRef(node.inputs(0)) + " >> " + emitLoWordRef(node.inputs(1)) + ") & 1;\n"
          } else {
            val inputWord = wordMangle(node.inputs(0), emitLoWordRef(node.inputs(1)) + "/" + bpw)
            s"${emitLoWordRef(node)} = ${inputWord} >> (${emitLoWordRef(node.inputs(1))} % ${bpw}) & 1"
          }
        } else {
          val rsh = node.inputs(2).litValue().toInt
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

      case x: Clock =>
        ""

      case x: Bits =>
        if (x.isInObject && x.inputs.length == 1) {
          emitTmpDec(x) + block((0 until words(x)).map(i => emitWordRef(x, i)
            + " = " + emitWordRef(x.inputs(0), i)))
        } else if (x.inputs.length == 0 && !x.isInObject) {
          emitTmpDec(x) + block("val_t __r = this->__rand_val()" +:
            (0 until words(x)).map(i => s"${emitWordRef(x, i)} = __r")) + trunc(x)
        } else {
          ""
        }

      case m: MemRead =>
        emitTmpDec(m) + block((0 until words(m)).map(i => emitWordRef(m, i)
          + " = " + emitRef(m.mem) + ".get(" + emitLoWordRef(m.addr) + ", "
          + i + ")"))

      case r: ROMRead =>
        emitTmpDec(r) + block((0 until words(r)).map(i => emitWordRef(r, i)
          + " = " + emitRef(r.rom) + ".get(" + emitLoWordRef(r.addr) + ", "
          + i + ")"))

      case a: Assert =>
        val cond = emitLoWordRef(a.cond) +
          (if (emitRef(a.cond) == "reset") "" else " || reset.lo_word()")
        "  ASSERT(" + cond + ", " + CString(a.message) + ");\n"

      case s: Sprintf =>
        ("#if __cplusplus >= 201103L\n"
          + "  " + emitRef(s) + " = dat_format<" + s.needWidth() + ">("
          + s.args.map(emitRef _).foldLeft(CString(s.format))(_ + ", " + _)
          + ");\n"
          + "#endif\n")

      case l: Literal =>
        if (words(l) == 1) ""
        else s"  val_t T${l.emitIndex}[] = {" + (0 until words(l)).map(emitWordRef(l, _)).reduce(_+", "+_) + "};\n"

      case _ =>
        ""
    }
  }

  def emitRefHi(node: Node): String = node match {
    case reg: Reg =>
      if (reg.next.isReg) emitRef(reg) + "__shadow"
      else emitRef(reg.next)
    case _ => emitRef(node)
  }

  def emitDefHi(node: Node): String = {
    node match {
      case reg: Reg =>
        s"  ${emitRef(reg)} = ${emitRefHi(reg)};\n"

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
        s"  ${emitRef(node)}.randomize(&__rand_seed);\n"

      case x: Mem[_] =>
        s"  ${emitRef(node)}.randomize(&__rand_seed);\n"

      case r: ROMData =>
        val res = new StringBuilder
        val sparse = !isPow2(r.n) || r.n != r.sparseLits.size
        if (sparse)
          res append s"  ${emitRef(r)}.randomize(&__rand_seed);\n"
        for ((i, v) <- r.sparseLits) {
          assert(v.value != None)
          val w = Some(v.value)
          if (sparse || w != 0)
            res append block((0 until words(r)).map(j => emitRef(r) + ".put(" + i + ", " + j + ", " + emitWordRef(v, j) + ")"))
        }
        res.toString

      case u: Bits => 
        if (u.driveRand && u.isInObject)
          s"   ${emitRef(node)}.randomize(&__rand_seed);\n"
        else
          ""
      case _ =>
        ""
    }
  }

  def emitInitHi(node: Node): String = {
    node match {
      case reg: Reg =>
        s"  dat_t<${node.needWidth()}> ${emitRef(reg)}__shadow = ${emitRef(reg.next)};\n"

      case m: MemWrite =>
        block((0 until words(m)).map(i =>
          s"if (${emitLoWordRef(m.cond)}) ${emitRef(m.mem)}" +
          s".put(${emitLoWordRef(m.addr)}, " +
          s"${i}, ${emitWordRef(m.data, i)})"))

      case _ =>
        ""
    }
  }

  def clkName (clock: Clock): String =
    (if (clock == Driver.implicitClock) "" else "_" + emitRef(clock))

  def genHarness(c: Module, name: String) {
    val harness  = createOutputFile(name + "-emulator.cpp");
    harness.write("#include \"" + name + ".h\"\n\n");
    if (Driver.clocks.length > 1) {
      harness.write("void " + c.name + "_t::setClocks ( std::vector< int > &periods ) {\n");
      var i = 0;
      for (clock <- Driver.clocks) {
        if (clock.srcClock == null) {
          harness.write("  " + emitRef(clock) + " = periods[" + i + "];\n")
          harness.write("  " + emitRef(clock) + "_cnt = periods[" + i + "];\n")
          i += 1;
        }
      }
      harness.write("}\n\n");
    }
    harness.write(s"""int main (int argc, char* argv[]) {\n""");
    harness.write(s"""  ${name}_t* module = new ${name}_t();\n""");
    harness.write(s"""  module->init();\n""");
    harness.write(s"""  ${name}_api_t* api = new ${name}_api_t();\n""");
    harness.write(s"""  api->init(module);\n""");
    if (Driver.isVCD) {
      val basedir = ensureDir(Driver.targetDir)
      harness.write(s"""  FILE *f = fopen("${basedir}${name}.vcd", "w");\n""");
    } else {
      harness.write(s"""  FILE *f = NULL;\n""");
    }
    if (Driver.dumpTestInput) {
      harness.write(s"""  FILE *tee = fopen("${name}.stdin", "w");\n""");
    } else {
      harness.write(s"""  FILE *tee = NULL;""");
    }
    harness.write(s"""  module->set_dumpfile(f);\n""");
    harness.write(s"""  api->set_teefile(tee);\n""");
    harness.write(s"""  api->read_eval_print_loop();\n""");
    harness.write(s"""  fclose(f);\n""");
    harness.write(s"""  fclose(tee);\n""");
    harness.write(s"""}\n""");
    harness.close();
  }

  override def compile(c: Module, flagsIn: String) {
    val flags = if (flagsIn == null) "-O2" else flagsIn

    val chiselENV = java.lang.System.getenv("CHISEL")
    val c11 = if (hasPrintfs) " -std=c++11 " else ""
    val allFlags = flags + c11 + " -I../ -I" + chiselENV + "/csrc/"
    val dir = Driver.targetDir + "/"
    val CXX = scala.util.Properties.envOrElse("CXX", "g++" )
    def run(cmd: String) {
      val bashCmd = Seq("bash", "-c", cmd)
      val c = bashCmd.!
      ChiselError.info(cmd + " RET " + c)
    }
    def link(name: String) {
      val ac = CXX + " -o " + dir + name + " " + dir + name + ".o " + dir + name + "-emulator.o"
      run(ac)
    }
    def cc(name: String) {
      val cmd = CXX + " -c -o " + dir + name + ".o " + allFlags + " " + dir + name + ".cpp"
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

  /** Ensures each node such that it has a unique name accross the whole
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

  /**
   * Takes a list of nodes and returns a list of tuples with the names attached.
   * Used to preserve original node names before the rename process.
   */
  def generateNodeMapping(nodes: Seq[Node]): ArrayBuffer[Tuple2[String, Node]] = {
    val mappings = new ArrayBuffer[Tuple2[String, Node]]
    for (m <- nodes) {
      if (m.chiselName != "") {
        val mapping = (m.chiselName, m)
        mappings += mapping
      }
    }
    return mappings
  }

  def emitMapping(mapping: Tuple2[String, Node]): String = {
    val (name, node) = mapping
    node match {
      case x: Binding =>
        ""
      case x: Literal =>
        ""
      case x: Reg =>
        s"""  dat_table["${name}"] = new dat_api<${node.needWidth()}>(&mod_typed->${emitRef(node)}, "${name}", "");\n"""
      case m: Mem[_] =>
        s"""  mem_table["${name}"] = new mem_api<${m.needWidth()}, ${m.n}>(&mod_typed->${emitRef(node)}, "${name}", "");\n"""
      case r: ROMData =>
        s"""  mem_table["${name}"] = new mem_api<${r.needWidth()}, ${r.n}>(&mod_typed->${emitRef(node)}, "${name}", "");\n"""
      case c: Clock =>
        s"""  dat_table["${name}"] = new dat_api<${node.needWidth()}>(&mod_typed->${emitRef(node)}, "${name}", "");\n"""
      case _ =>
        s"""  dat_table["${name}"] = new dat_api<${node.needWidth()}>(&mod_typed->${emitRef(node)}, "${name}", "");\n"""
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
    for (cc <- Driver.components) {
      if (!(cc == c)) {
        c.debugs ++= cc.debugs
        c.mods   ++= cc.mods;
      }
    }
    c.findConsumers()
    ChiselError.checkpoint()

    c.collectNodes(c);
    c.findOrdering(); // search from roots  -- create omods
    val mappings = generateNodeMapping(c.omods);
    renameNodes(c, c.omods);
    if (Driver.isReportDims) {
      val (numNodes, maxWidth, maxDepth) = c.findGraphDims();
      ChiselError.info("NUM " + numNodes + " MAX-WIDTH " + maxWidth + " MAX-DEPTH " + maxDepth);
    }

    val clkDomains = new HashMap[Clock, (StringBuilder, StringBuilder)]
    for (clock <- Driver.clocks) {
      val clock_lo = new StringBuilder
      val clock_hi = new StringBuilder
      clkDomains += (clock -> ((clock_lo, clock_hi)))
      clock_lo.append("void " + c.name + "_t::clock_lo" + clkName(clock) + " ( dat_t<1> reset ) {\n")
      clock_hi.append("void " + c.name + "_t::clock_hi" + clkName(clock) + " ( dat_t<1> reset ) {\n")
    }

    if (Driver.isGenHarness) {
      genHarness(c, c.name);
    }
    val out_h = createOutputFile(c.name + ".h");
    if (!Params.space.isEmpty) {
      val out_p = createOutputFile(c.name + ".p");
      out_p.write(Params.toDotpStringParams);
      out_p.close();
    }
    
    // Generate header file
    out_h.write("#ifndef __" + c.name + "__\n");
    out_h.write("#define __" + c.name + "__\n\n");
    out_h.write("#include \"emulator.h\"\n\n");
    
    // Generate module headers
    out_h.write("class " + c.name + "_t : public mod_t {\n");
    out_h.write(" private:\n");
    out_h.write("  val_t __rand_seed;\n");
    out_h.write("  void __srand(val_t seed) { __rand_seed = seed; }\n");
    out_h.write("  val_t __rand_val() { return ::__rand_val(&__rand_seed); }\n");
    out_h.write(" public:\n");
    val vcd = new VcdBackend(c)
    def headerOrderFunc(a: Node, b: Node) = {
      // pack smaller objects at start of header for better locality
      val aMem = a.isInstanceOf[Mem[_]] || a.isInstanceOf[ROMData]
      val bMem = b.isInstanceOf[Mem[_]] || b.isInstanceOf[ROMData]
      aMem < bMem || aMem == bMem && a.needWidth() < b.needWidth()
    }
    for (m <- c.omods.filter(_.isInObject).sortWith(headerOrderFunc))
      out_h.write(emitDec(m))
    for (m <- c.omods.filter(_.isInVCD).sortWith(headerOrderFunc))
      out_h.write(vcd.emitDec(m))
    for (clock <- Driver.clocks)
      out_h.write(emitDec(clock))

    out_h.write("\n");
    out_h.write("  void init ( val_t rand_init = 0 );\n");
    for ( clock <- Driver.clocks) {
      out_h.write("  void clock_lo" + clkName(clock) + " ( dat_t<1> reset );\n")
      out_h.write("  void clock_hi" + clkName(clock) + " ( dat_t<1> reset );\n")
    }
    out_h.write("  int clock ( dat_t<1> reset );\n")
    if (Driver.clocks.length > 1) {
      out_h.write("  void setClocks ( std::vector< int >& periods );\n")
    }
    out_h.write("  mod_t* clone();\n");
    out_h.write("  bool set_circuit_from(mod_t* src);\n");
    out_h.write("  void print ( FILE* f );\n");
    out_h.write("  void dump ( FILE* f, int t );\n");
    out_h.write("  void dump_init ( FILE* f );\n\n");
    out_h.write("};\n\n");
    out_h.write(Params.toCxxStringParams);
    
    // Generate API headers
    out_h.write(s"class ${c.name}_api_t : public mod_api_t {\n");
    out_h.write(s"  void init_mapping_table();\n");
    out_h.write(s"};\n\n");
    
    out_h.write("\n\n#endif\n");
    out_h.close();

    // Generate CPP files
    val out_cpps = ArrayBuffer[java.io.FileWriter]()
    val all_cpp = new StringBuilder
    def createCppFile(suffix: String = "-" + out_cpps.length) = {
      val f = createOutputFile(c.name + suffix + ".cpp")
      f.write("#include \"" + c.name + ".h\"\n")
      for (str <- Driver.includeArgs) f.write("#include \"" + str + "\"\n")
      f.write("\n")
      out_cpps += f
      f
    }
    def writeCppFile(s: String) = {
      out_cpps.last.write(s)
      all_cpp.append(s)
    }

    createCppFile()
    
    // generate init block
    writeCppFile("void " + c.name + "_t::init ( val_t rand_init ) {\n")
    writeCppFile("  this->__srand(rand_init);\n")
    for (m <- c.omods) {
      writeCppFile(emitInit(m))
    }
    for (clock <- Driver.clocks) {
      writeCppFile(emitInit(clock))
    }
    writeCppFile("}\n")

    def clock(n: Node) = if (n.clock == null) Driver.implicitClock else n.clock

    for (m <- c.omods)
      clkDomains(clock(m))._1.append(emitDefLo(m))

    for (m <- c.omods)
      clkDomains(clock(m))._2.append(emitInitHi(m))

    for (m <- c.omods)
      clkDomains(clock(m))._2.append(emitDefHi(m))

    for (clk <- clkDomains.keys) {
      clkDomains(clk)._1.append("}\n")
      clkDomains(clk)._2.append("}\n")
    }

    // generate clock(...) function
    writeCppFile("int " + c.name + "_t::clock ( dat_t<1> reset ) {\n")
    writeCppFile("  uint32_t min = ((uint32_t)1<<31)-1;\n")
    for (clock <- Driver.clocks) {
      writeCppFile("  if (" + emitRef(clock) + "_cnt < min) min = " + emitRef(clock) +"_cnt;\n")
    }
    for (clock <- Driver.clocks) {
      writeCppFile("  " + emitRef(clock) + "_cnt-=min;\n")
    }
    for (clock <- Driver.clocks) {
      writeCppFile("  if (" + emitRef(clock) + "_cnt == 0) clock_lo" + clkName(clock) + "( reset );\n")
    }
    for (clock <- Driver.clocks) {
      writeCppFile("  if (" + emitRef(clock) + "_cnt == 0) clock_hi" + clkName(clock) + "( reset );\n")
    }
    for (clock <- Driver.clocks) {
      writeCppFile("  if (" + emitRef(clock) + "_cnt == 0) " + emitRef(clock) + "_cnt = " +
                  emitRef(clock) + ";\n")
    }
    writeCppFile("  return min;\n")
    writeCppFile("}\n")

    // generate clone() function
    writeCppFile(s"mod_t* ${c.name}_t::clone() {\n")
    writeCppFile(s"  mod_t* cloned = new ${c.name}_t(*this);\n")
    writeCppFile(s"  return cloned;\n")
    writeCppFile(s"}\n")
    
    // generate set_circuit_from function
    writeCppFile(s"bool ${c.name}_t::set_circuit_from(mod_t* src) {\n")
    writeCppFile(s"  ${c.name}_t* mod_typed = dynamic_cast<${c.name}_t*>(src);\n")
    writeCppFile(s"  assert(mod_typed);\n")
    
    for (m <- c.omods) {
      if(m.name != "reset" && m.isInObject) {
        writeCppFile(emitCircuitAssign("mod_typed->", m))
      }
    }
    for (clock <- Driver.clocks) {
      writeCppFile(emitCircuitAssign("mod_typed->", clock))
    }
    writeCppFile("  return true;\n")
    writeCppFile(s"}\n")
    
    // generate print(...) function
    writeCppFile("void " + c.name + "_t::print ( FILE* f ) {\n")
    for (cc <- Driver.components; p <- cc.printfs) {
      hasPrintfs = true
      writeCppFile("#if __cplusplus >= 201103L\n"
        + "  if (" + emitLoWordRef(p.cond)
        + ") dat_fprintf<" + p.needWidth() + ">(f, "
        + p.args.map(emitRef _).foldLeft(CString(p.format))(_ + ", " + _)
        + ");\n"
        + "#endif\n")
    }
    if (hasPrintfs)
      writeCppFile("fflush(f);\n");
    writeCppFile("}\n")
    
    createCppFile()
    vcd.dumpVCDInit(writeCppFile)

    createCppFile()
    vcd.dumpVCD(writeCppFile)

    for (out <- clkDomains.values.map(_._1) ++ clkDomains.values.map(_._2)) {
      createCppFile()
      writeCppFile(out.result)
    }

    // Generate API functions
    createCppFile()
    writeCppFile(s"void ${c.name}_api_t::init_mapping_table() {\n");
    writeCppFile(s"  dat_table.clear();\n")
    writeCppFile(s"  mem_table.clear();\n")
    writeCppFile(s"  ${c.name}_t* mod_typed = dynamic_cast<${c.name}_t*>(module);\n")
    writeCppFile(s"  assert(mod_typed);\n")
    for (m <- mappings) {
      if (m._2.name != "reset" && (m._2.isInObject || m._2.isInVCD)) {
        writeCppFile(emitMapping(m))
      }
    }
    writeCppFile(s"}\n");
    
    createCppFile("")
    writeCppFile(all_cpp.result)
    out_cpps.foreach(_.close)

    def copyToTarget(filename: String) = {
	  val resourceStream = getClass().getResourceAsStream("/" + filename)
	  if( resourceStream != null ) {
	    val classFile = createOutputFile(filename)
	    while(resourceStream.available > 0) {
	      classFile.write(resourceStream.read())
	    }
	    classFile.close()
	    resourceStream.close()
	  } else {
		println(s"WARNING: Unable to copy '$filename'" )
	  }
    }
    /* Copy the emulator headers into the targetDirectory. */
    copyToTarget("emulator_mod.h")
    copyToTarget("emulator_api.h")
    copyToTarget("emulator.h")
  }

}
