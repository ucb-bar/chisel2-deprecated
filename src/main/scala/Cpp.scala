/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
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
import java.io.BufferedReader
import java.io.InputStreamReader
import scala.sys.process._
import sys.process.stringSeqToProcess
import Node._
import Reg._
import ChiselError._
import Literal._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import PartitionIslands._

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
  val unoptimizedFiles = HashSet[String]()
  val onceOnlyFiles = HashSet[String]()
  var cloneFile: String = ""
  var maxFiles: Int = 0
  val compileInitializationUnoptimized = Driver.compileInitializationUnoptimized
  // If we're dealing with multiple files for the purpose of separate
  //   optimization levels indicate we expect to compile multiple files.
  val compileMultipleCppFiles = Driver.compileInitializationUnoptimized
  // Suppress generation of the monolithic .cpp file if we're compiling
  // multiple files.
  // NOTE: For testing purposes, we may want to generate this file anyway.
  val suppressMonolithicCppFile = compileMultipleCppFiles  /* || false */
  // Compile the clone method at -O0
  val cloneCompiledO0 = true
  // Define shadow registers in the circuit object, instead of local registers in the clock hi methods.
  // This is required if we're generating paritioned combinatorial islands, or we're limiting the size of functions/methods.
  val shadowRegisterInObject = Driver.shadowRegisterInObject || Driver.partitionIslands || Driver.lineLimitFunctions > 0
  // Sets to manage allocation and generation of shadow registers
  val regWritten = HashSet[Node]()
  val needShadow = HashSet[Node]()
  val allocatedShadow = HashSet[Node]()
  var potentialShadowRegisters = 0
  val allocateOnlyNeededShadowRegisters = Driver.allocateOnlyNeededShadowRegisters
  val ignoreShadows = false
  val shadowPrefix = if (ignoreShadows) {
    "// "
  } else {
    ""
  }

  override def emitTmp(node: Node): String = {
    require(false)
    if (node.isInObject) {
      emitRef(node)
    } else {
      "dat_t<" + node.needWidth() + "> " + emitRef(node)
    }
  }

  // Give different names to temporary nodes in CppBackend
  override def emitRef(node: Node): String = {
    node match {
      case _: Bits if !node.isInObject && node.inputs.length == 1 => 
        emitRef(node.inputs(0))
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
        List((s"dat_t<${node.needWidth()}>", emitRef(node))) ++ {
          if (!allocateOnlyNeededShadowRegisters || needShadow.contains(node)) {
            // Add an entry for the shadow register in the main object.
            if (shadowRegisterInObject) {
              List((s"${shadowPrefix} dat_t<${node.needWidth()}>", shadowPrefix + emitRef(node) + s"__shadow"))
            } else {
              Nil
            }
          } else {
            Nil
          }
        }
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
    else if (words(x) > 1) s"*reinterpret_cast<dat_t<${gotWidth}>*>(&${emitRef(x)})"
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
        if (!Driver.isAssert) ""
        else "  ASSERT(" + cond + ", " + CString(a.message) + ");\n"

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
      if (reg.next.isReg && (!allocateOnlyNeededShadowRegisters || needShadow.contains(reg))) {
        emitRef(reg) + "__shadow"
      }
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

  // If we write a register node before we use its inputs, we need to shadow it.
  def determineRequiredShadowRegisters(node: Node) {
    node match {
      case reg: Reg => {
        regWritten += reg
      }
      case _ => {
        for (n <- node.inputs if regWritten.contains(n)) {
          needShadow += n
        }
      }
    }
  }

  def emitInitHi(node: Node): String = {
    node match {
      case reg: Reg => {
        potentialShadowRegisters += 1
        val allocateShadow = !allocateOnlyNeededShadowRegisters || needShadow.contains(reg)
        if (allocateShadow) {
          allocatedShadow += reg
          val storagePrefix = if (shadowRegisterInObject) {
            ""
          } else {
            " dat_t<" + node.needWidth()  + ">"
          }
          s"${shadowPrefix} ${storagePrefix} ${emitRef(reg)}__shadow = ${emitRef(reg.next)};\n"
        } else {
          s" ${emitRef(reg)} = ${emitRef(reg.next)};\n"
        }
      }

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
    val CXXFLAGS = scala.util.Properties.envOrElse("CXXFLAGS", "" )
    val LDFLAGS = scala.util.Properties.envOrElse("LDFLAGS", "")
    val chiselENV = java.lang.System.getenv("CHISEL")

    val c11 = if (hasPrintfs) " -std=c++11 " else ""
    val cxxFlags = (if (flagsIn == null) CXXFLAGS else flagsIn) + c11
    val cppFlags = scala.util.Properties.envOrElse("CPPFLAGS", "") + " -I../ -I" + chiselENV + "/csrc/"
    val allFlags = cppFlags + " " + cxxFlags
    val dir = Driver.targetDir + "/"
    val CXX = scala.util.Properties.envOrElse("CXX", "g++" )
    val parallelMakeJobs = Driver.parallelMakeJobs

    def run(cmd: String) {
      val bashCmd = Seq("bash", "-c", cmd)
      val c = bashCmd.!
      ChiselError.info(cmd + " RET " + c)
    }
    def linkOne(name: String) {
      val ac = CXX + " " + LDFLAGS + " -o " + dir + name + " " + dir + name + ".o " + dir + name + "-emulator.o"
      run(ac)
    }
    def linkMany(name: String, objects: Seq[String]) {
      val ac = CXX + " " + LDFLAGS + " -o " + dir + name + " " + objects.map(dir + _ + ".o ").mkString(" ") + dir + name + "-emulator.o"
      run(ac)
    }
    def cc(name: String, flags: String = allFlags) {
      val cmd = CXX + " -c -o " + dir + name + ".o " + flags + " " + dir + name + ".cpp"
      run(cmd)
    }

    def make(args: String) {
      // We explicitly unset CPPFLAGS and CXXFLAGS so the values
      // set in the Makefile will take effect.
      val cmd = "unset CPPFLAGS CXXFLAGS; make " + args
      run(cmd)
    }

    def editToTarget(filename: String, replacements: HashMap[String, String]) = {
      val resourceStream = getClass().getResourceAsStream("/" + filename)
      if( resourceStream != null ) {
        val makefile = createOutputFile(filename)
        val br = new BufferedReader(new InputStreamReader(resourceStream, "US-ASCII"))
        while(br.ready()) {
          val inputLine = br.readLine()
          // Break the line into "tokens"
          val tokens = inputLine.split("""(?=@\w+@)""")
          // Reassemble the line plugging in expansions for "macro"
          val outputLine = tokens.map(s => {
            if (s.length > 2 && s.charAt(0) == '@' && s.takeRight(1) == "@") {
              replacements(s)
            } else {
              s
            }
          }) mkString ""
          makefile.write(outputLine + "\n")
        }
        makefile.close()
        br.close()
      } else {
        println(s"WARNING: Unable to copy '$filename'" )
      }
    }

    // Compile all the unoptimized files at a (possibly) lower level of optimization.
    if (cloneCompiledO0) {
      onceOnlyFiles += cloneFile
    }
    // Set the default optimization levels.
    var optim0 = "-O0"
    var optim1 = "-O1"
    var optim2 = "-O2"
    // Is the caller explicitly setting -Osomething? If yes, we'll honor that setting
    //  and set our default optimization flags to "".
    val Oregex = new scala.util.matching.Regex("""[^\w]*(-O.+)""", "explicitO")
    val Omatch = Oregex.findFirstMatchIn(allFlags)
    Omatch match {
      case Some(m) => {
        println("Cpp: observing explicit '" + m.group("explicitO") + "'")
        optim0 = ""
        optim1 = ""
        optim2 = ""
      }
      case _ => {}
    }

    // Are we compiling multiple cpp files?
    if (compileMultipleCppFiles) {
      // Are we using a Makefile template and parallel makes?
      if (parallelMakeJobs != 0) {
        // Build the replacement string map for the Makefile template.
        val replacements = HashMap[String, String] ()
        val onceOnlyOFiles = onceOnlyFiles.map(_ + ".o") mkString " "
        val unoptimizedOFiles = unoptimizedFiles.filter( ! onceOnlyFiles.contains(_) ).map(_ + ".o") mkString " "
        val optimzedOFiles = ((for {
          f <- 0 until maxFiles
          basename = c.name + "-" + f
          if !unoptimizedFiles.contains(basename)
        } yield basename + ".o"
        ) mkString " ") + " " + c.name + "-emulator.o"
  
        replacements += (("@HFILES@", ""))
        replacements += (("@ONCEONLY@", onceOnlyOFiles))
        replacements += (("@UNOPTIMIZED@", unoptimizedOFiles))
        replacements += (("@OPTIMIZED@", optimzedOFiles))
        replacements += (("@EXEC@", c.name))
        replacements += (("@CPPFLAGS@", cppFlags))
        replacements += (("@CXXFLAGS@", cxxFlags))
        replacements += (("@OPTIM0@", optim0))
        replacements += (("@OPTIM1@", optim1))
        replacements += (("@OPTIM2@", optim2))
        // Read and edit the Makefile template.
        editToTarget("Makefile", replacements)
        val nJobs = if (parallelMakeJobs > 0) "-j" + parallelMakeJobs.toString() else "-j"
        make(nJobs)
      } else {
        // No make. Compile everything discretely.
        cc(c.name + "-emulator")
        // We should have unoptimized files.
        assert(unoptimizedFiles.size != 0 || onceOnlyFiles.size != 0,
          "no unoptmized files to compile for '--compileMultipleCppFiles'")
        // Compile the O0 files.
        onceOnlyFiles.map(cc(_, allFlags + " " + optim0))
  
        // Compile the remaining (O1) files.
        unoptimizedFiles.filter( ! onceOnlyFiles.contains(_) ).map(cc(_, allFlags + " " + optim1))
        val objects: ArrayBuffer[String] = new ArrayBuffer(maxFiles)
        // Compile the remainder at the specified optimization level.
        for (f <- 0 until maxFiles) {
          val basename = c.name + "-" + f
          // If we've already compiled this file, don't do it again,
          // but do add it to the list of objects to be linked.
          if (!unoptimizedFiles.contains(basename)) {
            cc(basename, allFlags + " " + optim2)
          }
          objects += basename
        }
        linkMany(c.name, objects)
      }
    } else {
      cc(c.name + "-emulator")
      cc(c.name)
      linkOne(c.name)
    }
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
            res += "  " + emitRef(io.consumers.head) + " = " + emitRef(c) + "->" + n + ";\n";
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
  def renameNodes(nodes: ArrayBuffer[Node]) {
    val comp = Driver.topComponent
    for (m <- nodes) {
      m match {
        case _: Literal =>
        case _ if m.named && (m != comp.defaultResetPin) && m.component != null =>
          // only modify name if it is not the reset signal or not in top component
          if (m.name != "reset" || m.component != comp)
            m.name = m.component.getPathName + "__" + m.name
        case _ =>
      }
    }
  }

  /**
   * Takes a list of nodes and returns a list of tuples with the names attached.
   * Used to preserve original node names before the rename process.
   */
  def generateNodeMapping = {
    val mappings = new ArrayBuffer[(String, Node)]
    for (m <- Driver.orderedNodes) {
      if (m.chiselName != "") {
        val mapping = (m.chiselName, m)
        mappings += mapping
      }
    }
    mappings
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
    val minimumLinesPerFile = Driver.minimumLinesPerFile
    val partitionIslands = Driver.partitionIslands
    val lineLimitFunctions = Driver.lineLimitFunctions

    // Generate CPP files
    val out_cpps = ArrayBuffer[CppFile]()
    val all_cpp = new StringBuilder

    def cppFileSuffix = "-" + out_cpps.length
    class CppFile(val suffix: String = cppFileSuffix) {
      var lines = 0
      var done = false
      val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName) 
      val name = n + suffix + ".cpp"
      val hfilename = n + ".h"
      var fileWriter = createOutputFile(name)
      fileWriter.write("#include \"" + hfilename + "\"\n")
      for (str <- Driver.includeArgs) fileWriter.write("#include \"" + str + "\"\n")
      fileWriter.write("\n")
      lines = 3


      def write(s: String) {
        lines += s.count(_ == '\n')
        fileWriter.write(s)
      }
      
      def close() {
        done = true
        fileWriter.close()
      }
      def advance() {
        this.done = true
      }
    }

    class LineLimitedFunction(functionNamePrefix: String, maxLines: Int, functionCodePrefix: String, functionCodeSuffix: String = "}\n", functionSignature: String = " ", functionArgs: String = " ", functionClass: String = c.name + "_t") {
      var bodyLines = 0
      val body = new StringBuilder
      val bodies = new scala.collection.mutable.Queue[String]
      private def functionName(i: Int): String = {
        functionNamePrefix + "_" + i.toString
      }
      private def newBody() {
        if (body.length > 0) {
          bodies += body.result
          body.clear()
        }
        bodyLines = 0
      }
      def addString(s: String) {
        if (s == "") {
          return
        }
        val lines = s.count(_ == '\n')
        if (maxLines > 0 && lines + bodyLines > maxLines) {
          newBody()
        }
        body.append(s)
        bodyLines += lines
      }
      def genCalls() {
        var offset = 0
        while (bodies.length > offset) {
          val functionCall = functionName(offset) + "(" + functionArgs + ");\n"
          addString(functionCall)
          offset += 1
        }
      }
      def done() {
        // Close off any body building in progress.
        newBody()
        if (bodies.length > 1) {
          genCalls()
          newBody()
        }
      }
      def getBodies(): String = {
        val bodycalls = new StringBuilder
        bodies.length match {
          case 0 => bodycalls.append(functionCodePrefix + functionCodeSuffix)
          case 1 => {
            bodycalls.append(functionCodePrefix)
            bodycalls.append(bodies.dequeue())
            bodycalls.append(functionCodeSuffix)
          }
          case _ => {
            for (i <- 0 until bodies.length - 1) {
              bodycalls.append("void " + functionClass + "::" + functionName(i) + "(" + functionSignature + ") {\n")
              bodycalls.append(bodies.dequeue())
              bodycalls.append("}\n")
            }
            bodycalls.append(functionCodePrefix)
            bodycalls.append(bodies.dequeue())
            bodycalls.append(functionCodeSuffix)
          }
        }
        bodycalls.result
      }
    }
    def createCppFile(suffix: String = cppFileSuffix) {
      // If we're trying to coalesce cpp files (minimumLinesPerFile > 0),
      //  don't actually create a new file unless we've hit the line limit.
      if ((out_cpps.size > 0) && (minimumLinesPerFile == 0 || out_cpps.last.lines < minimumLinesPerFile) && !out_cpps.last.done) {
        out_cpps.last.write("\n\n")
      } else {
        out_cpps += new CppFile(suffix)
        println("CppBackend: createCppFile " + out_cpps.last.name)
      }
    }
    def writeCppFile(s: String) {
      out_cpps.last.write(s)
      if (! suppressMonolithicCppFile) {
        all_cpp.append(s)
      }
    }
    def advanceCppFile() {
      out_cpps.last.advance()
    }

    // Generate header file
    def genHeader(vcd: Backend, islands: Array[Island], nInitMethods: Int, nSetCircuitMethods: Int, nDumpInitMethods: Int, nDumpMethods: Int, nInitMappingTableMethods: Int) {
      val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName) 
      val out_h = createOutputFile(n + ".h");
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

      def headerOrderFunc(a: Node, b: Node) = {
        // pack smaller objects at start of header for better locality
        val aMem = a.isInstanceOf[Mem[_]] || a.isInstanceOf[ROMData]
        val bMem = b.isInstanceOf[Mem[_]] || b.isInstanceOf[ROMData]
        aMem < bMem || aMem == bMem && a.needWidth() < b.needWidth()
      }
      for (m <- Driver.orderedNodes.filter(_.isInObject).sortWith(headerOrderFunc))
        out_h.write(emitDec(m))
      for (m <- Driver.orderedNodes.filter(_.isInVCD).sortWith(headerOrderFunc))
        out_h.write(vcd.emitDec(m))
      for (clock <- Driver.clocks)
        out_h.write(emitDec(clock))
  
      out_h.write("\n");

      // If we're generating multiple init methods, wrap them in private/public.
      if (nInitMethods > 1) {
        out_h.write(" private:\n");
        for (i <- 0 until nInitMethods - 1) {
          out_h.write("  void init_" + i + " ( );\n");
        }
        out_h.write(" public:\n");
      }
      out_h.write("  void init ( val_t rand_init = 0 );\n");

      var generatedPrivateClockPrototypes = false
      for ( clock <- Driver.clocks) {
        val clockNameStr = clkName(clock).toString()
        for (island <- islands if island.islandId != 0) {
          val islandId = island.islandId
          val suffix = clockNameStr + "_I_" + islandId + " ( dat_t<1> reset );\n"
          if (!generatedPrivateClockPrototypes) {
            out_h.write(" private:\n");
            generatedPrivateClockPrototypes = true
          }
          out_h.write("  void clock_lo" + suffix)
          out_h.write("  void clock_ihi" + suffix)
          out_h.write("  void clock_dhi" + suffix)
        }
        out_h.write("  void clock_lo" + clockNameStr + " ( dat_t<1> reset );\n")
        out_h.write("  void clock_hi" + clockNameStr + " ( dat_t<1> reset );\n")
      }
      if (generatedPrivateClockPrototypes) {
        out_h.write(" public:\n");
      }
      out_h.write("  int clock ( dat_t<1> reset );\n")
      if (Driver.clocks.length > 1) {
        out_h.write("  void setClocks ( std::vector< int >& periods );\n")
      }

      out_h.write("  mod_t* clone();\n");

      // If we're generating multiple set_circuit methods, wrap them in private/public.
      if (nSetCircuitMethods > 1) {
        out_h.write(" private:\n");
        for (i <- 0 until nSetCircuitMethods - 1) {
          out_h.write("  void set_circuit_from_" + i + " ( " + c.name + "_t* mod_typed );\n");
        }
        out_h.write(" public:\n");
      }
      out_h.write("  bool set_circuit_from(mod_t* src);\n");
      out_h.write("  void print ( FILE* f );\n");

      // If we're generating multiple dump methods, wrap them in private/public.
      if (nDumpMethods > 1) {
        out_h.write(" private:\n");
        for (i <- 0 until nDumpMethods - 1) {
          out_h.write("  void dump_" + i + " ( FILE* f );\n");
        }
        out_h.write(" public:\n");
      }
      out_h.write("  void dump ( FILE* f, int t );\n");
 
      // If we're generating multiple dump_init methods, wrap them in private/public.
      if (nDumpInitMethods > 1) {
        out_h.write(" private:\n");
        for (i <- 0 until nDumpInitMethods - 1) {
          out_h.write("  void dump_init_" + i + " ( FILE* f );\n");
        }
        out_h.write(" public:\n");
      }
      out_h.write("  void dump_init ( FILE* f );\n");
     
      // All done with the class definition. Close it off.
      out_h.write("\n};\n\n");
      out_h.write(Params.toCxxStringParams);
      
      // Generate API headers
      out_h.write(s"class ${c.name}_api_t : public mod_api_t {\n");
      // If we're generating multiple init_mapping_table( methods, wrap them in private/public.
      if (nInitMappingTableMethods > 1) {
        out_h.write(" private:\n");
        for (i <- 0 until nInitMappingTableMethods - 1) {
          out_h.write("  void init_mapping_table_" + i + " ( " + c.name + "_t* mod_typed );\n");
        }
        out_h.write(" public:\n");
      }

      out_h.write(s"  void init_mapping_table();\n");
      out_h.write(s"};\n\n");
      
      out_h.write("\n\n#endif\n");
      out_h.close();
    }

    def genInitMethod(): Int = {
      createCppFile()
      val head = "void " + c.name + "_t::init ( val_t rand_init ) {\n" +
                 "  this->__srand(rand_init);\n"
      val llf = new LineLimitedFunction("init", lineLimitFunctions, head)
      for (m <- Driver.orderedNodes) {
        llf.addString(emitInit(m))
      }
      for (clock <- Driver.clocks) {
        llf.addString(emitInit(clock))
      }
      llf.done()
      val nFunctions = llf.bodies.length
      writeCppFile(llf.getBodies)
      nFunctions
    }

    def genClockMethod() {
      createCppFile()
      writeCppFile("int " + c.name + "_t::clock ( dat_t<1> reset ) {\n")
      writeCppFile("  uint32_t min = ((uint32_t)1<<31)-1;\n")
      for (clock <- Driver.clocks) {
        writeCppFile("  if (" + emitRef(clock) + "_cnt < min) min = " + emitRef(clock) +"_cnt;\n")
      }
      for (clock <- Driver.clocks) {
        writeCppFile("  " + emitRef(clock) + "_cnt-=min;\n")
      }
      for (clock <- Driver.clocks) {
        writeCppFile("  if (" + emitRef(clock) + "_cnt == 0) clock_hi" + clkName(clock) + "( reset );\n")
      }
      for (clock <- Driver.clocks) {
        writeCppFile("  if (" + emitRef(clock) + "_cnt == 0) clock_lo" + clkName(clock) + "( reset );\n")
      }
      for (clock <- Driver.clocks) {
        writeCppFile("  if (" + emitRef(clock) + "_cnt == 0) " + emitRef(clock) + "_cnt = " +
                    emitRef(clock) + ";\n")
      }
      writeCppFile("  return min;\n")
      writeCppFile("}\n")
    }

    def genCloneMethod() {
      createCppFile()
      writeCppFile(s"mod_t* ${c.name}_t::clone() {\n")
      writeCppFile(s"  mod_t* cloned = new ${c.name}_t(*this);\n")
      writeCppFile(s"  return cloned;\n")
      writeCppFile(s"}\n")

      // Make a special note of the clone file. We may want to compile it -O0.
      cloneFile = out_cpps.last.name.dropRight(".cpp".length())
    }

    def genSetCircuitFromMethod(): Int = {
      createCppFile()
      val head = s"bool ${c.name}_t::set_circuit_from(mod_t* src) {\n" +
                 s"  ${c.name}_t* mod_typed = dynamic_cast<${c.name}_t*>(src);\n" +
                 s"  assert(mod_typed);\n"
      val tail = "  return true;\n}\n"
      val llf = new LineLimitedFunction("set_circuit_from", lineLimitFunctions, head, tail, c.name + "_t* mod_typed", "mod_typed")
      for (m <- Driver.orderedNodes) {
        if(m.name != "reset" && m.isInObject) {
          llf.addString(emitCircuitAssign("mod_typed->", m))
        }
      }
      for (clock <- Driver.clocks) {
        llf.addString(emitCircuitAssign("mod_typed->", clock))
      }
      llf.done()
      val nFunctions = llf.bodies.length
      writeCppFile(llf.getBodies)
      nFunctions
    }

    def genPrintMethod() {
      createCppFile()
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
    }

    def genDumpInitMethod(vcd: VcdBackend): Int = {
      createCppFile()
      val head = "void " + c.name + "_t::dump_init(FILE *f) {\n"
      val llf = new LineLimitedFunction("dump_init", lineLimitFunctions, head, "}\n", "FILE *f", "f")
      vcd.dumpVCDInit(llf.addString)
      llf.done()
      val nFunctions = llf.bodies.length
      writeCppFile(llf.getBodies)
      nFunctions
    }

    def genDumpMethod(vcd: VcdBackend): Int = {
      createCppFile()
      var head = "void " + c.name + "_t::dump(FILE *f, int t) {\n"
      val tail = "}\n"
      // Are we actually generating VCD?
      if (Driver.isVCD) {
        // Yes. dump is a real function.
        head += "  if (t == 0) return dump_init(f);\n" +
                "  fprintf(f, \"#%d\\n\", t);\n"
        // Are we generating a large dump function with gotos? (i.e., not inline)
        if (Driver.isVCDinline) {
          val llf = new LineLimitedFunction("dump", lineLimitFunctions, head, tail, "FILE *f", "f")
          vcd.dumpVCD(llf.addString)
          llf.done()
          val nFunctions = llf.bodies.length
          writeCppFile(llf.getBodies)
          nFunctions
        } else {
          // We're creating a VCD dump function with gotos.
          writeCppFile(head)
          vcd.dumpVCD(writeCppFile)
          writeCppFile(tail)
          1
        }
      } else {
        // No. Just generate the dummy (nop) function.
        writeCppFile(head + tail)
        1
      }
    }

    def genInitMappingTableMethod(mappings: ArrayBuffer[Tuple2[String, Node]]): Int = {
      createCppFile()
      val head = s"void ${c.name}_api_t::init_mapping_table() {\n" +
                 s"  dat_table.clear();\n" +
                 s"  mem_table.clear();\n" +
                 s"  ${c.name}_t* mod_typed = dynamic_cast<${c.name}_t*>(module);\n" +
                 s"  assert(mod_typed);\n"
      val llf = new LineLimitedFunction("init_mapping_table", lineLimitFunctions, head, "}\n", c.name + "_t* mod_typed", "mod_typed", c.name + "_api_t")
      for (m <- mappings) {
        if (m._2.name != "reset" && (m._2.isInObject || m._2.isInVCD)) {
          llf.addString(emitMapping(m))
        }
      }
      llf.done()
      val nFunctions = llf.bodies.length
      writeCppFile(llf.getBodies)

      // Add the init_mapping_table file to the list of unoptimized files.
      if (compileInitializationUnoptimized) {
        val trimLength = ".cpp".length()
        unoptimizedFiles += out_cpps.last.name.dropRight(trimLength)
      }
      nFunctions
    }

    println("CPP elaborate")
    super.elaborate(c)

    /* We flatten all signals in the toplevel component after we had
     a change to associate node and components correctly first
     otherwise we are bound for assertions popping up left and right
     in the Backend.elaborate method. */
    flattenAll // created Driver.orderedNodes
    ChiselError.checkpoint()

    val mappings = generateNodeMapping
    renameNodes(Driver.orderedNodes)
    if (Driver.isReportDims) {
      val (numNodes, maxWidth, maxDepth) = findGraphDims
      ChiselError.info("NUM " + numNodes + " MAX-WIDTH " + maxWidth + " MAX-DEPTH " + maxDepth);
    }

    // If we're partitioning a monolithic circuit into separate islands
    // of combinational logic, generate those islands now.
    val islands = if (partitionIslands) {
      createIslands()
    } else {
      val e = ArrayBuffer[Island]()
      e += new Island(0, new IslandNodes, new IslandNodes)
      e.toArray
    }
    val maxIslandId = islands.map(_.islandId).max
    val nodeToIslandArray = generateNodeToIslandArray(islands)

    class ClockDomains {
      type ClockCodeStrings = HashMap[Clock, (StringBuilder, StringBuilder, StringBuilder)]
      val code = new ClockCodeStrings
      val islandClkCode = new HashMap[Int, ClockCodeStrings]
      val islandStarted = Array.fill(3, maxIslandId + 1)(0)    // An array to keep track of the first time we added code to an island.
      val islandOrder = Array.fill(3, islands.size)(0)         // An array to keep track of the order in which we should output island code.
      var islandSequence = Array.fill(3)(0)
      val showProgress = false

      for (clock <- Driver.clocks) {
        val clock_dlo = new StringBuilder
        val clock_ihi = new StringBuilder
        val clock_dhi = new StringBuilder
        code += (clock -> ((clock_dlo, clock_ihi, clock_dhi)))
        clock_dlo.append("void " + c.name + "_t::clock_lo" + clkName(clock) + " ( dat_t<1> reset ) {\n")
        clock_ihi.append("void " + c.name + "_t::clock_hi" + clkName(clock) + " ( dat_t<1> reset ) {\n")
        // If we're generating islands of combinational logic,
        // have the main clock code call the island specific code,
        // and generate that island specific clock_(hi|lo) code.
        if (partitionIslands) {
          for (island <- islands) {
            val islandId = island.islandId
            // Do we need a new entry for this mapping?
            if (!islandClkCode.contains(islandId)) {
              islandClkCode += ((islandId, new ClockCodeStrings))
            }
            val clock_dlo_I = new StringBuilder
            val clock_ihi_I = new StringBuilder
            val clock_dhi_I = new StringBuilder
            islandClkCode(islandId) += (clock -> ((clock_dlo_I, clock_ihi_I, clock_dhi_I)))
            clock_dlo_I.append("void " + c.name + "_t::clock_lo" + clkName(clock) + "_I_" + islandId + " ( dat_t<1> reset ) {\n")
            clock_ihi_I.append("void " + c.name + "_t::clock_ihi" + clkName(clock) + "_I_" + islandId + " ( dat_t<1> reset ) {\n")
            clock_dhi_I.append("void " + c.name + "_t::clock_dhi" + clkName(clock) + "_I_" + islandId + " ( dat_t<1> reset ) {\n")
          }
        }
      }

      def clock(n: Node) = if (n.clock == null) Driver.implicitClock else n.clock

      def populate() {
        var nodeCount = 0
        def isNodeInIsland(node: Node, island: Island): Boolean = {
          return island == null || nodeToIslandArray(node._id).contains(island)
        }

        // Return tuple of booleans if we actually added any clock code.
        def addClkDefs(n: Node, codeStrings: ClockCodeStrings): (Boolean, Boolean, Boolean) = {
          val defLo = emitDefLo(n)
          val initHi = emitInitHi(n)
          val defHi = emitDefHi(n)
          val clockNode = clock(n)
          if (defLo != "" || initHi != "" || defHi != "") {
            codeStrings(clockNode)._1.append(defLo)
            codeStrings(clockNode)._2.append(initHi)
            codeStrings(clockNode)._3.append(defHi)
          }
          (defLo != "", initHi != "", defHi != "")
        }

        // Should we determine which shadow registers we need?
        if (allocateOnlyNeededShadowRegisters || true) {
          for (n <- Driver.orderedNodes) {
            determineRequiredShadowRegisters(n)
          }
        }

        // Are we generating partitioned islands?
        if (!partitionIslands) {
          // No. Generate and output a single, monolithic function.
          for (m <- Driver.orderedNodes) {
            addClkDefs(m, code)
          }
          // For the non-patitioned case, we're going to merge the clock_hi init and def code into a single function.
          // Add the closing brace to the def string.
          for (clk <- code.keys) {
            code(clk)._1.append("}\n")
            code(clk)._3.append("}\n")
          }
        } else {
          val addedCode = new Array[Boolean](3)
          for (m <- Driver.orderedNodes) {
            for (island <- islands) {
              if (isNodeInIsland(m, island)) {
                val islandId = island.islandId
                val codeStrings = islandClkCode(islandId)
                val addedCodeTuple = addClkDefs(m, codeStrings)
                addedCode(0) = addedCodeTuple._1
                addedCode(1) = addedCodeTuple._2
                addedCode(2) = addedCodeTuple._3
                // Update the generation number if we added any code to this island.
                for (lohi <- 0 to 2) {
                  if (addedCode(lohi)) {
                    // Is this the first time we've added code to this island?
                    if (islandStarted(lohi)(islandId) == 0) {
                      islandOrder(lohi)(islandSequence(lohi)) = islandId
                      islandSequence(lohi) += 1
                      islandStarted(lohi)(islandId) = islandSequence(lohi)
                    }
                  }
                }
              }
            }
            nodeCount += 1
            if (showProgress && (nodeCount % 1000) == 0) {
              println("ClockDomains: populated " + nodeCount + " nodes.")
            }
          }
          for (codeStrings <- islandClkCode.values) {
            for (clk <- codeStrings.keys) {
              codeStrings(clk)._1.append("}\n")
              codeStrings(clk)._2.append("}\n")
              codeStrings(clk)._3.append("}\n")
            }
          }
        }
      }

      def outputAllClkDomains() {
        // Are we generating partitioned islands?
        if (!partitionIslands) {
          for (out <- code.values.map(_._1) ++ (code.values.map(x => (x._2.append(x._3))))) {
            createCppFile()
            writeCppFile(out.result)
          }
        } else {
          // Output the clock code in the correct order.
          for (islandId <- islandOrder(0) if islandId > 0) {
            for (out <- islandClkCode(islandId).values.map(_._1)) {
              createCppFile()
              writeCppFile(out.result)
              out.clear()         // free the memory.
            }
          }
          for ((clock, clkcodes) <- code) {
            val (clock_lo, clock_ihi, clock_dhi) = clkcodes
            createCppFile()
            writeCppFile(clock_lo.result)
            clock_lo.clear()      // free the memory
            // Output the actual calls to the island specific clock code.
            for (islandId <- islandOrder(0) if islandId > 0) {
              writeCppFile("\t" + c.name + "_t::clock_lo" + clkName(clock) + "_I_" + islandId + "(reset);\n")
            }
            writeCppFile("}\n")
          }

          // Output the island-specific clock init code
          for (islandId <- islandOrder(1) if islandId > 0) {
            for (out <- islandClkCode(islandId).values.map(_._2)) {
              createCppFile()
              writeCppFile(out.result)
              out.clear()         // free the memory.
            }
          }

          // Output the island-specific clock def code
          for (islandId <- islandOrder(1) if islandId > 0) {
            for (out <- islandClkCode(islandId).values.map(_._3)) {
              createCppFile()
              writeCppFile(out.result)
              out.clear()         // free the memory.
            }
          }

          // Output the code to call the island-specific clock hi code.
          for ((clock, clkcodes) <- code) {
            val (clock_lo, clock_ihi, clock_dhi) = clkcodes
            createCppFile()
            writeCppFile(clock_ihi.result)
            clock_ihi.clear()      // free the memory
            // Output the actual calls to the island specific clock code.
            for (islandId <- islandOrder(1) if islandId > 0) {
              writeCppFile("\t" + c.name + "_t::clock_ihi" + clkName(clock) + "_I_" + islandId + "(reset);\n")
            }
            for (islandId <- islandOrder(1) if islandId > 0) {
              writeCppFile("\t" + c.name + "_t::clock_dhi" + clkName(clock) + "_I_" + islandId + "(reset);\n")
            }
            writeCppFile("}\n")
          }
       }
      }
    }
      
    val clkDomains = new ClockDomains

    val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName) 
    if (Driver.isGenHarness) {
      genHarness(c, n);
    }
    if (!Params.space.isEmpty) {
      val out_p = createOutputFile(c.name + ".p");
      out_p.write(Params.toDotpStringParams);
      out_p.close();
    }

    val vcd = new VcdBackend(c)

    ChiselError.info("populating clock domains")
    clkDomains.populate()

    println("CppBackend::elaborate: need " + needShadow.size + ", redundant " + (potentialShadowRegisters - needShadow.size) + " shadow registers")
    // Generate CPP files
    ChiselError.info("generating cpp files")
    
    // generate init block
    val nInitMethods = genInitMethod()
    
    // generate clock(...) function
    genClockMethod()

    advanceCppFile()
    // generate clone() function
    genCloneMethod()

    advanceCppFile()
    // generate set_circuit_from function
    val nSetCircuitFromMethods = genSetCircuitFromMethod()

    // generate print(...) function.
    // This will probably end up in the same file as the above clone code.
    genPrintMethod()

    advanceCppFile()
    val nDumpInitMethods = genDumpInitMethod(vcd)

    createCppFile()
    val nDumpMethods = genDumpMethod(vcd)

    out_cpps.foreach(_.fileWriter.flush())
    // If we're compiling initialization functions -O0, add the current files
    //  to the unoptimized file list.
    //  We strip off the trailing ".cpp" to facilitate creating both ".cpp" and ".o" files.
    if (compileInitializationUnoptimized) {
      val trimLength = ".cpp".length()
      unoptimizedFiles ++= out_cpps.map(_.name.dropRight(trimLength))
    }
    // Ensure we start off in a new file before we start outputting the clock_lo/hi.
    advanceCppFile()
    clkDomains.outputAllClkDomains()

    advanceCppFile()
    // Generate API functions
    val nInitMappingTableMethods = genInitMappingTableMethod(mappings)

    // Finally, generate the header - once we know how many methods we'll have.
    genHeader(vcd, islands, nInitMethods, nSetCircuitFromMethods, nDumpInitMethods, nDumpMethods, nInitMappingTableMethods)

    maxFiles = out_cpps.length

    if (! suppressMonolithicCppFile) {
      // We're now going to write the entire contents out to a singe file.
      // Make sure it's really a new file.
      advanceCppFile()
      createCppFile("")
      writeCppFile(all_cpp.result)
    }
    out_cpps.foreach(_.close)
    
    all_cpp.clear()
    out_cpps.clear()

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
