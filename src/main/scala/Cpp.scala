/*
 Copyright (c) 2011 - 2016 The Regents of the University of
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
import scala.collection.mutable.{ArrayBuffer, HashSet, HashMap}
import java.io.{BufferedReader, InputStreamReader}

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
  import PartitionIslands._
  private var hasPrintfs = false
  protected[this] val unoptimizedFiles = HashSet[String]()
  protected[this] val onceOnlyFiles = HashSet[String]()
  protected[this] var maxFiles: Int = 0
  protected[this] val compileInitializationUnoptimized = Driver.compileInitializationUnoptimized
  // If we're dealing with multiple files for the purpose of separate
  //   optimization levels indicate we expect to compile multiple files.
  protected[this] val compileMultipleCppFiles = Driver.compileInitializationUnoptimized
  // Define shadow registers in the circuit object, instead of local registers in the clock hi methods.
  // This is required if we're generating paritioned combinatorial islands, or we're limiting the size of functions/methods.
  protected[this] val shadowRegisterInObject = Driver.shadowRegisterInObject || Driver.partitionIslands || Driver.lineLimitFunctions > 0
  // If we need to put shadow registers in the object, we also should put multi-word literals there as well.
  // Actually, in order for emitLoWordRef() to work for multi-word literals, we need to have them in the object and initialized once.
  protected[this] val multiwordLiteralInObject = shadowRegisterInObject || true
  protected[this] val multiwordLiterals = HashSet[Literal]()
  // Should we put unconnected inputs in the object?
  protected[this] val unconnectedInputsInObject = Driver.partitionIslands
  protected[this] val unconnectedInputs = HashSet[Node]()
  // Sets to manage allocation and generation of shadow registers
  protected[this] val regWritten = HashSet[Node]()
  protected[this] val needShadow = HashSet[Node]()
  protected[this] val allocatedShadow = HashSet[Node]()

  protected[this] var potentialShadowRegisters = 0
  protected[this] val allocateOnlyNeededShadowRegisters = Driver.allocateOnlyNeededShadowRegisters

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

  // Emit the address of a node's values.
  def emitValueAddress(node: Node): String = {
    "&" + emitLoWordRef(node)
  }

  // Manage a constant pool.
  protected[this] val coalesceConstants = multiwordLiteralInObject
  protected[this] val constantPool = HashMap[(String, Width), Literal]()

  protected[this] def emitValue(value: BigInt, w: Int = 0): String = {
    val hex = value.toString(16)
    "0x" + (if (hex.length > bpw/4*w) hex.slice(hex.length-bpw/4*(w + 1), hex.length-bpw/4*w) else 0) + "L"
  }

  // Emit the value of a literal, instead of a reference to it.
  protected[this] def emitLitVal(n: Node, w: Int): String = {
    assert(isLit(n), ChiselError.error("Internal Error: Trying to emitLitVal for non-literal %s".format(n)))
    val l = n.asInstanceOf[Literal]
    val lit = l.value
    val value = if (lit < 0) (BigInt(1) << l.needWidth()) + lit else lit
    emitValue(value, w)
  }

  protected[this] def wordMangle(x: Node, w: String): String = x match {
    case l: Literal =>
      if (words(x) == 1) {
        emitRef(x)
      } else {
        val node = // Are we maintaining a constant pool?
          if (!coalesceConstants) x
          else constantPool.getOrElseUpdate((l.name, l.width_), l)
        s"T${node.emitIndex}[${w}]"
      }
    case _ =>
      if (x.isInObject) s"${emitRef(x)}.values[${w}]"
      else if (words(x) == 1) emitRef(x)
      else s"${emitRef(x)}[${w}]"
  }

  protected[this] def wordMangle(x: Node, w: Int): String = {
    val nWords = words(x)
    // If we're asking for a word that doesn't exist, return 0.
    // FIXME: Shouldn't this generate an error?
    if (w >= nWords) {
      "0L"
    }  else x match {
      // If this is a single word literal, we just output its value.
      case l: Literal =>
        if (nWords == 1) {
          emitLitVal(l, w)
        } else {
          // Otherwise, we'd better have multiwordLiteralInObject so we can return a reference to the specific word.
          assert(multiwordLiteralInObject, ChiselError.error("Internal Error: multiword literal reference without object to refer to"))
          wordMangle(x, w.toString)
        }
      case _ =>
        wordMangle(x, w.toString)
    }
  }

  protected[this] def isLit(node: Node): Boolean =
    node.isLit || node.isInstanceOf[Bits] && node.inputs.length == 1 && isLit(node.inputs.head)

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
  protected[this] def nodeVars(node: Node): List[(String, String)] = {
    node match {
      case x: Binding => List()
      case l: Literal if isInObject(l) && words(l) > 1 =>
        // Are we maintaining a constant pool?
        if (coalesceConstants) {
          // Is this the node that actually defines the constant?
          // If so, output the definition (we must do this only once).
          if (constantPool.contains((l.name, l.width_)) && constantPool((l.name, l.width_)) == l) {
            List((s"  static const val_t", s"T${l.emitIndex}[${words(l)}]"))
          } else {
            List()
          }
        } else {
          List((s"  static const val_t", s"T${l.emitIndex}[${words(l)}]"))
        }
      case l: Literal => List()
      case x: Reg => List((s"dat_t<${node.needWidth()}>", emitRef(node))) ++ {
        if ((!allocateOnlyNeededShadowRegisters || needShadow.contains(node)) && shadowRegisterInObject) {
          // Add an entry for the shadow register in the main object.
          List((s" dat_t<${node.needWidth()}>", emitRef(node) + s"__shadow"))
        } else {
          Nil
        }
      }
      case m: Mem[_] =>
        List((s"mem_t<${m.needWidth()},${m.n}>", emitRef(m)))
      case r: ROMData =>
        List((s"mem_t<${r.needWidth()},${r.n}>", emitRef(r)))
      case c: Clock =>
        List(("clk_t", emitRef(c)))
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

  protected[this] def emitCircuitAssign(srcPrefix:String, node: Node): String = {
    val out = new StringBuilder("")
    for (varDef <- nodeVars(node)) {
      out.append(s"  ${varDef._2} = ${srcPrefix}${varDef._2};\n")
    }
    out.toString()
  }

  protected[this] val bpw = 64
  protected[this] def words(node: Node): Int = (node.needWidth() - 1) / bpw + 1
  protected[this] def fullWords(node: Node): Int = node.needWidth()/bpw
  def emitLoWordRef(node: Node): String = emitWordRef(node, 0)
  // If we're generating multiple methods, literals and temporaries have to live in the main object.
  // We only get to ask isInObject once, so we'd better arrange for it to give the correct answer before we ask.
  def emitTmpDec(node: Node): String = {
    if (node.isInObject) {
      ""
    } else if (words(node) == 1) {
      s"  val_t ${emitRef(node)};\n"
    } else {
      s"  val_t ${emitRef(node)}[${words(node)}];\n"
    }
  }
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
    else s"  ${emitWordRef(x, words(x)-1)} = ${emitWordRef(x, words(x)-1)} & ${emitValue((BigInt(1) << (gotWidth%bpw))-1)};\n"
  }
  def opFoldLeft(o: Op, initial: (String, String) => String, subsequent: (String, String, String) => String) =
    (1 until words(o.inputs(0))).foldLeft(initial(emitLoWordRef(o.inputs(0)), emitLoWordRef(o.inputs(1))))((c, i) => subsequent(c, emitWordRef(o.inputs(0), i), emitWordRef(o.inputs(1), i)))

  def emitLog2(x: Node, priEnc: Boolean = false) = {
    val (func, range) =
      if (priEnc) ("priority_encode_1", (0 until words(x.inputs(0))))
      else ("log2_1", (words(x.inputs(0))-1 to 0 by -1))
    val body = range.map(i => s"${emitWordRef(x.inputs(0), i)} != 0, ${i*bpw} + ${func}(${emitWordRef(x.inputs(0), i)})")
                    .foldRight("0")((x, y) => s"TERNARY(${x}, ${y})")
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
            for (i <- log2Up(scala.math.min(bpw, o.inputs(0).needWidth()))-1 to 0 by -1)
              res += "__x = (__x >> " + (1L << i) + ") ^ __x"
            res += emitLoWordRef(o) + " = __x & 1"
            block(res)
          } else if (o.op == "~") {
            block((0 until words(o)).map(i => emitWordRef(o, i) + " = ~" + emitWordRef(o.inputs(0), i))) + trunc(o)
          } else if (o.op == "f-")
            "  " + emitLoWordRef(o) + " = fromFloat(-(toFloat(" + emitLoWordRef(o.inputs(0)) + ")));\n"
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
            assert(false, ChiselError.error("operator " + o.op + " unsupported"))
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
            val shb = emitLoWordRef(o.inputs(1))
            val res = ArrayBuffer[String]()
            res += s"val_t __c = 0"
            res += s"val_t __w = ${shb} / ${bpw}"
            res += s"val_t __s = ${shb} % ${bpw}"
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
          // Is this a single word shift?
          if (gotWidth <= bpw) {
            if (arith) {
              s"  ${emitLoWordRef(o)} = sval_t(${emitLoWordRef(o.inputs(0))} << ${bpw - gotWidth}) >> (${bpw - gotWidth} + ${emitLoWordRef(o.inputs(1))});\n" + trunc(o)
            } else {
              s"  ${emitLoWordRef(o)} = ${emitLoWordRef(o.inputs(0))} >> ${emitLoWordRef(o.inputs(1))};\n"
            }
          } else {
            val res = ArrayBuffer[String]()
            val nWords = ((gotWidth - 1) / bpw) + 1
            /* Use int and assume rsh < 2^32, define amount_shift as the amount to right shift */
            res += s"unsigned int __amount = ${emitLoWordRef(o.inputs(1))}"
            /* Define in_words as number of 64 bit words for the input */
            res += s"const unsigned int __in_words = ${nWords}"
            /* Define in_width as the bit width of the input */
            res += s"int __in_width = ${gotWidth}"
            res += s"val_t __d0[${nWords}]"
            val srcRef  = s"&${emitLoWordRef(o.inputs(0))}"
            // Call Rshift
            if (arith) {
              res += s"rsha_n(__d0, ${srcRef}, __amount, __in_words, __in_width)"
            } else {
              res += s"rsh_n(__d0, ${srcRef}, __amount, __in_words)"
            }
            /* Attach the result from __d0 to o */
            for ( i <- 0 until words(o) ) {
              res += s"${emitWordRef(o, i)} = __d0[${i}]"
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
        } else if (o.op == "f<") {
            "  " + emitLoWordRef(o) + " = toFloat(" + emitLoWordRef(o.inputs(0)) + ") < toFloat(" + emitLoWordRef(o.inputs(1)) + ");\n"
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
        } else if (o.op == "d<") {
            "  " + emitLoWordRef(o) + " = toDouble(" + emitLoWordRef(o.inputs(0)) + ") < toDouble(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else if (o.op == "d<=") {
            "  " + emitLoWordRef(o) + " = toDouble(" + emitLoWordRef(o.inputs(0)) + ") <= toDouble(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else if (o.op == "d>=") {
            "  " + emitLoWordRef(o) + " = toDouble(" + emitLoWordRef(o.inputs(0)) + ") >= toDouble(" + emitLoWordRef(o.inputs(1)) + ");\n"
        } else {
          assert(false, ChiselError.error("operator " + o.op + " unsupported"))
          ""
        })
      }

      case x: Extract =>
        x.inputs.tail.foreach(e => x.validateIndex(e))
        val source = node.inputs(0)
        val hi = node.inputs(1)
        val lo = if (node.inputs.length < 3) {
          hi
        } else {
          node.inputs(2)
        }
        // Is this a no-op - (i.e., all the source bits are extracted)?
        if (x.isNop) {
          emitTmpDec(node) + {
            // A straight assignment.
            block((0 until words(node)).map(i => emitWordRef(node, i) + " = " + emitWordRef(source, i)))
          }
        } else if (x.isOneBit) {
          emitTmpDec(node) + {
            // Ensure all the other bits are zero.
            if (words(node) > 1) {
              if (node.isInObject) {
                emitRef(node) + " = 0;\n"
              } else {
                "  memset(" + emitValueAddress(node) + ", 0, sizeof(" + emitRef(node) + "));\n"
              }
            } else {
              ""
            }
          } + {
            if (hi.isLit) {
              val value = hi.litValue().toInt
                  "  " + emitLoWordRef(node) + " = (" + emitWordRef(source, value/bpw) + " >> " + (value%bpw) + ") & 1;\n"
            } else if (source.needWidth() <= bpw) {
              "  " + emitLoWordRef(node) + " = (" + emitLoWordRef(source) + " >> " + emitLoWordRef(hi) + ") & 1;\n"
            } else {
              val inputWord = wordMangle(source, emitLoWordRef(hi) + "/" + bpw)
                  s"${emitLoWordRef(node)} = ${inputWord} >> (${emitLoWordRef(hi)} % ${bpw}) & 1;\n"
            }
          }
        } else if (x.isStaticWidth) {
            emitTmpDec(node) + {
              val rsh = lo.litValue().toInt
              if (rsh % bpw == 0) {
                block((0 until words(node)).map(i => emitWordRef(node, i) + " = " + emitWordRef(source, i + rsh/bpw))) + trunc(node)
              } else {
                block((0 until words(node)).map(i => emitWordRef(node, i)
                    + " = " + emitWordRef(source, i + rsh/bpw) + " >> "
                    + (rsh % bpw) + (
                        if (i + rsh/bpw + 1 < words(source)) {
                          " | " + emitWordRef(source, i + rsh/bpw + 1) + " << " + (bpw - rsh % bpw)
                        } else {
                          ""
                        }))) + trunc(node)
              }
            }
        } else {
          // Use the low level extract code.
          val nw = words(node)
          emitTmpDec(node) + ";\n  " + "  bit_word_funs<" + nw + ">::extract(" + emitValueAddress(node) + ", " + emitValueAddress(source) + ", " + emitRef(hi) + ", " + emitRef(lo) + ");\n"
        }

      case x: Clock => ""

      case x: Bits if x.isInObject && x.inputs.length == 1 => {
        emitTmpDec(x) + block((0 until words(x)).map(i => emitWordRef(x, i)
          + " = " + emitWordRef(x.inputs(0), i)))
      }
      case x: Bits if x.inputs.length == 0 && !(x.isTopLevelIO && x.dir == INPUT) =>
        emitTmpDec(x) + block("val_t __r = this->__rand_val()" +:
          (0 until words(x)).map(i => s"${emitWordRef(x, i)} = __r")) + trunc(x)

      case m: MemRead =>
        emitTmpDec(m) + block((0 until words(m)).map(i => emitWordRef(m, i)
          + " = " + emitRef(m.mem) + ".get(" + emitLoWordRef(m.addr) + ", "
          + i + ")"))

      case r: ROMRead =>
        emitTmpDec(r) + block((0 until words(r)).map(i => emitWordRef(r, i)
          + " = " + emitRef(r.rom) + ".get(" + emitLoWordRef(r.addr) + ", "
          + i + ")"))

      case a: Assert =>
        val cond =
          if (emitRef(a.cond) == "reset" || emitRef(a.cond) == Driver.implicitReset.name) emitLoWordRef(a.cond)
          else s"${emitLoWordRef(a.cond)} || !assert_fire || ${Driver.implicitReset.name}.lo_word()"
        if (!Driver.isAssert) ""
        else if (Driver.isAssertWarn) s"  WARN(${cond}, ${CString(a.message)});\n"
        else s"  ASSERT(${cond}, ${CString(a.message)});\n"

      case s: Sprintf =>
        ("#if __cplusplus >= 201103L\n"
          + "  " + emitRef(s) + " = dat_format<" + s.needWidth() + ">("
          + s.args.map(emitRef _).foldLeft(CString(s.format))(_ + ", " + _)
          + ");\n"
          + "#endif\n")

      case l: Literal if !l.isInObject && words(l) > 1 =>
        // Have we already allocated this literal in the main class definition?
        s"  val_t T${l.emitIndex}[] = {" + (0 until words(l)).map(emitWordRef(l, _)).reduce(_ + ", " + _) + "};\n"

      case _ =>
        ""
    }
  }

  def emitRefHi(node: Node): String = {
    node match {
      case reg: Reg => {
        val next = reg.next
        // If the input to this register is a register and we're not allocating only needed
        // shadow registers, assume we need the shadow register copy.
        // Otherwise, if we're allocating only needed shadow registers and this register
        // needs a shadow, now is the time to use that shadow.
	val useShadow = if (allocateOnlyNeededShadowRegisters) {
          needShadow.contains(reg)
        } else {
          next match { case _: Delay => true case _ => false }
        }
        if (useShadow) {
          emitRef(reg) + "__shadow"
        } else {
          emitRef(reg.next)
        }
      }
      case _ => emitRef(node)
    }
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
      case x: Clock => List(
        x.srcClock match {
          case None =>
            s"  ${emitRef(node)}.len = ${x.period.round};\n"
          case Some(src) =>
            s"  ${emitRef(node)}.len = ${emitRef(src)}.len ${
                if (src.period > x.period)
                "/ " + (src.period / x.period).round else
                "* " + (x.period / src.period).round
              };\n"
        },
        s"  ${emitRef(node)}.cnt = 0;\n",
        s"  ${emitRef(node)}.values[0] = 0;\n") mkString ""
      case x: Reg =>
        s"  ${emitRef(node)}.randomize(&__rand_seed);\n"

      case x: Mem[_] =>
        s"  ${emitRef(node)}.randomize(&__rand_seed);\n"

      case r: ROMData =>
        val BIZero = BigInt(0)
        val res = new StringBuilder
        val sparse = !isPow2(r.n) || r.n != r.sparseLits.size
        if (sparse)
          res append s"  ${emitRef(r)}.randomize(&__rand_seed);\n"
        for ((i, v) <- r.sparseLits) {
          val w = v.value
          if (sparse || w != BIZero)
            res append block((0 until words(r)).map(j => emitRef(r) + ".put(" + i + ", " + j + ", " + emitWordRef(v, j) + ")"))
        }
        res.toString

      case u: Bits if u.driveRand && u.isInObject =>
          s"   ${emitRef(node)}.randomize(&__rand_seed);\n"

      case _ =>
        ""
    }
  }

  def emitInitHi(node: Node): String = {
    node match {
      case reg: Reg => {
        potentialShadowRegisters += 1
        val allocateShadow = !allocateOnlyNeededShadowRegisters || needShadow.contains(reg)
        // If this isn't a shadow register, we've determined we don't need a shadow register
        // for this register, so we don't need to initialize it.
        if (allocateShadow) {
          allocatedShadow += reg
          val storagePrefix = if (shadowRegisterInObject) {
            ""
          } else {
            " dat_t<" + node.needWidth()  + ">"
          }
          s" ${storagePrefix} ${emitRef(reg)}__shadow = ${emitRef(reg.next)};\n"
        } else {
          ""
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

  // If we write a register node before we use its inputs, we need to shadow it.
  protected[this] def determineRequiredShadowRegisters(node: Node) {
    node match {
      case reg: Reg => {
        regWritten += node
        reg.next match {
          case _: Delay => needShadow += node
          case _ =>
        }
      }
      case _ =>
    }
    for (n <- node.inputs if regWritten.contains(n)) {
      needShadow += n
    }
  }

  protected[this] def clkName (clock: Clock): String =
    (if (clock == Driver.implicitClock) "" else "_" + emitRef(clock))

  protected[this] def genHarness(c: Module, name: String) {
    val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName)
    val harness  = createOutputFile(n + "-emulator.cpp")
    harness.write(s"""#include "${n}.h"\n\n""")
    if (Driver.clocks.length > 1) {
      harness.write(s"void ${c.name}_t::setClocks ( std::vector< int > &periods ) {\n")
      (Driver.clocks filter (_.srcClock == None)).zipWithIndex foreach {case (clk, i) =>
        harness.write(s"  ${emitRef(clk)}.len = periods[${i}];\n")
        harness.write(s"  ${emitRef(clk)}.cnt = periods[${i}];\n")
      }
      harness.write("}\n\n")
    }
    harness.write(s"""int main (int argc, char* argv[]) {\n""")
    harness.write(s"""  ${name}_t module;\n""")
    harness.write(s"""  ${name}_api_t api(&module);\n""")
    harness.write(s"""  module.init();\n""")
    harness.write(s"""  api.init_sim_data();\n""")
    if (Driver.isVCD) {
      harness.write(s"""  FILE *f = fopen("${Driver.targetDir}/${name}.vcd", "w");\n""")
    } else {
      harness.write(s"""  FILE *f = NULL;\n""")
    }
    harness.write(s"""  module.set_dumpfile(f);\n""")
    Driver.clocks foreach {clk =>
      harness write s"  module.${emitRef(clk)}.cnt = module.${emitRef(clk)}.len;\n"}
    harness.write(s"""  while(!api.exit()) api.tick();\n""")
    harness.write(s"""  if (f) fclose(f);\n""")
    harness.write(s"""}\n""")
    harness.close()
  }

  override def compile(c: Module, flagsIn: Option[String]) {
    val c11 = if (hasPrintfs) " -std=c++11 " else ""
    val cxxFlags = (flagsIn getOrElse CXXFLAGS) + c11
    val cppFlags = CPPFLAGS + " -I../ -I" + chiselENV + "/csrc/"
    val allFlags = List(cppFlags, cxxFlags).mkString(" ")
    val dir = Driver.targetDir + "/"
    val parallelMakeJobs = Driver.parallelMakeJobs

    def make(args: String) {
      // We explicitly unset CPPFLAGS and CXXFLAGS so the values
      // set in the Makefile will take effect.
      val cmd = "unset CPPFLAGS CXXFLAGS; make " + args
      if (!run(cmd)) throwException("make failed...")
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

    val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName)
    // Ensure onceOnlyFiles count as unoptimized.
    unoptimizedFiles ++= onceOnlyFiles
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
          basename = n + "-" + f
          if !unoptimizedFiles.contains(basename)
        } yield basename + ".o"
        ) mkString " ") + " " + n + "-emulator.o"

        replacements += (("@HFILES@", ""))
        replacements += (("@ONCEONLY@", onceOnlyOFiles))
        replacements += (("@UNOPTIMIZED@", unoptimizedOFiles))
        replacements += (("@OPTIMIZED@", optimzedOFiles))
        replacements += (("@EXEC@", n))
        replacements += (("@CPPFLAGS@", cppFlags))
        replacements += (("@CXXFLAGS@", cxxFlags))
        replacements += (("@LDFLAGS@", LDFLAGS))
        replacements += (("@OPTIM0@", optim0))
        replacements += (("@OPTIM1@", optim1))
        replacements += (("@OPTIM2@", optim2))
        replacements += (("@CXX@", CXX))
        // Read and edit the Makefile template.
        editToTarget("Makefile", replacements)
        val nJobs = if (parallelMakeJobs > 0) "-j" + parallelMakeJobs.toString() else "-j"
        make("-C " + Driver.targetDir + " " + nJobs)
      } else {
        // No make. Compile everything discretely.
        cc(dir, n + "-emulator", allFlags)
        // We should have unoptimized files.
        assert(unoptimizedFiles.size != 0 || onceOnlyFiles.size != 0,
          ChiselError.error("Internal Error: no unoptmized files to compile for '--compileMultipleCppFiles'"))
        // Compile the O0 files.
        onceOnlyFiles.map(cc(dir, _, allFlags + " " + optim0))

        // Compile the remaining (O1) files.
        unoptimizedFiles.filter( ! onceOnlyFiles.contains(_) ).map(cc(dir, _, allFlags + " " + optim1))
        val objects: ArrayBuffer[String] = new ArrayBuffer(maxFiles)
        // Compile the remainder at the specified optimization level.
        for (f <- 0 until maxFiles) {
          val basename = n + "-" + f
          // If we've already compiled this file, don't do it again,
          // but do add it to the list of objects to be linked.
          if (!unoptimizedFiles.contains(basename)) {
            cc(dir, basename, allFlags + " " + optim2)
          }
          objects += basename + ".o"
        }
        objects += (n + "-emulator.o")
        link(dir, n, objects)
      }
    } else {
      cc(dir, n + "-emulator", allFlags)
      cc(dir, n, allFlags)
      link(dir, n, List(n, n + "-emulator") map (_ + ".o"))
    }
  }

  override def elaborate(c: Module): Unit = {
    val minimumLinesPerFile = Driver.minimumLinesPerFile
    val partitionIslands = Driver.partitionIslands
    val lineLimitFunctions = Driver.lineLimitFunctions

    val clockPrototypes = ArrayBuffer[String]()

    // Generate CPP files
    val out_cpps = ArrayBuffer[CppFile]()

    def cppFileSuffix: String = if (compileMultipleCppFiles) {
        "-" + out_cpps.length
      } else {
        ""
      }

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
    }

    // Define some classes to help us deal with C++ methods.
    type CType = String
    case class CTypedName(ctype: CType, name: String)
    case class CMethod(name: CTypedName, arguments: Array[CTypedName] = Array[CTypedName](), cclass: String = c.name + "_t") {
      val body = new StringBuilder
      val argumentList = arguments.map(a => "%s %s".format(a.ctype, a.name)).mkString(", ")
      val callArgs = arguments.map(_.name).mkString(", ")
      val head = "%s %s::%s ( %s ) {\n".format(name.ctype, cclass, name.name, argumentList)
      val tail = "}\n"
      val genCall = "%s(%s);\n".format(name.name, callArgs)
      val prototype = "%s %s( %s );\n".format(name.ctype, name.name, argumentList)
    }

    // Split a large method up into a series of calls to smaller methods.
    // We assume the following:
    //  - all state is maintained in the class object (there is no local state
    //    manipulated by individual instructions).
    // The methodCodePrefix and methodCodeSuffix are lines to be emitted once,
    // in the top level method.
    class LineLimitedMethod(method: CMethod, methodCodePrefix: String = "", methodCodeSuffix: String = "", subCallArgs: Array[CTypedName] = Array[CTypedName]()) {
      var bodyLines = 0
      val body = new StringBuilder
      val bodies = new scala.collection.mutable.Queue[String]
      val maxLines = lineLimitFunctions
      val callArgs = subCallArgs.map(_.name).mkString(", ")
      val subArgList = subCallArgs.map(a => "%s %s".format(a.ctype, a.name)).mkString(", ")
      private def methodName(i: Int): String = {
        method.name.name + "_" + i.toString
      }
      private def newBody() {
        if (body.length > 0) {
          bodies += body.result
          body.clear()
        }
        bodyLines = 0
      }

      def addString(s: String) {
        if (s != "") {
          val lines = s.count(_ == '\n')
          if (maxLines > 0 && lines + bodyLines > maxLines) {
            newBody()
          }
          body.append(s)
          bodyLines += lines
        }
      }
      def genCalls() {
        var offset = 0
        while (bodies.length > offset) {
          val methodCall = methodName(offset) + "(" + callArgs + ");\n"
          addString(methodCall)
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
          case 0 => bodycalls.append(method.head + methodCodePrefix + methodCodeSuffix + method.tail)
          case 1 => {
            bodycalls.append(method.head + methodCodePrefix)
            bodycalls.append(bodies.dequeue())
            bodycalls.append(methodCodeSuffix + method.tail)
          }
          case _ => {
            for (i <- 0 until bodies.length - 1) {
              // If we want the submethods to return something other than "void",
              // we'll need to generate code to deal with that.
              bodycalls.append("void %s::%s(%s) {\n".format(method.cclass, methodName(i), subArgList))
              bodycalls.append(bodies.dequeue())
              bodycalls.append("}\n")
            }
            bodycalls.append(method.head + methodCodePrefix)
            bodycalls.append(bodies.dequeue())
            bodycalls.append(methodCodeSuffix + method.tail)
          }
        }
        bodycalls.result
      }
    }

    // This is the opposite of LineLimitedMethods.
    // It collects output until a threshold is reached.
    class CoalesceMethods(limit: Int) {
      var accumlation = 0
      var accumlatedMethodHead = ""
      var accumlatedMethodTail = ""
      val separateMethods = ArrayBuffer[CMethod]()

      def append(methodDefinition: CMethod) {
        val methodBody = methodDefinition.body.toString
        val nLinesApprox = methodBody.count(_ == '\n')

        def newMethod() {
          accumlation = 0
          accumlatedMethodHead = methodDefinition.head
          accumlatedMethodTail = methodDefinition.tail
          separateMethods.append(methodDefinition)
          createCppFile()
          writeCppFile(accumlatedMethodHead)
        }

        // Are we currently accumulating a method?
        if (accumlatedMethodHead == "") {
          // We are now.
          newMethod()
        }
        // Can we just merge this method in with the previous one?
        if (accumlation + nLinesApprox > limit) {
          // No. Time for a new method.
          // First, close off any accumulated method ..
          if (accumlation > 0) {
            writeCppFile(accumlatedMethodTail)
            // ... and start a new one.
            newMethod()
          }
        }
        writeCppFile(methodBody)
        accumlation += nLinesApprox
      }

      def done() {
        // First, close off any accumulated method.
        if (accumlation > 0) {
          writeCppFile(accumlatedMethodTail)
        }
      }
    }

    def createCppFile(suffix: String = cppFileSuffix) {
      // If we're trying to coalesce cpp files (minimumLinesPerFile > 0),
      //  don't actually create a new file unless the current file has been closed or we've hit the line limit.
      if ((out_cpps.size > 0) && (!compileMultipleCppFiles || (!out_cpps.last.done && out_cpps.last.lines < minimumLinesPerFile)) ) {
        out_cpps.last.write("\n\n")
      } else {
        // If the current file hasn't been closed, do so now.
        if (out_cpps.size > 0 && !out_cpps.last.done) {
          out_cpps.last.close()
        }
        out_cpps += new CppFile(suffix)
        println("CppBackend: createCppFile " + out_cpps.last.name)
      }
    }
    def writeCppFile(s: String) {
      out_cpps.last.write(s)
    }

    // If we're generating multiple cpp files, now is a good time to advance to the next.
    def advanceCppFile() {
      if (compileMultipleCppFiles) {
        out_cpps.last.close()
      }
    }

    // Generate header file
    def genHeader(vcd: Backend, islands: Array[Island], nInitMethods: Int, nDumpInitMethods: Int, nDumpMethods: Int, nSimMethods: Int) {
      val n = Driver.appendString(Some(c.name),Driver.chiselConfigClassName)
      val out_h = createOutputFile(n + ".h")
      out_h.write("#ifndef __" + c.name + "__\n")
      out_h.write("#define __" + c.name + "__\n\n")
      out_h.write("#include \"emulator.h\"\n\n")

      // Generate module headers
      out_h.write("class " + c.name + "_t : public mod_t {\n")
      out_h.write(" private:\n")
      out_h.write("  val_t __rand_seed;\n")
      out_h.write("  void __srand(val_t seed) { __rand_seed = seed; }\n")
      out_h.write("  val_t __rand_val() { return ::__rand_val(&__rand_seed); }\n")
      out_h.write(" public:\n")

      def headerOrderFunc(a: Node, b: Node) = {
        // pack smaller objects at start of header for better locality
        val aMem = a.isInstanceOf[Mem[_]] || a.isInstanceOf[ROMData]
        val bMem = b.isInstanceOf[Mem[_]] || b.isInstanceOf[ROMData]
        aMem < bMem || aMem == bMem && a.needWidth() < b.needWidth()
      }
      // Header declarations should be unique, add a simple check
      for (m <- Driver.orderedNodes.filter(_.isInObject).sortWith(headerOrderFunc)) {
        assertUnique(emitDec(m), "redeclaration in header for nodes")
        out_h.write(emitDec(m))
      }
      for (m <- Driver.orderedNodes.filter(_.isInVCD).sortWith(headerOrderFunc)){
        assertUnique(vcd.emitDec(m), "redeclaration in header for vcd")
        out_h.write(vcd.emitDec(m))
      }
      for (clock <- Driver.clocks) {
        assertUnique(emitDec(clock), "redeclaration in header for clock")
        out_h.write(emitDec(clock))
      }
      if (Driver.isVCD) out_h.write("  dat_t<1> reset__prev;\n") // also records reset

      out_h.write("\n")

      // If we're generating multiple init methods, wrap them in private/public.
      if (nInitMethods > 1) {
        out_h.write(" private:\n")
        for (i <- 0 until nInitMethods - 1) {
          out_h.write("  void init_" + i + " ( );\n")
        }
        out_h.write(" public:\n")
      }
      out_h.write("  void init ( val_t rand_init = 0 );\n")

      // Do we have already generated clock prototypes?
      if (clockPrototypes.length > 0) {
        out_h.write(" private:\n");
        for (proto <- clockPrototypes) {
          out_h.write("  " + proto)
        }
        out_h.write(" public:\n")
      }

      for (clock <- Driver.clocks) {
        val clockNameStr = clkName(clock).toString()
        out_h.write(s"  void clock_lo${clockNameStr} ( dat_t<1> reset, bool assert_fire=true );\n")
        out_h.write(s"  void clock_hi${clockNameStr} ( dat_t<1> reset );\n")
      }
      out_h.write("  int clock ( dat_t<1> reset );\n")
      if (Driver.clocks.length > 1) {
        out_h.write("  void setClocks ( std::vector< int >& periods );\n")
      }

      // For backwards compatibility, output both stream and FILE-based code.
      out_h.write("  void print ( FILE* f );\n")
      out_h.write("  void print ( std::ostream& s );\n")

      // If we're generating multiple dump methods, wrap them in private/public.
      if (nDumpMethods > 1) {
        out_h.write(" private:\n")
        for (i <- 0 until nDumpMethods - 1) {
          out_h.write("  void dump_" + i + " ( FILE* f );\n")
        }
        out_h.write(" public:\n")
      }
      out_h.write("  void dump ( FILE* f, int t, dat_t<1> reset=LIT<1>(0) );\n")

      // If we're generating multiple dump_init methods, wrap them in private/public.
      if (nDumpInitMethods > 1) {
        out_h.write(" private:\n");
        for (i <- 0 until nDumpInitMethods - 1) {
          out_h.write("  void dump_init_" + i + " ( FILE* f );\n")
        }
        out_h.write(" public:\n")
      }
      out_h.write("  void dump_init ( FILE* f );\n")

      // All done with the class definition. Close it off.
      out_h.write("\n};\n\n")
      out_h.write(Params.toCxxStringParams)

      if (Driver.isGenHarness) {
        out_h.write("#include \"emul_api.h\"\n")
        out_h.write("class " + c.name + "_api_t : public emul_api_t {\n")
        out_h.write(" public:\n")
        out_h.write("  %s_api_t(mod_t* m) : emul_api_t(m) { }\n".format(c.name))
        if (nSimMethods > 1) {
          out_h.write(" private:\n")
          for (i <- 0 until nSimMethods - 1) {
            out_h.write("  void init_sim_data_" + i + "(" + c.name + "_t* mod );\n")
          }
          out_h.write(" public:\n")
        }
        out_h.write("  void init_sim_data();\n")
        out_h.write("};")
      }

      out_h.write("\n\n#endif\n");
      out_h.close();
    }

    def genInitMethod(): Int = {

      // If we're putting literals in the class as static const's,
      // generate the code to initialize them here.
      if (multiwordLiteralInObject) {
        // Emit code to assign static const literals.
        def emitConstAssignment(l: Literal): String = {
          s"const val_t ${c.name}_t::T${l.emitIndex}[] = {" + (0 until words(l)).map(emitLitVal(l, _)).reduce(_ + ", " + _) + "};\n"
        }
        var wroteAssignments = false
        // Get the literals from the constant pool (if we're using one) ...
        if (coalesceConstants) {
          for (((v,w),l) <- constantPool) {
            writeCppFile(emitConstAssignment(l))
            wroteAssignments = true
          }
        } else {
          // ... or get the literals from their collected usage.
          for (l <- multiwordLiterals) {
            writeCppFile(emitConstAssignment(l))
            wroteAssignments = true
          }
        }
        // Add an additional newline after the assignments.
        if (wroteAssignments) {
          writeCppFile("\n")
        }
      }
      val method = CMethod(CTypedName("void", "init"), Array[CTypedName](CTypedName("val_t", "rand_init")))
      val llm = new LineLimitedMethod(method, "  this->__srand(rand_init);\n")
      Driver.orderedNodes foreach (llm addString emitInit(_))
      Driver.clocks foreach (llm addString emitInit(_))
      llm.done()
      val nMethods = llm.bodies.length
      writeCppFile(llm.getBodies)
      nMethods
    }

    def genClockMethod() {
      writeCppFile("int " + c.name + "_t::clock ( dat_t<1> reset ) {\n")
      writeCppFile("  uint32_t min = ((uint32_t)1<<31)-1;\n")

      // Do we have unconnected inputs that need randomizing?
      for (x <- unconnectedInputs) {
          writeCppFile(block("val_t __r = this->__rand_val()" +:
            (0 until words(x)).map(i => s"${emitWordRef(x, i)} = __r")) + trunc(x))
      }

      for (clock <- Driver.clocks) {
        writeCppFile("  if (" + emitRef(clock) + ".cnt < min) min = " + emitRef(clock) + ".cnt;\n")
      }
      for (clock <- Driver.clocks) {
        writeCppFile("  " + emitRef(clock) + ".cnt-=min;\n")
      }
      for (clock <- Driver.clocks) {
        writeCppFile("  if (" + emitRef(clock) + ".cnt == 0) clock_lo" + clkName(clock) + "( reset );\n")
      }
      if (Driver.isVCD) writeCppFile("  mod_t::dump( reset );\n")
      for (clock <- Driver.clocks) {
        writeCppFile("  if (" + emitRef(clock) + ".cnt == 0) clock_hi" + clkName(clock) + "( reset );\n")
      }
      for (clock <- Driver.clocks) {
        writeCppFile("  if (" + emitRef(clock) + ".cnt == 0) " + emitRef(clock) + ".cnt = " +
                    emitRef(clock) + ".len;\n")
      }
      writeCppFile("  return min;\n")
      writeCppFile("}\n")
    }

    def genPrintMethod() {
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
      if (hasPrintfs) {
        writeCppFile("fflush(f);\n");
      }
      writeCppFile("}\n")

      writeCppFile("void " + c.name + "_t::print ( std::ostream& s ) {\n")
      for (cc <- Driver.components; p <- cc.printfs) {
        hasPrintfs = true
        writeCppFile("#if __cplusplus >= 201103L\n"
          + "  if (" + emitLoWordRef(p.cond)
          + ") dat_prints<" + p.needWidth() + ">(s, "
          + p.args.map(emitRef _).foldLeft(CString(p.format))(_ + ", " + _)
          + ");\n"
          + "#endif\n")
      }
      if (hasPrintfs) {
        writeCppFile("s.flush();\n");
      }
      writeCppFile("}\n")
    }

    def genDumpInitMethod(vcd: VcdBackend): Int = {
      val method = CMethod(CTypedName("void", "dump_init"), Array[CTypedName](CTypedName("FILE*", "f")))
      val llm = new LineLimitedMethod(method, "", "", Array[CTypedName](CTypedName("FILE*", "f")))
      vcd.dumpVCDInit(llm.addString)
      llm.done()
      val nMethods = llm.bodies.length
      writeCppFile(llm.getBodies)
      nMethods
    }

    def genDumpMethod(vcd: VcdBackend): Int = {
      val method = CMethod(CTypedName("void", "dump"),
        Array[CTypedName](CTypedName("FILE*", "f"), CTypedName("int", "t"), CTypedName("dat_t<1>", "reset")))
      // Are we actually generating VCD?
      if (Driver.isVCD) {
        // Yes. dump is a real method.
        val codePrefix = List(
          "  if (t == 0) return dump_init(f);\n",
          "  fprintf(f, \"#%d\\n\", t << 1);\n") mkString ""
        // Are we generating a large dump method with gotos? (i.e., not inline)
        if (Driver.isVCDinline) {
          val llm = new LineLimitedMethod(method, codePrefix, "", Array[CTypedName](CTypedName("FILE*", "f")))
          vcd.dumpVCD(llm.addString)
          llm.done()
          val nMethods = llm.bodies.length
          writeCppFile(llm.getBodies)
          nMethods
        } else {
          // We're creating a VCD dump method with gotos.
          writeCppFile(method.head + codePrefix)
          vcd.dumpVCD(writeCppFile)
          writeCppFile(method.tail)
          1
        }
      } else {
        // No. Just generate the dummy (nop) method.
        writeCppFile(method.head + method.tail)
        1
      }
    }

    def genInitSimDataMethod(c: Module) = {
      val method = CMethod(CTypedName("void", "init_sim_data"), Array[CTypedName](), c.name + "_api_t")
      val codePrefix = s"  sim_data.inputs.clear();\n" +
                 s"  sim_data.outputs.clear();\n" +
                 s"  sim_data.signals.clear();\n" +
                 s"  ${c.name}_t* mod = dynamic_cast<${c.name}_t*>(module);\n" +
                 s"  assert(mod);\n"
      val llm = new LineLimitedMethod(method, codePrefix, "", Array[CTypedName](CTypedName(s"${c.name}_t*", "mod")))
      val (inputs, outputs) = c.wires.unzip._2 partition (_.dir == INPUT)
      var id = 0
      Driver.orderedNodes.map {
        case m: Mem[_] =>
          Driver.signalMap(m) = id
          id += m.n
        case node if node.prune || node.driveRand =>
        case node if node.chiselName != "" && !node.isTopLevelIO && node.isInObject =>
          Driver.signalMap(node) = id
          id += 1
        case _ =>
      }
      llm addString (inputs map (in =>
        "  sim_data.inputs.push_back(new dat_api<%d>(&mod->%s));\n".format(in.needWidth, emitRef(in))) mkString "")
      llm addString (outputs map (out =>
        "  sim_data.outputs.push_back(new dat_api<%d>(&mod->%s));\n".format(out.needWidth, emitRef(out))) mkString "")
      llm addString (Driver.signalMap flatMap {
        case (mem: Mem[_], id) => List(
            "  std::string %s_path = \"%s\";\n".format(emitRef(mem), mem.chiselName),
            "  for (size_t i = 0 ; i < %d ; i++) {\n".format(mem.n),
            "    sim_data.signals.push_back(new dat_api<%d>(&mod->%s.contents[i]));\n".format(mem.needWidth, emitRef(mem)),
            "    sim_data.signal_map[%s_path+\"[\"+itos(i,false)+\"]\"] = %d+i;\n".format(emitRef(mem), id), "  }\n")
        case (node, id) => List(
          "  sim_data.signals.push_back(new dat_api<%d>(&mod->%s));\n".format(node.needWidth, emitRef(node)),
          "  sim_data.signal_map[\"%s\"] = %d;\n".format(node.chiselName, Driver.signalMap(node)))
      } mkString "")
      llm addString (Driver.clocks map { clk =>
        "  sim_data.clk_map[\"%s\"] = new clk_api(&mod->%s);\n".format(clk.name, clk.name)
      } mkString "")

      llm.done()
      val nMethods = llm.bodies.length
      writeCppFile(llm.getBodies)
      nMethods
    }

    println("CPP elaborate")
    super.elaborate(c)

    /* We flatten all signals in the toplevel component after we had
     a change to associate node and components correctly first
     otherwise we are bound for assertions popping up left and right
     in the Backend.elaborate method. */
    flattenAll // created Driver.orderedNodes
    ChiselError.checkpoint()

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
      type ClockCodeMethods = HashMap[Clock, (CMethod, CMethod, CMethod)]
      val code = new ClockCodeMethods
      val islandClkCode = new HashMap[Int, ClockCodeMethods]
      val islandStarted = Array.fill(3, maxIslandId + 1)(0)    // An array to keep track of the first time we added code to an island.
      val islandOrder = Array.fill(3, islands.size)(0)         // An array to keep track of the order in which we should output island code.
      var islandSequence = Array.fill(3)(0)
      val showProgress = false

      for (clock <- Driver.clocks) {
        // All clock methods take the same arguments and return void.
        val clockArgs = Array[CTypedName](CTypedName("dat_t<1>", Driver.implicitReset.name))
        val clockLoArgs = clockArgs :+ CTypedName("bool", "assert_fire")
        val clockLoName = "clock_lo" + clkName(clock)
        val clock_dlo = new CMethod(CTypedName("void", clockLoName), clockLoArgs)
        val clockHiName = "clock_hi" + clkName(clock)
        val clock_ihi = new CMethod(CTypedName("void", clockHiName), clockArgs)
        // For simplicity, we define a dummy method for the clock_hi exec code.
        // We won't actually call such a  method - its code will be inserted into the
        // clock_hi method after all the clock_hi initialization code.
        val clockHiDummyName = "clock_hi_dummy" + clkName(clock)
        val clock_xhi = new CMethod(CTypedName("void", clockHiDummyName), clockArgs)
        code += (clock -> ((clock_dlo, clock_ihi, clock_xhi)))
        // If we're generating islands of combinational logic,
        // have the main clock code call the island specific code,
        // and generate that island specific clock_(hi|lo) code.
        if (partitionIslands) {
          for (island <- islands) {
            val islandId = island.islandId
            // Do we need a new entry for this mapping?
            if (!islandClkCode.contains(islandId)) {
              islandClkCode += ((islandId, new ClockCodeMethods))
            }
            val clockLoName = "clock_lo" + clkName(clock) + "_I_" + islandId
            val clock_dlo_I = new CMethod(CTypedName("void", clockLoName), clockLoArgs)
            // Unlike the unpartitioned case, we will generate and call separate
            // initialize and execute clock_hi methods if we're partitioning.
            val clockIHiName = "clock_ihi" + clkName(clock) + "_I_" + islandId
            val clock_ihi_I = new CMethod(CTypedName("void", clockIHiName), clockArgs)
            val clockXHiName = "clock_xhi" + clkName(clock) + "_I_" + islandId
            val clock_xhi_I = new CMethod(CTypedName("void", clockXHiName), clockArgs)
            islandClkCode(islandId) += (clock -> ((clock_dlo_I, clock_ihi_I, clock_xhi_I)))
          }
        }
      }

      def clock(n: Node) = n.clock getOrElse Driver.implicitClock

      def populate() {
        var nodeCount = 0
        def isNodeInIsland(node: Node, island: Island): Boolean = {
          island == null || nodeToIslandArray(node._id).contains(island)
        }

        // Return tuple of booleans if we actually added any clock code.
        def addClkDefs(n: Node, codeMethods: ClockCodeMethods): (Boolean, Boolean, Boolean) = {
          val defLo = emitDefLo(n)
          val initHi = emitInitHi(n)
          val defHi = emitDefHi(n)
          val clockNode = clock(n)
          if (defLo != "" || initHi != "" || defHi != "") {
            codeMethods(clockNode)._1.body.append(defLo)
            codeMethods(clockNode)._2.body.append(initHi)
            codeMethods(clockNode)._3.body.append(defHi)
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
          // No. Generate and output single, monolithic methods.
          for (m <- Driver.orderedNodes) {
            addClkDefs(m, code)
          }

        } else {
          // We're generating partitioned islands
          val addedCode = new Array[Boolean](3)
          for (m <- Driver.orderedNodes) {
            for (island <- nodeToIslandArray(m._id)) {
              val islandId = island.islandId
              val codeMethods = islandClkCode(islandId)
              val addedCodeTuple = addClkDefs(m, codeMethods)
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
            nodeCount += 1
            if (showProgress && (nodeCount % 1000) == 0) {
              println("ClockDomains: populated " + nodeCount + " nodes.")
            }
          }
        }
      }

      def outputAllClkDomains() {
        // Are we generating partitioned islands?
        if (!partitionIslands) {
          //.values.map(_._1.body) ++ (code.values.map(x => (x._2.append(x._3))))
          for ((clock, clockMethods) <- code) {
            val clockLo = clockMethods._1
            val clockIHi = clockMethods._2
            val clockXHi = clockMethods._3
            createCppFile()
            writeCppFile(clockLo.head + clockLo.body.result + clockLo.tail)
            createCppFile()
            writeCppFile(clockIHi.head + clockIHi.body.result)
            // Note, we tacitly assume that the clock_hi initialization and execution
            // code have effectively the same signature and tail.
            assert(clockIHi.tail == clockXHi.tail, ChiselError.error("Internal Error: clockIHi.tail != clockXHi.tail"))
            writeCppFile(clockXHi.body.result + clockXHi.tail)
          }
        } else {
          // We allow for the consolidation of the islands.
          // Keeping them distinct causes the object to balloon in size,
          // requiring three methods for each island.

          val accumulatedClockLos = new CoalesceMethods(lineLimitFunctions)
          // Output the clock code in the correct order.
          for (islandId <- islandOrder(0) if islandId > 0) {
            for ((clock, clkcodes) <- islandClkCode(islandId)) {
              val clock_lo = clkcodes._1
              accumulatedClockLos.append(clock_lo)
              clock_lo.body.clear()      // free the memory
            }
          }
          accumulatedClockLos.done()

          // Now emit the calls on the accumulated methods from the main clock_lo method.
          for ((clock, clkcodes) <- code) {
            val clock_lo = clkcodes._1
            createCppFile()
            // This is just the definition of the main clock_lo method.
            writeCppFile(clock_lo.head)
            // Output the actual calls to the island specific clock_lo code.
            for (clockLoMethod <- accumulatedClockLos.separateMethods) {
              writeCppFile("\t" + clockLoMethod.genCall)
            }
            writeCppFile("}\n")
          }

          // Output the island-specific clock_hi init code
          val accumulatedClockHiIs = new CoalesceMethods(lineLimitFunctions)
          for (islandId <- islandOrder(1) if islandId > 0) {
            for (clockHiI <- islandClkCode(islandId).values.map(_._2)) {
              accumulatedClockHiIs.append(clockHiI)
              clockHiI.body.clear()         // free the memory.
            }
          }
          accumulatedClockHiIs.done()

          // Output the island-specific clock_hi def code
          val accumulatedClockHiXs = new CoalesceMethods(lineLimitFunctions)
          for (islandId <- islandOrder(2) if islandId > 0) {
            for (clockHiX <- islandClkCode(islandId).values.map(_._3)) {
              accumulatedClockHiXs.append(clockHiX)
              clockHiX.body.clear()         // free the memory.
            }
          }
          accumulatedClockHiXs.done()

          // Output the code to call the island-specific clock_hi (init and exec) code.
          for ((clock, clkcodes) <- code) {
            val clock_ihi = clkcodes._2
            val clock_xhi = clkcodes._3
            createCppFile()
            // This is just the definition of the main clock_hi init method.
            writeCppFile(clock_ihi.head)
            // Output the actual calls to the island specific clock code.
            for (method <- accumulatedClockHiIs.separateMethods) {
              writeCppFile("\t" + method.genCall)
            }
            for (method <- accumulatedClockHiXs.separateMethods) {
              writeCppFile("\t" + method.genCall)
            }
            writeCppFile(clock_xhi.tail)
          }

          // Put the accumulated method definitions where the header
          // generation code can find them.
          for( method <- accumulatedClockLos.separateMethods ++ accumulatedClockHiIs.separateMethods ++ accumulatedClockHiXs.separateMethods) {
            clockPrototypes += method.prototype
          }
        }
      }
    }

    if (Driver.isGenHarness) {
      genHarness(c, c.name)
      /* Copy the emulator headers into the targetDirectory. */
      copyToTarget("sim_api.h")
      copyToTarget("emul_api.h")
    }
    if (!Params.space.isEmpty) {
      val out_p = createOutputFile(c.name + ".p")
      out_p.write(Params.toDotpStringParams)
      out_p.close()
    }

    ChiselError.info("populating clock domains")
    val clkDomains = new ClockDomains
    clkDomains.populate()

    println("CppBackend::elaborate: need " + needShadow.size + ", redundant " + (potentialShadowRegisters - needShadow.size) + " shadow registers")

    // Shouldn't this be conditional on Driver.isVCD?
    // In any case, defer it until after we've generated the "real"
    // simulation code.
    val vcd = new VcdBackend(c)

    // Generate CPP files
    ChiselError.info("generating cpp files")

    // generate init block
    createCppFile()
    val nInitMethods = genInitMethod()

    // generate clock(...) method
    createCppFile()
    genClockMethod()

    // generate print(...) method.
    createCppFile()
    genPrintMethod()

    createCppFile()
    val nDumpInitMethods = genDumpInitMethod(vcd)

    createCppFile()
    val nDumpMethods = genDumpMethod(vcd)

    // If we're compiling initialization methods -O0 or -O1, add the current files
    //  to the unoptimized file list.
    //  We strip off the trailing ".cpp" to facilitate creating both ".cpp" and ".o" files.
    if (compileInitializationUnoptimized) {
      val trimLength = ".cpp".length()
      unoptimizedFiles ++= out_cpps.map(_.name.dropRight(trimLength))
    }
    // Ensure we start off in a new file before we start outputting the clock_lo/hi.
    advanceCppFile()
    createCppFile()
    clkDomains.outputAllClkDomains()

    // Generate API methods
    val nSimMethods = if (Driver.isGenHarness) {
      advanceCppFile()
      createCppFile()
      // If we're compiling initialization methods -O0, add the current file to the unoptimized file list.
      //  We strip off the trailing ".cpp" to facilitate creating both ".cpp" and ".o" files.
      if (compileInitializationUnoptimized) {
        val trimLength = ".cpp".length()
        onceOnlyFiles += out_cpps.last.name.dropRight(trimLength)
      }
      genInitSimDataMethod(c)
    } else {
      0
    }

    // Finally, generate the header - once we know how many methods we'll have.
    genHeader(vcd, islands, nInitMethods, nDumpInitMethods, nDumpMethods, nSimMethods)

    maxFiles = out_cpps.length

    out_cpps.foreach(_.close)

    out_cpps.clear()

    copyToTarget("emulator.h")
  }

  // Return true if we want this node to be included in the main object.
  // The Driver (and node itself) may also help determine this.
  override def isInObject(n: Node): Boolean = n match {
    // Should we put multiword literals in the object?
    case l: Literal if multiwordLiteralInObject && words(n) > 1 => {
      multiwordLiterals += l
      true
    }
    // Should we put disconnected inputs in the object (we will generated random values for them)
    case b: Bits if unconnectedInputsInObject && b.inputs.length == 0 => {
      unconnectedInputs += b
      true
    }
    case _ => false
  }
}
