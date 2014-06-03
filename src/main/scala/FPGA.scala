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

class FPGABackend extends VerilogBackend
{
  def isMultiWrite(m: Mem[_]) = m.writes.size > 1
  def writen(m: MemWrite) = if (isMultiWrite(m.mem)) m.mem.writes.indexOf(m) else 0
  def writeMap(m: Mem[_], exclude: Int = -1) = {
    if (isMultiWrite(m)) {
      (0 until m.writes.size).filterNot(_ == exclude).map(emitRef(m) + "_" + _)
    } else {
      Seq(emitRef(m))
    }
  }

  override def emitDec(node: Node): String = {
    node match {
      case m: Mem[_] =>
        assert(m.isInline)
        "  reg [" + (m.needWidth()-1) + ":0] " + writeMap(m).map(_ + " [" + (m.n-1) + ":0]").reduceLeft(_ + ", " + _) + ";\n"

      case _ =>
        super.emitDec(node)
    }
  }

  override def emitReg(node: Node): String = node match {
    case m: MemWrite => ""
    case _ => super.emitReg(node)
  }

  override def emitDef(node: Node): String = {
    node match {
      case m: MemRead =>
        "  assign " + emitTmp(node) + " = " + writeMap(m.mem).map(_ + "[" + emitRef(m.addr) + "]").reduceLeft(_ + " ^ " + _) + ";\n"

      case m: MemWrite =>
        // check if byte-wide write enable can be used
        def litOK(x: Node) = x.isLit && (0 until x.needWidth()).forall(i => x.litOf.value.testBit(i) == x.litOf.value.testBit(i/8*8))
        def extractOK(x: Node) = x.isInstanceOf[Extract] && x.inputs.length == 3 && x.inputs(2).isLit && x.inputs(2).litOf.value % 8 == 0 && x.inputs(1).isLit && (x.inputs(1).litOf.value + 1) % 8 == 0 && useByteMask(x.inputs(0))
        def catOK(x: Node) = x.isInstanceOf[Op] && x.asInstanceOf[Op].op == "##" && x.inputs.forall(i => useByteMask(i))
        def useByteMask(x: Node): Boolean = extractOK(x) || litOK(x) || catOK(x) || x.isInstanceOf[Bits] && x.inputs.length == 1 && useByteMask(x.inputs(0))

        val me = writen(m)
        val mw = isMultiWrite(m.mem)
        val meStr = emitRef(m.mem) + (if (mw) "_" + me else "")
        val i = "i" + emitTmp(m)
        val gotWidth = m.mem.needWidth()
        (if (mw) "  wire [" + (gotWidth - 1) + ":0] " + emitRef(m.mem) + "_w" + me + " = " + writeMap(m.mem, me).map(_ + "[" + emitRef(m.addr) + "]").reduceLeft(_ + " ^ " + _) + ";\n" else "") +
        (if (m.isMasked) {
          val bm = gotWidth % 8 == 0 && useByteMask(m.mask)
          val max = if (bm) gotWidth/8 else gotWidth
          val maskIdx = if(bm) i + "*8" else i
          val dataIdx = if (bm) i + "*8+7:" + i + "*8" else i
          "  generate\n" +
          "    genvar " + i + ";\n" +
          "    for (" + i + " = 0; " + i + " < " + max + "; " + i + " = " + i + " + 1) begin: f" + emitTmp(m) + "\n" +
          "      always @(posedge " + emitRef(m.clock) + ")\n" +
          "        if (" + emitRef(m.cond) + " && " + emitRef(m.mask) + "[" + maskIdx + "])\n" +
          "          " + meStr + "[" + emitRef(m.addr) + "][" + dataIdx + "] <= " + emitRef(m.data) + "[" + dataIdx + "]" + (if (mw) " ^ " + emitRef(m.mem) + "_w" + me + "[" + dataIdx + "]" else "") + ";\n" +
          "    end\n" +
          "  endgenerate\n"
        } else {
          "  always @(posedge " + emitRef(m.clock) + ")\n" +
          "    if (" + emitRef(m.cond) + ")\n" +
          "      " + meStr + "[" + emitRef(m.addr) + "] <= " + emitRef(m.data) + (if (mw) " ^ " + emitRef(m.mem) + "_w" + me else "") + ";\n"
        })

      case _ =>
        super.emitDef(node)
    }
  }
}
