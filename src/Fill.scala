// author: jonathan bachrach
package Chisel {

import Component._;
import Fill._;
import IOdir._;
import Lit._;

object Fill {
  def fillWidthOf(i: Int, n: Node) = { (m: Node) => (m.inputs(i).width * n.maxNum.toInt) }
  def apply(n: Int, mod: Bits): Bits = {
    val (bits_lit) = (mod.litOf);
    if (isFolding && bits_lit != null) {
      // println("FOLDING EXTRACT " + bits_lit.value + "(" + off_lit.value + ")");
      var res = bits_lit.value;
      val w   = mod.getWidth();
      for (i <- 0 until n) 
        res = (res << w)|bits_lit.value;
      Bits(res, n * w);
    } else {
      val fill = new Fill()

      // initialize
      val fillConst = UFix(n)
      fill.init("", fillWidthOf(0, fillConst), mod, fillConst)

      // make output
      val output = Bits(OUTPUT)
      output.setIsCellIO
      fill.nameHolder = output
      output assign fill
      output
    }
  }
  def apply(mod: Bits, n: Int): Bits = apply(n, mod)
}

object NodeFill {
  def apply(n: Int, mod: Node): Node = {
    val res = new Fill()
    res.init("", (m: Node) => {m.inputs(0).width * n}, mod, Literal(n))
    res
  }
  def apply(mod: Node, n: Int): Node = apply(n, mod)
}

class Fill extends Node {
  var n: Node = if(inputs.length >= 2) inputs(1) else null;
  override def toString: String = "FILL(" + inputs(0) + ", " + n + ")";
  override def emitDef: String = 
    "  assign " + emitTmp + " = {" + inputs(1).emitRef + "{" + inputs(0).emitRef + "}};\n";
  override def emitDefLoC: String = {
    if (inputs(1).isLit)
      "  " + emitTmp + " = " + inputs(0).emitRef + ".fill<" + width + "," + inputs(1).value + ">();\n";
    else
      "  " + emitTmp + " = " + inputs(0).emitRef + ".fill<" + width + ">(" + inputs(1).emitRef + ");\n";
}}

}
