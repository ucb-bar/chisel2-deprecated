// author: jonathan bachrach
package Chisel {


object Fill {
  def fillWidthOf(i: Int, n: Node) = { (m: Node) => m.inputs(i).width * n.maxNum }
  def apply (mod: Node, n: Node): Node = {
    val res = new Fill();
    res.init("", fillWidthOf(0, n), mod, n);
    res.n = n;
    res
  }
}
class Fill extends Node {
  var n: Node = null;
  override def toString: String = "FILL(" + inputs(0) + ", " + n + ")";
  override def emitDef: String = 
    "  assign " + emitTmp + " = {" + n.emitRef + "{" + inputs(0).emitRef + "}};\n";
  override def emitDefLoC: String = 
    if (n.isLit)
      "  " + emitTmp + " = " + inputs(0).emitRef + ".fill<" + width + "," + n.value + ">();\n";
    else
      "  " + emitTmp + " = " + inputs(0).emitRef + ".fill<" + width + ">(" + n.emitRef + ");\n";
}

}
