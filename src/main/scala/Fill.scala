// author: jonathan bachrach
package Chisel {

import Fill._;
import IOdir._;

object Fill {
  def fillWidthOf(i: Int, n: Node) = { (m: Node) => m.inputs(i).width * n.maxNum }
  def apply[T <: dat_t](mod: T, n: Fix): Fix = {
    val fillcell = new FillCell(mod);
    fillcell.io.mod <> mod;
    fillcell.io.n <> n;
    fillcell.io.out
  }
  def apply(mod: Int, n: Int): Fix =
    Fill(Fix(mod), Fix(n));
}


class FillCell[T <: dat_t](data: T) extends Cell {
  val io = new bundle_t(){val mod = data.clone.asInput();
			  val n = Fix('input);
			  val out = Fix('output);
			}
  io.setIsCellIO;
  val primitiveNode = new Fill();
  primitiveNode.init("primitiveNode", Fill.fillWidthOf(0, io.n), io.mod.toNode, io.n);
  io.out := primitiveNode;
  primitiveNode.nameHolder = io.out;
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
