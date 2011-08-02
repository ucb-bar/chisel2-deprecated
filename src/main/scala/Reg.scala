// author: jonathan bachrach
package Chisel {

import Node._;
import Reg._;

object Reg {
  def regWidth(w: Int) = {
    if(w <= 0)
      maxWidth _;
    else 
      fixWidth(w)
  }
  val noInit = Lit(0);
  def apply[T <: dat_t: Manifest](d: T = null, width: Int = -1, rVal: T = null): T = {
    val data = if(d == null) if(rVal == null) Fab[T]() else rVal else d;
    val regCell = new RegCell[T](data, width, d != null, rVal != null);
    if(d != null) regCell.io.d <> d;
    if(rVal != null) regCell.io.rVal <> rVal;
    regCell.io.q
}

  def apply[T <: dat_t](d: T): T = {
    val regCell = new RegCell(d, -1, true, false);
    regCell.io.d <> d;
    regCell.io.q
  }
}


class RegCell[T <: dat_t](data: T, w: Int, hasInput: Boolean, isReset: Boolean) extends Cell {
  val io = new bundle_t(){val d = data.clone().asInput();
			  val rVal = data.clone().asInput();
			  val q = data.clone().asOutput();
		      }
  io.setIsCellIO;
  val primitiveNode = new Reg();
  val dInput: Node = if(hasInput) io.d.toBits else null;
  if(isReset)
    primitiveNode.init("", regWidth(w), dInput, io.rVal.toBits);
  else
    primitiveNode.init("", regWidth(w), dInput);
  val fb = io.q.fromBits(primitiveNode).asInstanceOf[T] 
  fb.setIsCellIO;
  fb ^^ io.q;
  io.q.comp = primitiveNode.asInstanceOf[Reg];
  io.q.isRegOut = true;
}

class Reg extends Delay with proc{
  def updateVal = inputs(0);
  def resetVal  = inputs(1);
  def isReset  = inputs.length == 2;
  def isUpdate = !(updateVal == null);
  def update (x: Node) = { inputs(0) = x };
  def <==(src: Node): this.type = {
    if (cond.length == 0)
      update(src);
    else if (!isUpdate) {
      var res = cond(0);
      for (i <- 1 until cond.length)
        res = cond(i) && res;
      // println(this.name + " <== " + res + " " + cond.length);
      // val res = cond.foldRight(Lit(1,1)){(a, b) => a&&b}
      update(Multiplex(res, src, this))
    } else {
      var res = cond(0);
      for (i <- 1 until cond.length)
        res = cond(i) && res;
      // println(this.name + " <== " + res + " " + cond.length);
      update(Multiplex(res, src, updateVal))
    }
    this
    // clauses += Pair(cond.head, src);
  }
  def nameOpt: String = if (name.length > 0) name else "REG"
  override def toString: String = {
    if (component == null) return "nullcompreg";
    if (component.isWalking.contains(this)) 
      nameOpt
    else {
      component.isWalking += this;
      var res = nameOpt + "(";
      if (isUpdate) res = res + " " + updateVal;
      if (isReset)  res = res + " " + resetVal;
      res += ")";
      component.isWalking -= this;
      res;
    }
  }
  override def emitRefV: String = if (name == "") "R" + emitIndex else name;
  override def emitDef: String = "";
  override def emitReg: String =
    "    " + emitRef + " <= " + 
    (if (isReset) "reset ? " + resetVal.emitRef + " : " else "" ) + 
    updateVal.emitRef + ";\n"
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";

  override def emitDefLoC: String = 
    "  " + emitRef + "_shadow = " + 
    (if (isReset) "mux<" + width + ">(reset, " + resetVal.emitRef + ", " else "") + 
    updateVal.emitRef + (if (isReset) ");\n" else ";\n");
  override def emitDefHiC: String =
    "  " + emitRef + " = " + emitRef + "_shadow;\n";
  override def emitDecC: String = 
    "  dat_t<" + width + "> " + emitRef + ";\n" +
    "  dat_t<" + width + "> " + emitRef + "_shadow;\n";
}

}
