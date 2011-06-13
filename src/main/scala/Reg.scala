// author: jonathan bachrach
package Chisel {

import Node._;

object Reg {
  def regWidth(w: Int) = {
    if(w <= 0)
      maxWidth _;
    else 
      fixWidth(w)
  }
  val noInit = Lit(0);
  def apply(d: Node = null, name: String = "", width: Int = -1, reset: Node = noInit): Reg = {
    if(reset == noInit)
      new Reg().init(name, regWidth(width), d).asInstanceOf[Reg];
    else {
      new Reg().init(name, regWidth(width), d, reset).asInstanceOf[Reg];
    }
  } 
}
class Reg extends Delay {
  def updateVal = inputs(0);
  def resetVal  = inputs(1);
  def isReset  = inputs.length == 2;
  def isUpdate = !(updateVal == null);
  def update (x: Node) = { inputs(0) = x };
  /*
  def reset(init: Node): Reg = { 
    if (isReset)
      inputs(1) = init; 
    else 
      inputs += init;
    inferWidth = widthOf(1);
    this 
  }*/
  def <==(src: Node): Reg = {
    if (cond.length == 0)
      update(src);
    else if (!isUpdate) {
      var res = cond(0);
      for (i <- 1 until cond.length)
        res = cond(i) && res;
      // println(this.name + " <== " + res + " " + cond.length);
      // val res = cond.foldRight(Lit(1,1)){(a, b) => a&&b}
      update(Mux(res, src, this))
    } else {
      var res = cond(0);
      for (i <- 1 until cond.length)
        res = cond(i) && res;
      // println(this.name + " <== " + res + " " + cond.length);
      update(Mux(res, src, updateVal))
    }
    this
    // clauses += Pair(cond.head, src);
  }
  def nameOpt: String = if (name.length > 0) name else "REG"
  override def toString: String = {
    if (component == null) return "";
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
