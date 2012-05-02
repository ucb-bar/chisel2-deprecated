// author: jonathan bachrach
package Chisel {

import Node._;
import Reg._;
import ChiselError._;

object Reg {
  def regWidth(w: Int) = {
    if(w <= 0)
      maxWidth _;
    else 
      fixWidth(w)
  }
  val noInit = Lit(0){Fix()};
  def apply[T <: Data](data: T, width: Int, resetVal: T)(gen: => T): T = {

    /*
    val reg = new Reg()

    // initialize
    if(resetVal != null){
      reg.isReset = true
      reg.init("", regWidth(width), d, resetVal.toNode)
    } else
      reg.init("", regWidth(width), d)

    // make output
    val output = gen.fromNode(reg).asInstanceOf[T]
    output.setIsCellIO
    output.comp = reg
    reg.nameHolder = output
    output.isRegOut = true
    output
    * */

    val d: Array[(String, IO)] = 
      if(data == null) 
        gen.flatten.map{case(x, y) => (x -> null)}
      else 
        data.flatten

    val res = gen.asOutput
    res.setIsCellIO

    if(resetVal != null) {
      for((((res_n, res_i), (data_n, data_i)), (rval_n, rval_i)) <- res.flatten zip d zip resetVal.flatten) {
        val w = rval_i.getWidth
        assert(w > 0, {println("Negative width to wire " + res_i)})
        val reg = new Reg()
        reg.init("", regWidth(w), data_i, rval_i)

        // make output
        reg.isReset = true
        res_i.inputs += reg
        res_i.comp = reg
      }
    } else {
      for(((res_n, res_i), (data_n, data_i)) <- res.flatten zip d) {
        val w = res_i.getWidth
        val reg = new Reg()
        reg.init("", regWidth(w), data_i)

        // make output
        res_i.inputs += reg
        res_i.comp = reg
      }
    }

    res
  }

  def apply[T <: Data](data: T): T = {
    Reg[T](data, -1, null.asInstanceOf[T]){data.clone}
  }

  def apply[T <: Data](data: T, resetVal: T): T = Reg[T](data, -1, resetVal){data.clone}

  def apply[T <: Data](width: Int = -1, resetVal: T): T = Reg[T](null.asInstanceOf[T], width, resetVal){resetVal.clone}

  def apply[T <: Data]()(gen: => T): T = Reg[T](null.asInstanceOf[T], gen.width, null.asInstanceOf[T])(gen)

}

class Reg extends Delay with proc{
  def updateVal = inputs(0);
  def resetVal  = inputs(1);
  def enableSignal = inputs(enableIndex);
  var enableIndex = 0;
  var hasResetSignal = false
  var isReset = false
  var isEnable = false;
  def isUpdate = !(updateVal == null);
  def update (x: Node) = { inputs(0) = x };
  var assigned = false;
  var enable = Bool(false);
  def procAssign(src: Node) = {
    if (assigned)
      ChiselErrors += ChiselError("reassignment to Reg", Thread.currentThread().getStackTrace);
    val cond = genCond();
    if (conds.length >= 1) {
      isEnable = Component.isEmittingComponents
      enable = enable || cond;
    }
    updates.enqueue((cond, src));
  }
  override def genMuxes(default: Node) = {
    if(isEnable){
      inputs += enable;
      enableIndex = inputs.length - 1;
    }
    super.genMuxes(default);
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
  override def assign(src: Node) = {
    if(assigned || inputs(0) != null) {
      ChiselErrors += ChiselError("reassignment to Reg", Thread.currentThread().getStackTrace);
    } else { 
      assigned = true; super.assign(src)
    }
  }
  override def emitInitC: String = {
    "  " + emitRef + " = random_initialization ? dat_t<" + width + ">::rand() : LIT<" + width + ">(0);\n"
  }
  override def emitRefC: String = 
    if(isHiC) emitRefV + "_shadow_out" else emitRefV
  override def emitRefV: String = if (name == "") "R" + emitIndex else name;
  override def emitDef: String = "";
  override def emitReg: String = {
    if(isEnable){
      if(isReset){
	"    if(reset) begin\n" + 
	"      " + emitRef + " <= " + resetVal.emitRef + ";\n" +
	"    end else if(" + enableSignal.emitRef + ") begin\n" + 
	"      " + emitRef + " <= " + updateVal.emitRef + ";\n" +
	"    end\n"
      } else {
	"    if(" + enableSignal.emitRef + ") begin\n" +
	"      " + emitRef + " <= " + updateVal.emitRef + ";\n" +
	"    end\n"
      }
    } else {
      "    " + emitRef + " <= " + 
      (if (isReset) "reset ? " + resetVal.emitRef + " : " else "" ) + 
      updateVal.emitRef + ";\n"
    }
  }
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";

  override def emitDefLoC: String = {
    val updateLogic = 
      (if (isReset) "mux<" + width + ">(" + inputs.last.emitRef + ", " + resetVal.emitRef + ", " else "") + 
    updateVal.emitRef + (if (isReset) ");\n" else ";\n");

    "  " + emitRef + "_shadow = " +  updateLogic;
    // "  " + emitRef + "_shadow_out = " + emitRef + ";\n";
    
  }
  override def emitDefHiC: String = {
    "  " + emitRef + " = " + emitRef + "_shadow;\n";
  }
  override def emitInitHiC: String = {
    "  dat_t<" + width + "> " + emitRef + "_shadow_out = " + emitRef + ";\n";
  }
  override def emitDecC: String = {
    "  dat_t<" + width + "> " + emitRef + ";\n" +
    "  dat_t<" + width + "> " + emitRef + "_shadow;\n";
  }
}

}
