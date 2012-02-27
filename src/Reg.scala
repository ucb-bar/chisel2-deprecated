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
    val reg = new Reg()
    val d = if(data == null) null else data.toNode

    // initialize
    if(resetVal != null){
      reg.isReset = true
      reg.init("", regWidth(w), d, resetVal.toNode)
    } else
      reg.init("", regWidth(w), d)

    // make output
    val output = gen.fromNode(reg).asInstanceOf[T]
    output.setIsCellIO
    output.comp = reg
    reg.nameHolder = output
    output.isRegOut = true
    output
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
      ChiselErrors += IllegalState("reassignment to Reg", 3);
    val cond = genCond();
    if (conds.length >= 1) {
      isEnable = true;
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
      ChiselErrors += IllegalState("reassignment to Reg", 3);
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

    "  " + emitRef + "_shadow = " +  updateLogic +
    "  " + emitRef + "_shadow_out = " + emitRef + ";\n";
    
  }
  override def emitDefHiC: String = {
    "  " + emitRef + " = " + emitRef + "_shadow;\n";
  }
  override def emitDecC: String = 
    "  dat_t<" + width + "> " + emitRef + ";\n" +
    "  dat_t<" + width + "> " + emitRef + "_shadow;\n" +
    "  dat_t<" + width + "> " + emitRef + "_shadow_out;\n";
}

}
