// author: jonathan bachrach
package Chisel {

import Node._;
import Component._;
import IOdir._;
import ChiselError._;
import Terminator._;

object IOdir {
  val INPUT  = new IOdir(0)
  val OUTPUT = new IOdir(1)
}
class IOdir (idi: Int) {
  val id = idi
  override def toString: String = if (id == 0) "INPUT-DIR" else "OUTPUT-DIR";
}

trait IODirection {

}

object INPUT extends IODirection {

}

object OUTPUT extends IODirection {

}
class IO extends Wire with Cloneable{ 
  var dir: IODirection = null;
  var unnamed = false;
  
  // TODO: OPTIONALLY ONLY EMIT TOP COMPONENT IO IN OBJECT
  override def isIo = true; // = component == topComponent; // true; 
  override def emitDef: String = {
    if (dir == INPUT)
      ""
    else
      super.emitDef;
  }
  override def terminate(): Unit = { this assign terminator }
  override def emitDec: String = "";
  override def emitDecC: String = if (isEmittingComponents) ""; else super.emitDecC;
  // override def emitDef: String = "";
  override def apply(name: String): Data = this
  override def flatten = Array((name, this));
  override def emitDefLoC: String = {
    if (dir == INPUT) {
      // TODO: HACK
      if (inputs.length == 1)
        "  " + emitTmp + " = " + inputs(0).emitRef + ";\n"
      // else if (consumers.length == 1)
      //   "  " + consumers(0).emitRef + " = " + emitTmp + ";// CONSUMER=1 \n"
      else if (inputs.length == 0 && !isInObject) 
        "  " + emitTmp + ";\n"
      else
        ""
    } else 
      super.emitDefLoC
  }
  override def toString: String = {
    if (dir == INPUT)
      "INPUT(" + name + (if (component == null) "" else ("." + component)) + ")";
    else 
      "OUTPUT(" + name + (if (component == null) "" else ("." + component)) + ")";
  }
  override def flip(): this.type = {
    if (dir == INPUT) {
      dir = OUTPUT;
    } else {
      dir = INPUT
    }
    this
  }

  override def <>(src: Node) = { 
    if (dir == INPUT) {
      // println("<>'ing " + this + " <-- " + src);
      src match { 
      case other: IO => 
	if (other.dir == OUTPUT && this.staticComp == other.staticComp && !isCellIO) {
	  other assign this;
        } else if (isCellIO || (other.dir == OUTPUT && this.staticComp.parent == other.staticComp.parent)) {
          this assign other;
        } else if (other.dir == INPUT) {
	  if(this.staticComp == other.staticComp.parent)
	    other assign this;
	  else if(this.staticComp.parent == other.staticComp)
	    this assign other;
	  else
	    ChiselErrors += ChiselError({"Connecting Input " + this + " Input " + other}, Thread.currentThread().getStackTrace);
	} else {
          ChiselErrors += ChiselError({"Connecting Input " + this + " to " + other}, Thread.currentThread().getStackTrace);
        }
      case default => ChiselErrors += ChiselError({"Connecting Input " + this + " to IO without direction " + default}, Thread.currentThread().getStackTrace);
      }
    } else { // DIR == OUTPUT
      // println("<>'ing " + this + " --> " + src);i
      src match { 
        case other: IO  => 
	  if (other.dir == INPUT && this.staticComp == other.staticComp && !isCellIO){
	    this assign other;
	  } else if (isCellIO || (other.dir == INPUT && this.staticComp.parent == other.staticComp.parent)) {
            other assign this;
          } else if (other.dir == OUTPUT) {
	    if(this.staticComp == other.staticComp.parent)
	      this assign other;
	    else if (this.staticComp.parent == other.staticComp)
	      other assign this;
	    else if (this.isCellIO && other.isCellIO)
	      ChiselErrors += ChiselError("Ambiguous Connection of Two Nodes", Thread.currentThread().getStackTrace);
	    else if (this.isCellIO){
	      other assign this; }
	    else if (other.isCellIO){
	      this assign other; }
	    else
	      ChiselErrors += ChiselError({"Connecting Output " + this + " to Output " + other}, Thread.currentThread().getStackTrace);
	  } else {
            ChiselErrors += ChiselError({"Connecting Output " + this + " to IO without direction " + other}, Thread.currentThread().getStackTrace);
          }
        case default => 
          //println("Connecting Output " + this + " to Node " + default);
      }
    } 
  }  

  override def ^^(parent: Node) = { 
    if (dir == INPUT) {
      // println("^^ " + this + " COMP " + component + " & " + src + " SRC COMP " + src.component);
      parent match { 
        case other: IO => 
          if (other.dir == INPUT || isCellIO) {
            this assign other;
          } else 
            ChiselErrors += ChiselError({"^^ing Input " + this + " to Output " + other}, Thread.currentThread().getStackTrace);
        case default => 
          ChiselErrors += ChiselError({"// ^^ing Input " + this + " to Node " + default}, Thread.currentThread().getStackTrace);
      }
    } else { // dir == OUTPUT
      // println("^^ing " + this + " COMP " + component + " & " + src + " SRC COMP " + src.component + " CHILD? " + isChild);
      parent match { 
        case other: IO  => 
          if (other.dir == OUTPUT || isCellIO) {
            other assign this;
          } else
            ChiselErrors += ChiselError({"^^ing Output " + this + " to Input " + other}, Thread.currentThread().getStackTrace);
        case default => 
          ChiselErrors += ChiselError({"^^ing Output " + this + " to Node " + default}, Thread.currentThread().getStackTrace);
      } 
    }
  }
  override def assign(src: Node) = {
    if (!src.isTerminated) {
    if(assigned)
      ChiselErrors += ChiselError("reassignment to output", Thread.currentThread().getStackTrace);
    else { 
      assigned = true; 
      if (inputs.length > 0) inputs(0) = src; else inputs += src;
    }
    }
  }
  override def asInput(): this.type = {
    dir = INPUT;
    this
  }
  override def asOutput(): this.type = {
    dir = OUTPUT;
    this
  }
  override def emitRefV = if(name == "") super.emitRefV else if(!named) name + "_" + emitIndex else name
  override def setIsCellIO = isCellIO = true;
  override def setIsClkInput = {isClkInput = true; this assign clk;}
  override def clone = {
    val res = this.getClass.newInstance.asInstanceOf[this.type];
    //res.init("", widthOf(0), null);
    res.inferWidth = this.inferWidth
    res.width_ = this.width_;
    res.dir = this.dir;
    res.name = this.name;
    res
  }
  override def toNode = this;
  override def fromNode(src: Node) = {
    val res = new IO().asInstanceOf[this.type];
    res.init("", widthOf(0), src);
    res.dir = dir;
    res assign src;
    res
  }
  override def maxNum = {
    if(inputs.length == 0) 
      width;
    else if(inputs(0).isLit)
      inputs(0).value
    else if (inputs.length == 1 && isCellIO)
      inputs(0).maxNum
    else
      super.maxNum
  }
};


}
