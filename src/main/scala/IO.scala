// author: jonathan bachrach
package Chisel {

import Node._;
import Component._;
import IOdir._;
import ChiselError._;

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
/*
object Input {
  def apply(): IO = apply("", widthOf(0))
  def apply(width: Int): Fix = Fix(width, INPUT);
  def apply(name: String): IO = apply(name, widthOf(0))
  def apply(name: String, width: Int): Fix = 
    { val res = new Fix(); res.dir = INPUT; if(name != "") res.named = true; res.init(name, width); res }
  def apply(name: String, inferWidth: (Node) => Int): IO = 
    { val res = new IO(); res.dir = INPUT; res.init(name, inferWidth); res }
}
object Output {
  def apply(width: Int): Fix = Fix(width, OUTPUT);
  def apply(name: String): IO = apply(name, widthOf(0), null)
  def apply(name: String, inferWidth: (Node) => Int, x: Node): IO = 
    { val res = new IO(); res.dir = OUTPUT; res.init(name, inferWidth, x); res }
  def apply(name: String, width: Int): IO = 
    { val res = new IO(); res.dir = OUTPUT; res.init(name, width); res }
  def apply(name: String, x: Node): IO = apply(name, maxWidth _, x); 
  def apply(x: Node): IO = apply("", x);
}
* */
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
      else
        ""
    } else
      super.emitDefLoC
  }
  override def toString: String = {
    if (dir == INPUT)
      "INPUT(" + name + "." + component + ")";
    else 
      "OUTPUT(" + name + "." + component + ")";
  }
  override def flip(): this.type = {
    if (dir == INPUT) {
      dir = OUTPUT;
    } else {
      dir = INPUT
    }
    this
  }
  /*
  override def <>(src: Node) = { 
    if (dir == INPUT) {
      // println("<>'ing " + this + " <-- " + src);
      src match { 
      case other: IO => 
        if (other.dir == OUTPUT || isCellIO) {
          this assign other;
        } else {
          ChiselErrors += IllegalConnection("Connecting Input " + this + " to Input " + other, 2);
        }
      case default => ChiselErrors += IllegalConnection("Connecting Input " + this + " to Node " + default, 2);
      }
    } else { // DIR == OUTPUT
      // println("<>'ing " + this + " --> " + src);
      src match { 
        case other: IO  => 
          if (other.dir == INPUT || isCellIO) {
            other assign this;
          } else {
            ChiselErrors += IllegalConnection("Connecting Output " + this + " to Output " + other, 2);
          }
        case default => 
          //println("Connecting Output " + this + " to Node " + default);
      }
    } 
  }
  * */

  override def <>(src: Node) = { 
    if (dir == INPUT) {
      // println("<>'ing " + this + " <-- " + src);
      src match { 
      case other: IO => 
	if (other.dir == OUTPUT && this.staticComp == other.staticComp && !isCellIO) {
	  other assign this;
        } else if (isCellIO || (other.dir == OUTPUT && this.staticComp.staticParent == other.staticComp.staticParent)) {
          this assign other;
        } else if (other.dir == INPUT) {
	  if(this.staticComp == other.staticComp.staticParent)
	    other assign this;
	  else if(this.staticComp.staticParent == other.staticComp)
	    this assign other;
	  else
	    ChiselErrors += IllegalConnection("Connecting Input " + this + " Input " + other, 2);
	} else {
          ChiselErrors += IllegalConnection("Connecting Input " + this + " to " + other, 2);
        }
      case default => ChiselErrors += IllegalConnection("Connecting Input " + this + " to IO without direction " + default, 2);
      }
    } else { // DIR == OUTPUT
      // println("<>'ing " + this + " --> " + src);i
      src match { 
        case other: IO  => 
	  if (other.dir == INPUT && this.staticComp == other.staticComp && !isCellIO){
	    this assign other;
	  } else if (isCellIO || (other.dir == INPUT && this.staticComp.staticParent == other.staticComp.staticParent)) {
            other assign this;
          } else if (other.dir == OUTPUT) {
	    if(this.staticComp == other.staticComp.staticParent)
	      this assign other;
	    else if (this.staticComp.staticParent == other.staticComp)
	      other assign this;
	    else if (this.isCellIO && other.isCellIO)
	      ChiselErrors += IllegalConnection("Ambiguous Connection of Two Nodes", 2);
	    else if (this.isCellIO){
	      other assign this; }
	    else if (other.isCellIO){
	      this assign other; }
	    else
	      ChiselErrors += IllegalConnection("Connecting Output " + this + " to Output " + other, 2);
	  } else {
            ChiselErrors += IllegalConnection("Connecting Output " + this + " to IO without direction " + other, 2);
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
            ChiselErrors += IllegalConnection("^^ing Input " + this + " to Output " + other, 2);
        case default => 
          ChiselErrors += IllegalConnection("// ^^ing Input " + this + " to Node " + default, 2);
      }
    } else { // dir == OUTPUT
      // println("^^ing " + this + " COMP " + component + " & " + src + " SRC COMP " + src.component + " CHILD? " + isChild);
      parent match { 
        case other: IO  => 
          if (other.dir == OUTPUT || isCellIO) {
            other assign this;
          } else
            ChiselErrors += IllegalConnection("^^ing Output " + this + " to Input " + other, 2);
        case default => 
          ChiselErrors += IllegalConnection("^^ing Output " + this + " to Node " + default, 2);
      } 
    }
  }
  override def assign(src: Node) = {
    if(assigned)
      ChiselErrors += IllegalState("reassignment to output", 3);
    else { 
      assigned = true; 
      if (inputs.length > 0) inputs(0) = src; else inputs += src;
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
