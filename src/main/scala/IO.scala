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

object Input {
  def apply(): IO = apply("", widthOf(0))
  def apply(width: Int): int_t = int_t(INPUT, width);
  def apply(name: String): IO = apply(name, widthOf(0))
  def apply(name: String, width: Int): int_t = 
    { val res = new int_t(); res.dir = INPUT; if(name != "") res.named = true; res.init(name, width); res }
  def apply(name: String, inferWidth: (Node) => Int): IO = 
    { val res = new IO(); res.dir = INPUT; res.init(name, inferWidth); res }
}
object Output {
  def apply(width: Int): int_t = int_t(OUTPUT, width);
  def apply(name: String): IO = apply(name, widthOf(0), null)
  def apply(name: String, inferWidth: (Node) => Int, x: Node): IO = 
    { val res = new IO(); res.dir = OUTPUT; res.init(name, inferWidth, x); res }
  def apply(name: String, width: Int): IO = 
    { val res = new IO(); res.dir = OUTPUT; res.init(name, width); res }
  def apply(name: String, x: Node): IO = apply(name, maxWidth _, x); 
  def apply(x: Node): IO = apply("", x);
}
class IO extends Wire { 
  var dir: IOdir = null;
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
  override def apply(name: String): dat_t = this
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
  override def <>(src: Node) = { 
    if (dir == INPUT) {
      // println("<>'ing " + this + " <-- " + src);
      src match { 
      case other: IO => 
        if (other.dir == OUTPUT || isCellIO) {
          this := other;
        } else {
          println("Connecting Input " + this + " to Input " + other);
        }
      case default => println("Connecting Input " + this + " to Node " + default);
      }
    } else { // DIR == OUTPUT
      // println("<>'ing " + this + " --> " + src);
      src match { 
        case other: IO  => 
          if (other.dir == INPUT || isCellIO) {
            other := this;
          } else {
            println("Connecting Output " + this + " to Output " + other);
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
            this := other;
          } else 
            println("// ^^ing Input " + this + " to Output " + other);
        case default => 
          println("// ^^ing Input " + this + " to Node " + default);
      }
    } else { // dir == OUTPUT
      // println("^^ing " + this + " COMP " + component + " & " + src + " SRC COMP " + src.component + " CHILD? " + isChild);
      parent match { 
        case other: IO  => 
          if (other.dir == OUTPUT || isCellIO) {
            other := this;
          } else
            println("// ^^ing Output " + this + " to Input " + other);
        case default => 
          println("// ^^ing Output " + this + " to Node " + default);
      } 
    }
  }
  override def :=(src: Node) = {
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
  override def setIsClkInput = {isClkInput = true; this := clk;}
  override def toBits = this;
  override def fromBits(src: Node) = {
    val res = new IO().asInstanceOf[this.type];
    res.init("", widthOf(0), src);
    res.dir = dir;
    res := src;
    res
  }
  override def maxNum = {
    if(inputs(0).isLit)
      inputs(0).value
    else if (inputs.length == 1 && isCellIO)
      inputs(0).maxNum
    else
      super.maxNum
  }
};


}
