// author: jonathan bachrach
package Chisel {


import Node._;
import Component._;
import IOdir._;

object IOdir {
  val INPUT  = new IOdir(0)
  val OUTPUT = new IOdir(1)
}
class IOdir (idi: Int) {
  val id = idi
  override def toString: String = if (id == 0) "INPUT-DIR" else "OUTPUT-DIR";
}

object Input {
  def apply(width: Int): IO = apply("", width)
  def apply(name: String): IO = apply(name, widthOf(0))
  def apply(name: String, width: Int): IO = 
    { val res = new IO(); res.dir = INPUT; res.init(name, width); res }
  def apply(name: String, inferWidth: (Node) => Int): IO = 
    { val res = new IO(); res.dir = INPUT; res.init(name, inferWidth); res }
}
object Output {
  def apply(width: Int): IO = apply("", width);
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
  var assigned = false;
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
  override def apply(name: String): Interface = this
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
        if (other.dir == OUTPUT) {
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
          if (other.dir == INPUT) {
            other := this;
          } else {
            println("Connecting Output " + this + " to Output " + other);
          }
        case default => 
          println("Connecting Output " + this + " to Node " + default);
      }
    } 
  }
  override def ^^(parent: Node) = { 
    if (dir == INPUT) {
      // println("^^ " + this + " COMP " + component + " & " + src + " SRC COMP " + src.component);
      parent match { 
        case other: IO => 
          if (other.dir == INPUT) {
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
          if (other.dir == OUTPUT) {
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
      throw new IllegalStateException("resassignment to output");
    else { assigned = true; super.:=(src)}
  }
};

}
