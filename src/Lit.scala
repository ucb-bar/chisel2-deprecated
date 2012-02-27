// author: jonathan bachrach
package Chisel {

import scala.math.log;
import scala.math.abs;
import scala.math.ceil;
import scala.math.max;
import scala.math.min;
import Literal._;
import IOdir._;
import ChiselError._;

object Lit {
  def apply[T <: Bits](x: Int)(gen: => T): T = {
    makeLit(Literal(x, signed = gen.isInstanceOf[Fix]))(gen)
  }

  def apply[T <: Bits](x: Int, width: Int)(gen: => T): T = {
    makeLit(Literal(x, width, gen.isInstanceOf[Fix]))(gen)
  }

  def apply[T <: Bits](n: String, width: Int = -1)(gen: => T): T = {
    makeLit(Literal(n, width))(gen)
  }

  def apply[T <: Bits](width: Int, base: Char, literal: String)(gen: => T): T = {
    makeLit(Literal(width, base, literal))(gen)
  }

  def apply(value: Boolean): Bool = 
    makeBool((if(value) Literal(1) else Literal(0)))

  def makeLit[T <: Bits](x: Literal)(gen: => T): T = {
    val output= gen.asOutput
    output.setIsCellIO
    output assign x
    output
  }

  def makeBool(x: Literal): Bool = {
    val output = Bool(OUTPUT)
    output.setIsCellIO
    output assign x
    output
  }
}

object Literal {
  implicit def intToLit (x: Int) = Literal(x);
  def bigMax(x: BigInt, y: BigInt) = if (x > y) x else y;
  def sizeof(x: BigInt): Int = { 
    val y = bigMax(BigInt(1), x.abs).toDouble;
    val res = max(1, (ceil(log(y+1)/log(2.0))).toInt);
    // println("SIZEOF " + y + " LOG2 " + (log(y)/log(2.0)) + " IS " + res);
     res
   }

  def signedsizeof(x: BigInt, width: Int = -1, signed: Boolean = false): (Int, String) = {
    var count = 0;
    var n = x;
    var resNum = BigInt(0);
    while((x > 0 && n != 0) || (x < 0 && n != -1)){
      resNum += (n & BigInt(1)) << count;
      count += 1;
      n >>= 1;
    }
    var resWidth = count + (if(x == -1) 2 
			    else if(signed || x == 0) 1
			    else 0);
    resNum += (if(x == -1) 3
	       else if(x < 0) 1 << (resWidth-1)
	       else 0); 
    if(width != -1)
      if(width < resWidth)
	ChiselErrors += IllegalState("width " + width + " is too small for literal " + x, 4);
      else if(width > resWidth && x < 0){
	while(width > resWidth){
	  resWidth += 1;
	  resNum   += 1 << (resWidth-1);
	}
      } else {resWidth = width}
    ((resWidth, resNum.toString(16)))
  }
  
  def sizeof(base: Char, x: String): Int = {
    var res = 0;
    var first = true;
    val size = 
      if(base == 'b')
	1
      else if(base == 'h')
	4
      else if(base == 'o')
	3
      else
	-1	
    for(c <- x) 
      if(first) {
	first = false;
	res += sizeof(c.asDigit);
      } else if (c != '_') 
	res += size;
    res
  }
  val hexNibbles = "0123456789abcdef";
  def toHexNibble(x: String, off: Int): Char = {
    var res = 0;
    // println("OFF = " + off);
    for (i <- 0 until 4) {
      val idx = off+i;
      val c   = if (idx < 0) '0' else x(idx);
      res     = 2 * res + (if (c == '1') 1 else 0);
    }
    hexNibbles(res)
  }
  val pads = Vector(0, 3, 2, 1);
  def toHex(x: String): String = {
    var res = "";
    val numNibbles = (x.length-1) / 4 + 1;
    val pad = pads(x.length % 4);
    // println("X = " + x + " NN = " + numNibbles + " PAD = " + pad);
    for (i <- 0 until numNibbles) {
      res += toHexNibble(x, i*4 - pad);
    }
    res
  }
  def toLitVal(x: String): BigInt = {
    var res = BigInt(0);
    for (c <- x.substring(2, x.length)) 
      res = res * 16 + c.asDigit;
    res
  }

  def toLitVal(x: String, shamt: Int): BigInt = {
    var res = BigInt(0);
    for(c <- x)
      if(c != '_'){
	if(!(hexNibbles + "?").contains(c)) ChiselErrors += IllegalState("Literal: " + x + " contains illegal character: " + c, 4);
	res = res * shamt + c.asDigit;
      }
    res
  }

  def removeUnderscore(x: String): String = {
    var res = ""
    for(c <- x){
      if(c != '_'){
	res = res + c
      }
    }
    res
  }

  def parseLit(x: String): (String, String, Int) = {
    var bits = "";
    var mask = "";
    var width = 0;
    for (d <- x) {
      if (d != '_') {
	if(!"01?".contains(d)) ChiselErrors += IllegalState("Literal: " + x + " contains illegal character: " + d, 4);
        width += 1;
        mask   = mask + (if (d == '?') "0" else "1");
        bits   = bits + (if (d == '?') "0" else d.toString);
      }
    }
    (bits, mask, width)
  }
  def stringToVal(base: Char, x: String): BigInt = {
    if(base == 'x')
      toLitVal(x);
    else if(base == 'd')
      x.toInt
    else if(base == 'h')
      toLitVal(x, 16)
    else if(base == 'b')
      toLitVal(x, 2)  
    else if(base == 'o')
      toLitVal(x, 8)
    else
      -1L
  }

  // def apply(x: Int, width: Int = -1, signed: Boolean = false): Literal = 
  //   apply(x.toLong, width, signed)
  def apply(x: BigInt, width: Int = -1, signed: Boolean = false): Literal = { 
    val res = new Literal(); 
    val (w, numString) = signedsizeof(x, width, signed);
    //val n = "0x" + numString;
    res.init("0x" + numString, w); 
    res.inputVal = x;
    res 
  }
  // def apply(x: Int, width: Int, signed: Boolean = false): Literal = { 
  //   val res = new Literal(); 
  //   val (w, numString) = signedsizeof(x, width, signed);
  //   res.init("0x" + numString, width); 
  //   res 
  // }
  //def apply(x: Long, width: Int): Literal = { val res = new Literal(); res.init("0x%x".format(x), width); res }
  // def apply(n: String): Lit = { 
  //   val (bits, mask, width) = parseLit(n);  apply(n, width);
  // }
  def apply(n: String, width: Int): Literal = 
    apply(width, n(0), n.substring(1, n.length));
  def apply(width: Int, base: Char, literal: String): Literal = {
    if (!"dhbo".contains(base)) ChiselErrors += IllegalArgument("no base specified", 4);
    val res = new Literal();
    if(width == -1)
      res.init(removeUnderscore(literal), sizeof(base, literal));
    else{
      res.init(removeUnderscore(literal), width); 
      if(width < sizeof(base, literal)) 
	ChiselErrors += IllegalState("width " + width + " is too small for literal: " + res + " with min width " + sizeof(base, literal), 4)
    }
    res.base = base;
    if (base == 'b') {res.isZ = literal.contains('?'); res.isBinary = true;}
    res
  }
}
class Literal extends Node {
  //implicit def intToLit (x: Int) = Lit(x);
  var isZ = false;
  var isBinary = false;
  var base = 'x';
  var inputVal = BigInt(0);
  // override def toString: String = "LIT(" + name + ")"
  override def value: BigInt = stringToVal(base, name);
  override def maxNum = value;
  override def minNum = value;
  override def isLit = true;
  override def toString: String = name;
  override def emitDecC: String = "";
  override def emitRefC: String = 
    (if (isBinary) { 
      var (bits, mask, swidth) = parseLit(name);
      var bwidth = if(base == 'b') width else swidth;
      if (isZ) {
        ("LITZ<" + bwidth + ">(0x" + toHex(bits) + ", 0x" + toHex(mask) + ")")
      } else
        ("LIT<" + bwidth + ">(0x" + toHex(bits) + ")")
    } else if(base == 'd' || base == 'x'){
      ("LIT<" + width + ">(" + name + "L)")
    } else
      ("LIT<" + width + ">(0x" + name + "L)")
   ) + "/* " + inputVal + "*/";

  override def emitRefVCD: String = 
    (if (isBinary) { 
      var (bits, mask, swidth) = parseLit(name);
      var bwidth = if(base == 'b') width else swidth;
      if (isZ) {
        ("LITZ<" + bwidth + ">(0x" + toHex(bits) + ", 0x" + toHex(mask) + ")")
      } else
        ("LIT<" + bwidth + ">(0x" + toHex(bits) + ")")
    } else if(base == 'd' || base == 'x'){
      ("LIT<" + width + ">(" + name + "L)")
    } else
      ("LIT<" + width + ">(0x" + name + "L)")
   );

  override def emitDec: String = "";
  override def emitRefV: String = 
    (if (width == -1) name 
    else if(isBinary) ("" + width + "'b" + name)
    else if(base == 'x') ("" + width + "'h" + name.substring(2, name.length))
    else if(base == 'd') ("" + width + "'d" + name)
    else if(base == 'h') ("" + width + "'h" + name)
    else "") + "/* " + inputVal + "*/";
  def d (x: BigInt): Literal = Literal(x, value.toInt)
  //def ~(x: String): Lit = Lit(value, x(0), x.substring(1, x.length));
}

}
