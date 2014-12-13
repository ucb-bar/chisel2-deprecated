/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

package Chisel
import scala.math.log
import scala.math.abs
import scala.math.ceil
import scala.math.max
import scala.math.min
import Literal._
import ChiselError._

/* Factory for literal values to be used by Bits and SInt factories. */
object Lit {
  def apply[T <: Bits](n: String, width: Int = -1)(gen: => T): T = {
    makeLit(Literal(n, width))(gen)
  }

  def apply[T <: Bits](n: String, base: Char, width: Int = -1)(gen: => T): T = {
    makeLit(Literal(width, base, n))(gen)
  }

  def apply[T <: Bits](n: BigInt)(gen: => T): T = {
    makeLit(Literal(n, signed = gen.isInstanceOf[SInt]))(gen)
  }

  def apply[T <: Bits](n: BigInt, width: Int)(gen: => T): T = {
    makeLit(Literal(n, width, signed = gen.isInstanceOf[SInt]))(gen)
  }

  def makeLit[T <: Bits](x: Literal)(gen: => T): T = {
    gen.fromNode(x)
  }
}

object Literal {

  private def bigMax(x: BigInt, y: BigInt): BigInt = if (x > y) x else y;
  def sizeof(x: BigInt): Int = {
    val y = bigMax(BigInt(1), x.abs).toDouble;
    val res = max(1, (ceil(log(y + 1)/log(2.0))).toInt);
    res
   }

  @deprecated("This part of the implementation is not used anymore?", "2.0")
  def signedsizeof(x: BigInt, width: Int = -1, signed: Boolean = false): (Int, String) = {
    var count = 0;
    var n = x;
    var resNum = BigInt(0);
    while((x > 0 && n != 0) || (x < 0 && n != -1)){
      resNum += (n & BigInt(1)) << count;
      count += 1;
      n >>= 1;
    }
    var resWidth = count + (
      if(x == -1) {
        2
      } else if(signed || x == 0) {
        1
      } else {
        0
      });
    resNum += (
      if(x == -1) {
        3
      } else if(x < 0) {
        1 << (resWidth-1)
      } else {
        0
      });
    if(width != -1) {
      if(width < resWidth) {
        ChiselError.error({"width " + width + " is too small for literal " + x});
      } else if(width > resWidth && x < 0) {
        while(width > resWidth){
          resWidth += 1;
          resNum   += 1 << (resWidth-1);
        }
      } else {
        resWidth = width
      }
    }
    ((resWidth, resNum.toString(16)))
  }

  private def sizeof(base: Char, x: String): Int = {
    var res = 0;
    var first = true;
    val size =
      if(base == 'b') {
        1
      } else if(base == 'h') {
        4
      } else if(base == 'o') {
        3
      } else {
        -1
      }
    for(c <- x)
      if (c == '_') {

      } else if(first) {
        first = false;
        res += sizeof(c.asDigit);
      } else if (c != '_') {
        res += size;
      }
    res
  }
  val hexNibbles = "0123456789abcdef";
  def toHexNibble(x: String, off: Int): Char = {
    var res = 0;
    for (i <- 0 until 4) {
      val idx = off + i;
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
    for (i <- 0 until numNibbles) {
      res += toHexNibble(x, i*4 - pad);
    }
    res
  }
  def toLitVal(x: String): BigInt = {
    // eliminate the hex prefix from the string.
    // NOTE: there may be a leading '-'
    // We need to accept the fact that fundamentally,
    // all numeric quantities are signed in Java/Scala.
    BigInt(x.replace("0x", ""), 16)
  }

  def toLitVal(x: String, shamt: Int): BigInt = {
    var res = BigInt(0);
    for(c <- x)
      if(c != '_'){
        if(!(hexNibbles + "?").contains(c.toLower)) ChiselError.error({"Literal: " + x + " contains illegal character: " + c});
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
        if(!"01?".contains(d)) ChiselError.error({"Literal: " + x + " contains illegal character: " + d});
        width += 1;
        mask   = mask + (if (d == '?') "0" else "1");
        bits   = bits + (if (d == '?') "0" else d.toString);
      }
    }
    (bits, mask, width)
  }
  def stringToVal(base: Char, x: String): BigInt = {
    if(base == 'x') {
      toLitVal(x);
    } else if(base == 'd') {
      BigInt(x.toInt)
    } else if(base == 'h') {
      toLitVal(x, 16)
    } else if(base == 'b') {
      toLitVal(x, 2)
    } else if(base == 'o') {
      toLitVal(x, 8)
    } else {
      BigInt(-1)
    }
  }

  /** Derive the bit length for a Literal
   *  
   */
  def bitLength(b: BigInt): Int = {
    // Check for signedness
    // We have seen unexpected values (one too small) when using .bitLength on negative BigInts,
    // so use the positive value instead.
    val usePositiveValueForBitLength = false
    (if (usePositiveValueForBitLength && b < 0) {
      -b
    } else {
      b
    }).bitLength
  }
  /** Creates a *Literal* instance from a scala integer.
    */
  def apply(x: BigInt, width: Int = -1, signed: Boolean = false): Literal = {
    val res = new Literal();
    // Check for signedness
    // We get unexpected values (one too small) when using .bitLength on negative BigInts,
    // so use the positive value instead.
    val bl = bitLength(x)
    val xWidth = if (signed) {
      bl + 1
    } else {
      max(bl, 1)
    }
    val w = if(width == -1) xWidth else width
    val xString = if (x >= 0) {
      "0x" + x.toString(16)
    } else {
      "-0x" + (-x).toString(16)
    }

    if(xWidth > width && width != -1) {
      // Is this a zero-width wire with value 0
      if (!(x == 0 && width == 0 && Driver.isSupportW0W)) {
        ChiselError.error({"width " + width + " is too small for literal " + x + ". Smallest allowed width is " + xWidth});
      }
    }
    res.init(xString, w);
    res.hasInferredWidth = width == -1
    res
  }

  /** Creates a *Literal* instance from a scala string. The first character
    of the string indicates the base for the suffix characters.
    */
  def apply(n: String, width: Int): Literal =
    apply(width, n(0), n.substring(1, n.length));

  def apply(width: Int, base: Char, literal: String): Literal = {
    if (!"dhbo".contains(base)) {
      ChiselError.error("no base specified");
    }
    val res = new Literal();
    if(width == -1) {
      res.init(removeUnderscore(literal), sizeof(base, literal));
    } else {
      res.init(removeUnderscore(literal), width);
      if(width < sizeof(base, literal)) {
        ChiselError.error({"width " + width + " is too small for literal: " + res + " with min width " + sizeof(base, literal)})
      }
    }
    res.base = base;
    if (base == 'b') res.isZ = literal.contains('?')
    res
  }
}

/** Stores the actual value of a scala literal as a string.
  This class should not end-up being instantiated directly in user code.
  */
class Literal extends Node {
  var hasInferredWidth = false
  var isZ = false;
  var base = 'x';
  lazy val value: BigInt = stringToVal(base, name);
  override def litOf: Literal = this
  override def toString: String = name;
  override lazy val isInVCD: Boolean = false

  override def canCSE: Boolean = true
  override def hashCodeForCSE: Int = value.toInt
  override def equalsForCSE(x: Node): Boolean = x match {
    case x: Literal => value == x.value && isZ == x.isZ && widthW == x.widthW
    case _ => false
  }
}
