/*
 Copyright (c) 2011, 2012, 2013, 2014 The University of Sydney.
 All Rights Reserved.  Redistribution and use in source and
 binary forms, with or without modification, are permitted
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

import Node._
import ChiselError._

/** Factory methods for [[Chisel.Fixed Fixed]] */
object Fixed {
    /** Convert a double to fixed point with a specified fractional width
      * @param x Double to convert
      * @param fracWidth the integer fractional width to use in the conversion
      * @return A BigInt representing the bits in the fixed point
      */
    def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
    /** Convert a Float to fixed point with a specified fractional width
      * @param x Float to convert
      * @param fracWidth the integer fractional width to use in the conversion
      * @return A BigInt representing the bits in the fixed point
      */
    def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
    /** Convert an Int to fixed point with a specified fractional width
      * @param x Double to convert
      * @param fracWidth the integer fractional width to use in the conversion
      * @return A BigInt representing the bits in the fixed point
      */
    def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))

    /** Create a Fixed [[Chisel.Node]] with specified width and fracWidth
      * @param x An Int to convert to fixed point
      * @param width the total number of bits to use in the representation
      * @param fracWidth the integer fractional width to use in the conversion
      * @return A fixed node with the specified parameters
      */
    def apply(x : Int, width : Int, fracWidth : Int) : Fixed = apply(toFixed(x, fracWidth), width, fracWidth)
    /** Create a Fixed [[Chisel.Node]] with specified width and fracWidth
      * @param x An Float to convert to fixed point
      * @param width the total number of bits to use in the representation
      * @param fracWidth the integer fractional width to use in the conversion
      * @return A fixed node with the specified parameters
      */
    def apply(x : Float, width : Int, fracWidth : Int) : Fixed = apply(toFixed(x, fracWidth), width, fracWidth)
    /** Create a Fixed [[Chisel.Node]] with specified width and fracWidth
      * @param x An Double to convert to fixed point
      * @param width the total number of bits to use in the representation
      * @param fracWidth the integer fractional width to use in the conversion
      * @return A fixed node with the specified parameters
      */
    def apply(x : Double, width : Int, fracWidth : Int) : Fixed = apply(toFixed(x, fracWidth), width, fracWidth)
    /** Create a Fixed [[Chisel.Node]] with specified width and fracWidth
      * @param x An BigInt to use literally as the fixed point bits
      * @param width the total number of bits to use in the representation
      * @param fracWidth the integer fractional width to use
      * @return A fixed node with the specified parameters
      */
    def apply(x : BigInt, width : Int, fracWidth : Int) : Fixed =  {
      val res = Lit(x, width){Fixed()}
      res.fractionalWidth = fracWidth
      res
    }

    /** Create a Fixed I/O [[Chisel.Node]] with specified width and fracWidth
      * @param dir Direction of I/O for the node, eg) INPUT or OUTPUT
      * @param width the total number of bits to use in the representation
      * @param fracWidth the integer fractional width to use
      * @return A fixed node with the specified parameters
      */
    def apply(dir : IODirection = null, width : Int = -1, fracWidth : Int = -1) : Fixed = {
        val res = new Fixed(fracWidth);
        res.create(dir, width)
        res
    }
}

/** A Fixed point data type
  * @constructor Use [[Chisel.Fixed$ Fixed]] object to create rather than this class directly */
class Fixed(var fractionalWidth : Int = 0) extends Bits with Num[Fixed] {
    type T = Fixed

    /** Convert a Node to a Fixed data type with the same fractional width as this instantiation */
    override def fromNode(n : Node): this.type = {
        val res = Fixed(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
        res.fractionalWidth = this.getFractionalWidth()
        res
    }

    /** Create a Fixed representation from an Int */
    override def fromInt(x : Int) : this.type = Fixed(x, this.getWidth(), this.getFractionalWidth()).asInstanceOf[this.type]

    /** clone this Fixed instantiation */
    override def cloneType: this.type = Fixed(this.dir, this.getWidth(), this.getFractionalWidth()).asInstanceOf[this.type];

    override protected def colonEquals(that : Bits): Unit = that match {
      case f: Fixed => {
        val res = if((f.getWidth() == this.getWidth()*2) && (f.getFractionalWidth() == this.getFractionalWidth()*2)) {
          truncate(f, this.getFractionalWidth())
        } else {
          checkAligned(f)
          f
        }
        super.colonEquals(res)
      }
      case _ => illegalAssignment(that)
    }

    def getFractionalWidth() : Int = this.fractionalWidth

    private def truncate(f : Fixed, truncateAmount : Int) : Fixed = fromSInt(f.toSInt >> UInt(truncateAmount))
    private def truncate(f : SInt, truncateAmount : Int) : SInt = f >> UInt(truncateAmount)

    /** Ensure two Fixed point data types have the same fractional width, Error if not */
    private def checkAligned(b : Fixed) {
      if(this.getFractionalWidth() != b.getFractionalWidth()) ChiselError.error(this.getFractionalWidth() + " Fractional Bits does not match " + b.getFractionalWidth())
      if(this.getWidth() != b.getWidth()) ChiselError.error(this.getWidth() + " Width does not match " + b.getWidth())
    }

    /** Convert a SInt to a Fixed by reinterpreting the Bits */
    private def fromSInt(s : SInt, width : Int = this.getWidth(), fracWidth : Int = this.getFractionalWidth()) : Fixed = {
        val res = chiselCast(s){Fixed()}
        res.fractionalWidth = fracWidth
        res.width = width
        res
    }

    // Order Operators
    def > (b : Fixed) : Bool = {
        checkAligned(b)
        this.toSInt > b.toSInt
    }

    def < (b : Fixed) : Bool = {
        checkAligned(b)
        this.toSInt < b.toSInt
    }

    def >= (b : Fixed) : Bool = {
        checkAligned(b)
        this.toSInt >= b.toSInt
    }

    def <= (b : Fixed) : Bool = {
        checkAligned(b)
        this.toSInt <= b.toSInt
    }

    def === (b : Fixed) : Bool = {
        checkAligned(b)
        this.toSInt === b.toSInt
    }

    def >> (b : UInt) : Fixed = {
        fromSInt(this.toSInt >> b)
    }

    // Arithmetic Operators
    def unary_-() : Fixed = Fixed(0, this.getWidth(), this.getFractionalWidth()) - this

    def + (b : Fixed) : Fixed = {
        checkAligned(b)
        fromSInt(this.toSInt + b.toSInt)
    }

    def - (b : Fixed) : Fixed = {
        checkAligned(b)
        fromSInt(this.toSInt - b.toSInt)
    }

    /** Multiply increasing the Bit Width */
    def * (b : Fixed) : Fixed = {
        checkAligned(b)
        val temp = this.toSInt * b.toSInt
        fromSInt(temp, temp.getWidth(), this.getFractionalWidth()*2)
    }

    /** Multiply with one bit of rounding */
    def *& (b : Fixed) : Fixed = {
        checkAligned(b)
        val temp = this.toSInt * b.toSInt
        val res = temp + ((temp & UInt(1)<<UInt(this.getFractionalWidth()-1))<<UInt(1))
        fromSInt(truncate(res, this.getFractionalWidth()))
    }

    /** Multiply truncating the result to the same Fixed format */
    def *% (b : Fixed) : Fixed = {
        checkAligned(b)
        val temp = this.toSInt * b.toSInt
        fromSInt(truncate(temp, this.getFractionalWidth()))
    }

    def / (b : Fixed) : Fixed = {
        checkAligned(b)
        fromSInt((this.toSInt << UInt(this.getFractionalWidth())) / b.toSInt)
    }

    /** This is just the modulo of the two fixed point bit representations changed into SInt and operated on */
    def % (b : Fixed) : Fixed = {
      checkAligned(b)
      fromSInt(this.toSInt % b.toSInt)
    }
}
