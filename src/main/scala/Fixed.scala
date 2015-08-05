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

/** A Fixed point data type */
class Fixed(var fractionalWidth : Int = 0) extends Bits with Num[Fixed] {
    type T = Fixed

    /** Convert a Node to a Fixed data type with the same fractional width as this instantiation */
    override def fromNode(n : Node): this.type = {
        val res = Fixed(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
        res.fractionalWidth = this.fractionalWidth
        res
    }

    /** Create a Fixed representation from an Int */
    override def fromInt(x : Int) : this.type = {
        Fixed(x, this.getWidth(), this.fractionalWidth).asInstanceOf[this.type]
    }

    /** clone this Fixed instantiation */
    override def cloneType: this.type = {
        val res = Fixed(this.dir, this.getWidth(), this.fractionalWidth).asInstanceOf[this.type];
        res
    }

    def getFractionalWidth : Int = this.fractionalWidth

    /** Ensure two Fixed point data types have the same fractional width, Error if not */
    def checkAligned(b : Fixed) = if(this.fractionalWidth != b.fractionalWidth) ChiselError.error(this.fractionalWidth + " Fractional Bits does not match " + b.fractionalWidth)

    /** Convert a SInt to a Fixed by reinterpreting the Bits */
    def fromSInt(s : SInt) : Fixed = {
        val res = chiselCast(s){Fixed()}
        res.fractionalWidth = fractionalWidth
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
    def unary_-() : Fixed = Fixed(0, this.getWidth(), this.fractionalWidth) - this

    def + (b : Fixed) : Fixed = {
        checkAligned(b)
        fromSInt(this.toSInt + b.toSInt)
    }

    def - (b : Fixed) : Fixed = {
        checkAligned(b)
        fromSInt(this.toSInt - b.toSInt)
    }

    /** Multiply with one bit of rounding */
    def *& (b : Fixed) : Fixed = {
        checkAligned(b)
        val temp = this.toSInt * b.toSInt
        val res = temp + ((temp & UInt(1)<<UInt(this.fractionalWidth-1))<<UInt(1))
        fromSInt(res >> UInt(this.fractionalWidth))
    }

    /** Multiply truncating the result to the same Fixed format */
    def * (b : Fixed) : Fixed = {
        checkAligned(b)
        val temp = this.toSInt * b.toSInt
        fromSInt(temp >> UInt(this.fractionalWidth))
    }

    def / (b : Fixed) : Fixed = {
        checkAligned(b)
        fromSInt((this.toSInt << UInt(this.fractionalWidth)) / b.toSInt)
    }

    /** The remainder of the division this/b */
    def % (b : Fixed) : Fixed = (this / b) & Fill(this.fractionalWidth, UInt(1))
}
