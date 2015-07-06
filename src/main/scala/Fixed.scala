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
    def toFixed(x : Double, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
    def toFixed(x : Float, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))
    def toFixed(x : Int, fracWidth : Int) : BigInt = BigInt(scala.math.round(x*scala.math.pow(2, fracWidth)))

    def apply(x : Int, width : Int, fracWidth : Int) : Fixed = apply(toFixed(x, fracWidth), width, fracWidth)
    def apply(x : Float, width : Int, fracWidth : Int) : Fixed = apply(toFixed(x, fracWidth), width, fracWidth)
    def apply(x : Double, width : Int, fracWidth : Int) : Fixed = apply(toFixed(x, fracWidth), width, fracWidth)
    def apply(x : BigInt, width : Int, fracWidth : Int) : Fixed =  { 
      val res = Lit(x, width){Fixed()}
      res.fractionalWidth = fracWidth
      res
    }

    def apply(dir : IODirection = null, width : Int = -1, fracWidth : Int = -1) : Fixed = {
        val res = new Fixed(fracWidth);
        res.create(dir, width)
        res
    }
}

class Fixed(var fractionalWidth : Int = 0) extends Bits with Num[Fixed] {
    type T = Fixed

    /* Fixed Factory Method */
    override def fromNode(n : Node): this.type = {
        val res = Fixed(OUTPUT).asTypeFor(n).asInstanceOf[this.type]
        res.fractionalWidth = this.fractionalWidth
        res
    }

    override def fromInt(x : Int) : this.type = {
        Fixed(x, this.getWidth(), this.fractionalWidth).asInstanceOf[this.type]
    }

    override def cloneType: this.type = {
        val res = Fixed(this.dir, this.getWidth(), this.fractionalWidth).asInstanceOf[this.type];
        res
    }

    def getFractionalWidth : Int = this.fractionalWidth

    def checkAligned(b : Fixed) = if(this.fractionalWidth != b.fractionalWidth) ChiselError.error(this.fractionalWidth + " Fractional Bits does not match " + b.fractionalWidth)

    def fromSInt(s : SInt) : Fixed = {
        val res = chiselCast(s){Fixed()}
        res.fractionalWidth = fractionalWidth
        res
    }

    def toDouble(x : BigInt, fracWidth : Int) : Double = x.toDouble/scala.math.pow(2, fracWidth)

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

    def *& (b : Fixed) : Fixed = {
        checkAligned(b)
        val temp = this.toSInt * b.toSInt
        val res = temp + ((temp & UInt(1)<<UInt(this.fractionalWidth-1))<<UInt(1))
        fromSInt(res >> UInt(this.fractionalWidth))
    }

    def * (b : Fixed) : Fixed = {
        checkAligned(b)
        val temp = this.toSInt * b.toSInt
        fromSInt(temp >> UInt(this.fractionalWidth))
    }

    def / (b : Fixed) : Fixed = {
        checkAligned(b)
        fromSInt((this.toSInt << UInt(this.fractionalWidth)) / b.toSInt)
    }

    // Newton-Rhapson to find 1/x - x_(t+1) = x_t(2 - a*x_t)
    def performNR(in : Fixed, xt : Fixed) : Fixed = xt*(Fixed(2, in.getWidth, in.fractionalWidth) - in*xt)

    def tailNR(in : Fixed, xt : Fixed, it : Int, pipeline : Boolean) : Fixed = {
      val nxt = performNR(in, xt)
      val xtn = if (pipeline) Reg(init=Fixed(0, in.getWidth, in.fractionalWidth), next=nxt) else nxt
      if (it == 0) nxt else tailNR(in, xtn, it - 1, pipeline)
    }

    def /& (b : Fixed) : Fixed = this /& (b, false, b.fractionalWidth/4, 4)
    def /& (b : Fixed, pipeline : Boolean) : Fixed = this /& (b, pipeline, b.fractionalWidth/4, 4)
    def /& (b : Fixed, numNR :  Int) : Fixed = this /& (b, false, b.fractionalWidth/4, numNR)
    def /& (b : Fixed, lookUpPrecision : Int, numNR : Int) : Fixed = this /& (b, false, lookUpPrecision, numNR)

    def /& (b : Fixed, pipeline : Boolean, lookUpPrecision : Int, numNR : Int) = {
        checkAligned(b)

        val lookUp = (0 until scala.math.pow(2, b.getWidth - b.fractionalWidth + lookUpPrecision).toInt).map(i => if (i == 0) Fixed(0, b.getWidth, b.fractionalWidth) else Fixed(1/toDouble(BigInt(i), 1 + lookUpPrecision), b.getWidth, b.fractionalWidth))
        // Put lookUp into ROM
        val lookUpTable = Vec(lookUp)

        val lp = b(b.getWidth - 1, b.fractionalWidth - 1 - lookUpPrecision)
        val x0 = lookUpTable(lp)
        val repb = tailNR(b, x0, numNR, pipeline)
        this * repb
    }

    def % (b : Fixed) : Fixed = (this / b) & Fill(this.fractionalWidth, UInt(1))
}
