/*
 Copyright (c) 2011 - 2016 The Regents of the University of
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

package SysCTest

import Chisel._
import collection.mutable.ArrayBuffer

class AddFilter extends Module {
   val io = new Bundle {
     val a = Decoupled( UInt(width = 16) ).flip()
     val b = Decoupled( UInt(width = 16) )
   }

   io.b.bits := io.a.bits + UInt(10)
   io.a.ready := io.b.ready
   io.b.valid := io.a.valid
}

class MyCounter extends Module {
   val io = new Bundle {
      val out = Decoupled(UInt(width = 4))
   }

   val c = Reg(UInt(0, width = 8))

   val out = UInt(width = 8)
   out := c
   io.out.bits := out(7,4)
   io.out.valid := out(3,0) === UInt(0)
   when(io.out.ready){
      c := c + UInt(1)
   }
}


class AddTests(c: AddFilter) extends Tester(c) {
  step(1)
}

class CounterTests(c: MyCounter) extends Tester(c) {
  step(1)
}

object AddFilter {
  def main(args: Array[String]): Unit = {
    /* Copy the initial arguments.
     * We may alter these before passing them on.
     */
    var argsBuild: ArrayBuffer[String] = ArrayBuffer(args : _*)

    //Pull out design name
    var design:String = "AddFilter"
    for(i <- 0 until args.length){
       if(args(i).equals("--design")) {
          design = args(i + 1)
    // Remove the arguments that only we understand
    argsBuild.remove(i, 2)
       }
    }
    println(design)

    val argsPass = argsBuild.toArray

    //Main
    design match{
       case "AddFilter" => {
          chiselMainTest(argsPass, () => Module(new AddFilter())){
             c => new AddTests(c)
          }
       }
       case "MyCounter" => {
          chiselMainTest(argsPass, () => Module(new MyCounter())){
             c => new CounterTests(c)
          }
       }
       case x => {
          println("No design chosen!")
       }
    }

    /*
    //Create component
    val component:Module =
       design match {
          case "AddFilter" => new AddFilter()
          case "MyCounter" => new MyCounter()
          case _ => new AddFilter()
       }
    println(component)

    chiselMainTest(args, () => Module(new AddFilter())) {
      c => new MyTests(c) }
    */
  }
}
