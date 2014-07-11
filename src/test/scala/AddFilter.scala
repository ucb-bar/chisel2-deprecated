package Test

import Chisel._

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
    //Pull out design name
    var design:String = ""
    for(i <- 0 until args.length){
       if(args(i).equals("--design"))
          design = args(i + 1)
    }
    println(design)

    //Main
    design match{
       case "AddFilter" => {
          chiselMainTest(args, () => Module(new AddFilter())){
             c => new AddTests(c)
          }
       }
       case "MyCounter" => {       
          chiselMainTest(args, () => Module(new MyCounter())){
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
