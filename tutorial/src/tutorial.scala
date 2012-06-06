package Tutorial {

import Chisel._

object Tutorial {
  def main(args: Array[String]): Unit = { 
    val tut_args = args.slice(1, args.length) ++ Array("--target-dir", "../emulator", "--gen-harness");
    val res = 
    args(0) match {
      case "gcd" => 
        chiselMainTest(tut_args, () => new GCD())
      case "combinational" => 
        chiselMain(tut_args, () => new Combinational())
      case "functional" => 
        chiselMain(tut_args, () => new Functional())
      case "mux2" => 
        // chiselMainTest(tut_args, () => new Mux2())(
        //   c => Scanner("%x %x %x", 
        //                c.io.sel, c.io.in0, c.io.in1),
        //   c => Printer("%x %x %x %x", 
        //                c.io.sel, c.io.in0, c.io.in1, c.io.out))
        println("MUX2 STARTING ...")
        chiselMainTest(tut_args, () => new Mux2())
    }
    System.exit(if (res == false) 1 else 0)
  }
}

}
