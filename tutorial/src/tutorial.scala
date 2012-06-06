package Tutorial {

import Chisel._

object Tutorial {
  def main(args: Array[String]): Unit = { 
    val tut_args = args.slice(1, args.length) ++ Array("--target-dir", "../emulator", "--gen-harness");
    val res = 
    args(0) match {
      case "gcd" => 
        chiselMain(tut_args, () => new GCD())
      case "combinational" => 
        chiselMain(tut_args, () => new Combinational())
      case "functional" => 
        chiselMain(tut_args, () => new Functional())
      case "mux2" => 
        chiselMain(tut_args, () => new Mux2())
      case "sequential" => 
        chiselMain(tut_args, () => new Sequential())
      case "parity" => 
        chiselMain(tut_args, () => new Parity())
      case "memo" => 
        chiselMain(tut_args, () => new Memo())
      case "filter" => 
        chiselMain(tut_args, () => new Filter())
    }
  }
}

}
