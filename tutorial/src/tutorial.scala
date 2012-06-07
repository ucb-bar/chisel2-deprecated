package Tutorial {

import Chisel._

object Tutorial {
  def main(args: Array[String]): Unit = { 
    val userArgs = args.slice(1, args.length) 
    val dir = if (userArgs.contains("--v")) "../verilog" else "../emulator"
    val tutArgs = userArgs ++ Array("--target-dir", dir, "--gen-harness");
    val res = 
    args(0) match {
      case "gcd" => 
        chiselMain(tutArgs, () => new GCD())
      case "combinational" => 
        chiselMain(tutArgs, () => new Combinational())
      case "functional" => 
        chiselMain(tutArgs, () => new Functional())
      case "mux2" => 
        chiselMain(tutArgs, () => new Mux2())
      case "sequential" => 
        chiselMain(tutArgs, () => new Sequential())
      case "parity" => 
        chiselMain(tutArgs, () => new Parity())
      case "memo" => 
        chiselMain(tutArgs, () => new Memo())
      case "filter" => 
        chiselMain(tutArgs, () => new Filter())
      case "tbl" => 
        chiselMain(tutArgs, () => new Tbl())
      case "life" => 
        chiselMain(tutArgs, () => new Life(3))
      case "mul" => 
        chiselMain(tutArgs, () => new Mul())
      case "echo" => 
        chiselMain(tutArgs, () => new Echo("../src/in.wav", "../out.wav"))
    }
  }
}

}
