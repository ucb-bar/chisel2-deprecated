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
        chiselMainTest(tutArgs, () => new GCD()){
          c => new GCDTests(c)}
      case "combinational" => 
        chiselMainTest(tutArgs, () => new Combinational()){
          c => new CombinationalTests(c)}
      case "functional" => 
        chiselMainTest(tutArgs, () => new Functional()){
          c => new FunctionalTests(c)}
      case "mux2" => 
        chiselMainTest(tutArgs, () => new Mux2()){
          c => new Mux2Tests(c)}
      case "mux4" =>
        chiselMainTest(tutArgs, () => new Mux4()){
          c => new Mux4Tests(c)}
      case "sequential" => 
        chiselMainTest(tutArgs, () => new Sequential()){
          c => new SequentialTests(c)}
      case "parity" => 
        chiselMainTest(tutArgs, () => new Parity()){
          c => new ParityTests(c)}
      case "memo" => 
        chiselMainTest(tutArgs, () => new Memo()){
          c => new MemoTests(c)}
      case "filter" => 
        chiselMainTest(tutArgs, () => new Filter()){
          c => new FilterTests(c)}
      case "tbl" => 
        chiselMainTest(tutArgs, () => new Tbl()){
          c => new TblTests(c)}
      case "life" => 
        chiselMainTest(tutArgs, () => new Life(3)){
          c => new LifeTests(c)}
      case "mul" => 
        chiselMainTest(tutArgs, () => new Mul()){
          c => new MulTests(c)}
      case "echo" => 
        chiselMainTest(tutArgs, () => new Echo("../src/in.wav", "../out.wav")){
          c => new EchoTests(c)}
      case "risc" => 
        chiselMainTest(tutArgs, () => new Risc()){
          c => new RiscTests(c)}
    }
  }
}

}
