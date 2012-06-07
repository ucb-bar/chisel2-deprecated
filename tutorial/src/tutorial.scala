package Tutorial {

import Chisel._

object Tutorial {
  def main(args: Array[String]): Unit = { 
    val userArgs = args.slice(1, args.length) 
    val dir = if (userArgs.contains("--v")) "../verilog" else "../emulator"
    val tutArgs = userArgs ++ Array("--target-dir", dir, "--gen-harness");
    val res = 
    args(0) match {
      case "GCD" => 
        chiselMainTest(tutArgs, () => new GCD()){
          c => new GCDTests(c)}
      case "Combinational" => 
        chiselMainTest(tutArgs, () => new Combinational()){
          c => new CombinationalTests(c)}
      case "Functional" => 
        chiselMainTest(tutArgs, () => new Functional()){
          c => new FunctionalTests(c)}
      case "Mux2" => 
        chiselMainTest(tutArgs, () => new Mux2()){
          c => new Mux2Tests(c)}
      case "Mux4" =>
        chiselMainTest(tutArgs, () => new Mux4()){
          c => new Mux4Tests(c)}
      case "Accumulator" => 
        chiselMainTest(tutArgs, () => new Accumulator()){
          c => new AccumulatorTests(c)}
      case "Parity" => 
        chiselMainTest(tutArgs, () => new Parity()){
          c => new ParityTests(c)}
      case "Memo" => 
        chiselMainTest(tutArgs, () => new Memo()){
          c => new MemoTests(c)}
      case "Filter" => 
        chiselMainTest(tutArgs, () => new Filter()){
          c => new FilterTests(c)}
      case "Tbl" => 
        chiselMainTest(tutArgs, () => new Tbl()){
          c => new TblTests(c)}
      case "Life" => 
        chiselMainTest(tutArgs, () => new Life(3)){
          c => new LifeTests(c)}
      case "Mul" => 
        chiselMainTest(tutArgs, () => new Mul()){
          c => new MulTests(c)}
      case "Echo" => 
        chiselMainTest(tutArgs, () => new Echo("../src/in.wav", "../out.wav")){
          c => new EchoTests(c)}
      case "Risc" => 
        chiselMainTest(tutArgs, () => new Risc()){
          c => new RiscTests(c)}
    }
  }
}

}
