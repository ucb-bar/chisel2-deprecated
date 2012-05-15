package Tutorial {

import Chisel._

object Tutorial {
  def main(args: Array[String]): Unit = { 
    val tut_args = args.slice(1, args.length) ++ Array("--target-dir", "../emulator", "--gen-harness");
    args(0) match {
      case "GCD" => chiselMain(tut_args, () => new GCD());
    }
  }
}

}
