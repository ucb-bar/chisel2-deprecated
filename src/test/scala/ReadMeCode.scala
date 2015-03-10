import Chisel._

class HelloModule extends Module {
  val io = new Bundle {}
  printf("Hello World!\n")
}

class HelloModuleTests(c: HelloModule) extends Tester(c) {
  step(1)
}

object hello {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array[String]("--backend", "c", "--compile", "--test", "--genHarness"),
       () => Module(new HelloModule())){c => new HelloModuleTests(c)}
  }
}

