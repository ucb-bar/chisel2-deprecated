import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore
import Chisel._

class FPAssignSuite extends TestSuite {

  @Test def floatAssignTest {
    class FpAssign extends Module {
      val io = new Bundle {
        val in = UInt(INPUT, 32)
        val out = Flo(OUTPUT)
      }
      val reg = RegNext(io.in)
      io.out.equalsBits(reg)
    }

    class FpAssignTests( c : FpAssign ) extends Tester(c) {
      poke(c.io.in, 1056964608)
      step(1)
      expect(c.io.out, (0.5).toFloat)
    }

    launchCppTester( (c : FpAssign) => new FpAssignTests(c) )
    chiselMain(Array[String]("--v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new FpAssign()))
    assertFile("FPAssignSuite_FpAssign_1.v")

  }

}
