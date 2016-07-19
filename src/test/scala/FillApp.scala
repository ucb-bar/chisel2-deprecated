//package ChiselTests
import Chisel._
import org.junit.Test
import org.junit.Assert._

class FillSuite extends TestSuite {

  // Issue #Chisel3/233 - Fill(Chisel.UInt, Int)
  @Test def testFillArgOrder() {
    println("\ntestFillArgOrder ...")

    class FillApp() extends Module {
      val io = new Bundle {
        val a = UInt(INPUT, 4)
      }
      val f = Fill(UInt(1,1), 3)
    }
    
    val testArgs = chiselEnvironmentArguments() ++ Array("--targetDir", dir.getPath.toString(),
      "--minimumCompatibility", "3.0.0", "--wError", "--backend", "null")
    intercept[IllegalStateException] {
      chiselMain(testArgs, () => Module(new FillApp()))
    }
    assertTrue(ChiselError.hasErrors)
  }
}
