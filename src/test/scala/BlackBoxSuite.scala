
import org.junit.Assert._
import org.junit.Test
import org.junit.Ignore

import Chisel._

class BlackBoxSuite extends TestSuite {

  class UserBB extends BlackBox {
    val io = new Bundle {
      val in = UInt( INPUT, 4 )
      val out = UInt( OUTPUT, 4 )
    }
    io.in.setName("in")
    io.out.setName("out")
    setModuleName("UserBB")
    io.out := io.in
  }

  class UserClockedBB( clkIn : Clock ) extends BlackBox( bbClock = clkIn ) {
    val io = new Bundle {
      val in = UInt( INPUT, 4 )
      val out = UInt( OUTPUT, 4 )
    }
    io.in.setName("in")
    io.out.setName("out")
    setModuleName("UserClockedBB")
    io.out := Reg( init = UInt(0, 4), next = io.in )
    renameClock( clkIn, "clkIn" )
    renameReset("rst")
  }

  @Test def userBBTest {

    class UserMod extends Module {
      val io = new Bundle {
        val in = UInt( INPUT, 4 )
        val out = UInt( OUTPUT, 4 )
      }
      val userbb = Module( new UserBB )
      userbb.io <> io
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new UserMod))
    assertFile("BlackBoxSuite_UserMod_1.v")
  }

  @Test def userClockedBBTest {

    class UserMod extends Module {
      val io = new Bundle {
        val in = UInt( INPUT, 4 )
        val out = UInt( OUTPUT, 4 )
      }
      val inDelay = Reg( init = UInt( 0, 4 ), next = io.in )
      val usrClk = Clock( src = clock, period = 2 )
      usrClk.setName("usrClk")
      val userbb = Module( new UserClockedBB( usrClk ) )
      userbb.io.in := inDelay
      io.out := userbb.io.out
    }

    chiselMain(Array[String]("--backend", "v",
      "--targetDir", dir.getPath.toString()),
      () => Module(new UserMod))
    assertFile("BlackBoxSuite_UserMod_2.v")
  }

}
