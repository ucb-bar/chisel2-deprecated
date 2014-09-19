import org.scalatest.junit.AssertionsForJUnit
import org.junit.Before
import java.io.File
import scala.reflect.ClassTag
import scala.collection.JavaConversions
import Chisel._

class TestSuite extends AssertionsForJUnit {

  val dir = new File("test-outputs")
  val blankLines_re = """(?m)^\s*$[\r\n]+""".r
  @Before def initialize() {
    dir.mkdir
  }

  def assertFile( filename: String ) {
    val useNewCompare = true
    val reffile = scala.io.Source.fromURL(getClass.getResource(filename))
    val refText = blankLines_re.replaceAllIn(reffile.mkString, "")
    reffile.close()
    val testfile = scala.io.Source.fromFile(
      dir.getPath + "/" + filename, "utf-8")
    val testText = blankLines_re.replaceAllIn(testfile.mkString, "")
    testfile.close()
    if (useNewCompare) {
      val comparitor = new TextComparitor()
      val testTextWithSubstitutions = comparitor.substituteTextIfPossible(refText, testText)
      assertResult(refText) { testTextWithSubstitutions }
    } else {
      assertResult(refText) { testText }
    }
  }

  def launchTester[M <: Module : ClassTag, T <: Tester[M]](b: String, t: M => T) {
    val ctor = implicitly[ClassTag[M]].runtimeClass.getConstructors.head
    chiselMainTest(Array[String]("--backend", b,
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
      () => Module(ctor.newInstance(this).asInstanceOf[M])) {t}
  }
  def launchCppTester[M <: Module : ClassTag, T <: Tester[M]](t: M => T) = launchTester("c", t)
  def launchVerilogTester[M <: Module : ClassTag, T <: Tester[M]](t: M => T) = launchTester("v", t)


  class BoolIO extends Bundle {
    val in  = Bool(INPUT)
    val out = Bool(OUTPUT)
  }

  class UIntIO extends Bundle {
    val in  = UInt(INPUT, 4)
    val out = UInt(OUTPUT, 4)
  }

  class DecoupledUIntIO extends Bundle {
    val in = Decoupled(UInt(width = 4)).flip
    val out = Decoupled(UInt(width = 4))
  }

  class EnableIO extends Bundle {
    val en  = Bool(INPUT)
    val in  = UInt(INPUT, 4)
    val out = UInt(OUTPUT, 4)
  }
}
