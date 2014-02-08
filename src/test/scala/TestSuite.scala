import org.scalatest.junit.AssertionsForJUnit
import org.junit.Before
import java.io.File
import scala.reflect.ClassTag
import scala.collection.JavaConversions
import Chisel._

class TestSuite extends AssertionsForJUnit {

  val dir = new File("test-outputs")

  @Before def initialize() {
    dir.mkdir
  }

  def assertFile( filename: String ) {
    val reffile = scala.io.Source.fromURL(getClass.getResource(filename))
    val content = reffile.mkString
    reffile.close()
    val source = scala.io.Source.fromFile(
      dir.getPath + "/" + filename, "utf-8")
    val lines = source.mkString
    source.close()
    assert(lines === content)
  }

  def launchTester[M <: Module : ClassTag, T <: Tester[M]](b: String, t: M => T) {
    val ctor = implicitly[ClassTag[M]].runtimeClass.getConstructors.head
    chiselMainTest(Array[String]("--backend", b,
      "--targetDir", dir.getPath.toString(), "--genHarness", "--compile", "--test"),
      () => Module(ctor.newInstance(this).asInstanceOf[M])) {t}
  }
  def launchCppTester[M <: Module : ClassTag, T <: Tester[M]](t: M => T) = launchTester("c", t)
  def launchVerilogTester[M <: Module : ClassTag, T <: Tester[M]](t: M => T) = launchTester("v", t)
}
