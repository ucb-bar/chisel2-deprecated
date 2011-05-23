package Chisel{

class intWrapper(x: Int) {
  val myVal = x;
  def ~(x: String): Lit = Lit(myVal, x(0), x.substring(1, x.length));
  def ~(x: Int): Lit = Lit(myVal, 'd', x.toString);
  //def d(x: Int): Lit = Lit(x, myVal);
  //def $(x: String): Lit = Lit(myVal, x(0), x.substring(1, x.length));
}

object ChiselUtil {
  implicit def intToIntWrapper(x: Int) = new intWrapper(x);
}

}
