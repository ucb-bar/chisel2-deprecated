package Chisel {

class intWrapper(x: Int) {
  val myVal = x;
  def ~(x: String) = Lit(myVal, x(0), x.substring(1, x.length)){Fix()};
  def ~(x: Int) = Lit(myVal, 'd', x.toString){Fix()};
  //def d(x: Int): Lit = Lit(x, myVal);
  //def $(x: String): Lit = Lit(myVal, x(0), x.substring(1, x.length));
  def ~(x: Symbol) = Lit(myVal, x.name(0), x.name.substring(1, x.name.length)){Fix()}
}

class symbolWrapper(x: Symbol) {
  val myVal = x;
  //def unary_#() = Lit(myVal.name.substring(1, x.name.length).toInt);
  def unary_~() = Lit(myVal.name.substring(1, x.name.length).toInt){Fix()};
  //def unary_$() = Lit(myVal.name.substring(1, x.name.length).toInt);
}


class boolWrapper(x: Boolean) {
  val myVal = x;
  def unary_~() = {
    val res = Bool('output);
    if(myVal)
      res := Lit(true)
    else
      res := Lit(false)
    res
  }
}

object LitConv {
  implicit def intToIntWrapper(x: Int) = new intWrapper(x);
  implicit def symbolToSymbolWrapper(x: Symbol) = new symbolWrapper(x);
  implicit def boolToBoolWrapper(x: Boolean) = new boolWrapper(x);
}

object Fab {
  def apply[T: Manifest](inputs: Any*): T = {
    var t: List[Class[ _ ]] = Nil;
    for(i <- inputs.toList)
      i match {
	case x: Int => t = classOf[Int] :: t;
	case y: Double => t = classOf[Double] :: t;
	case z: List[String] => t = classOf[List[String]] :: t;
	case default => ;
      }
    val clazz = manifest[T].erasure.asInstanceOf[Class[T]];
    clazz.newInstance
    //val constructor = clazz.getConstructor(t: _*);
    //constructor.newInstance(inputs.map(t => t.asInstanceOf[AnyRef]): _*);
  }
}

}
