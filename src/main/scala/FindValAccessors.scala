package Chisel

import scala.collection.mutable.ArrayBuffer
import java.lang.reflect.Modifier._
import java.lang.reflect.Method

object FindValAccessors {

  private def getValNames(c: Class[_]): ArrayBuffer[String] = {
    val valNames = new ArrayBuffer[String]()
    for (v <- c.getDeclaredFields) {
      v.setAccessible(true)
      valNames += v.getName
    }
    val sc = c.getSuperclass
    if (sc != null) { valNames ++= getValNames(sc) }
    valNames
  }

  def apply(c: Class[_]): ArrayBuffer[Method] = {
    val valNames = getValNames(c)
    val valAccessors = new ArrayBuffer[Method]()
    for (m <- c.getDeclaredMethods) {
      val ml = ArrayBuffer(m)
      val name = m.getName()
      val types = m.getParameterTypes()
      if (types.length == 0 && valNames.contains(name)) {
        valAccessors ++= ml
      }
    }
    valAccessors
  }

}
