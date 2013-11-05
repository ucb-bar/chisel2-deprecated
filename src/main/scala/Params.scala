package Chisel

import Node._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

//import com.codahale.jerkson.Json._
import java.lang.reflect.{Type, ParameterizedType}
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.`type`.TypeReference;

import scala.io.Source
import java.io._

//>Params.scala: Implementation of parameter framework. Defines case class
  //containers for parameter types. Params object is what actually stores
  //the data structures of parameters, whether they are generated from a Chisel
  //design, or read from a json file

//abstract class Param[+T] {
//  def value: T
//
//  def register(comp: Module, pname: String) = {
//    Params.register(comp, pname, this).asInstanceOf[T]
//  }
//}

abstract class Param[+T] {
  def value: T

  def register(comp: Module, pname: String) = {
    Params.register(comp, pname, this).asInstanceOf[T]
  }
}

case class ValueParam(override val value: Any) extends Param[Any]

case class RangeParam(override val value: Int, min: Int, max: Int, step: Int = 1, log: Boolean = false) extends Param[Int]

case class EnumParam(override val value: String, values: List[String]) extends Param[String]

object Params {
  type Space = HashMap[String,HashMap[String,Param[Any]]]
  type VSpace = HashMap[String,HashMap[String,ValueParam]]

  var space = new Space
  var vspace = new VSpace

  var buildingSpace = true

  def register(comp: Module, pname: String, p: Param[Any]) = {
    val cname = comp.getClass.getName
    if(buildingSpace) {
      // TODO: error on duplicate key
      if(!space.contains(cname)) {
        space(cname) = new HashMap[String,Param[Any]]
      }
      space(cname)(pname) = p
      p.value
    } else {
      // TODO: error on key not found
      vspace(cname)(pname).value
    }
  }
  
  def load(filename: String) = {
    buildingSpace = false
    //val json = io.Source.fromFile(filename).mkString
    //vspace = JacksonWrapper.deserialize[VSpace](json)
    vspace = HashMap("sha3.Sha3Accel" -> HashMap("W" -> new ValueParam(64)))
    //println("Loaded: " + json + "\nfrom " + filename)
  }

  def dump(filename: String) = {
    val json = JacksonWrapper.serialize(space)
    val writer = new PrintWriter(new File(filename))
    println("Dumping to " + filename + ":\n" + json)
    writer.write(json)
    writer.close()
  }
}

object JacksonWrapper {
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def serialize(value: Any): String = {
    val writer = new StringWriter()
    mapper.writeValue(writer, value)
    writer.toString
  }

  def deserialize[T: Manifest](value: String) : T =
    mapper.readValue(value, typeReference[T])

  private [this] def typeReference[T: Manifest] = new TypeReference[T] {
    override def getType = typeFromManifest(manifest[T])
  }

  private [this] def typeFromManifest(m: Manifest[_]): Type = {
    if (m.typeArguments.isEmpty) { m.runtimeClass }
    else new ParameterizedType {
      def getRawType = m.runtimeClass
      def getActualTypeArguments = m.typeArguments.map(typeFromManifest).toArray
      def getOwnerType = null
    }
  }
}
