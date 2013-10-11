package Chisel

import Node._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import com.codahale.jerkson.Json._
import scala.io.Source
import java.io._

abstract class Param[+T] {
  def value: T

  def register(comp: Component, pname: String) = {
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

  def register(comp: Component, pname: String, p: Param[Any]) = {
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
    val json = io.Source.fromFile(filename).mkString
    //println("Loaded: " + json + "\nfrom " + filename)
    vspace = parse[VSpace](json)
  }

  def dump(filename: String) = {
    val json = generate(space)
    val writer = new PrintWriter(new File(filename))
    //println("Dumping to " + filename + ":\n" + json)
    writer.write(json)
    writer.close()
  }
}

