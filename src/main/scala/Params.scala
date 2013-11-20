/* Unfinished. Has 3 basic parameters available */
package Chisel

import Node._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import java.lang.reflect.{Type, ParameterizedType}

import scala.io.Source
import java.io._

//>Params.scala: Implementation of parameter framework. Defines case class
  //containers for parameter types. Params object is what actually stores
  //the data structures of parameters, whether they are generated from a Chisel
  //design, or read from a json file

abstract class Param[+T] {
  def value: T

  def register(module: Module, pname: String) = {
    Params.register(module, pname, this).asInstanceOf[T]
  }
}

case class ValueParam(override val value: Any) extends Param[Any]

case class RangeParam(override val value: Int, min: Int, max: Int, step: Int = 1, log: Boolean = false) extends Param[Int]

case class EnumParam(override val value: String, values: List[String]) extends Param[String]

object Params {
  type Space = HashMap[String,HashMap[String,Param[Any]]]

  var space = new Space
  var design = new Space

  var buildingSpace = true

  def register(module: Module, pname: String, p: Param[Any]) = {
    val mname = module.getClass.getName
    if(buildingSpace) {
      // TODO: error on duplicate key
      if(!space.contains(mname)) {
        space(mname) = new HashMap[String,Param[Any]]
      }
      space(mname)(pname) = p
      p.value
    } else {
      // TODO: error on key not found
      design(mname)(pname).value
    }
  }

  def load_file(filename: String) : Params.Space = {
    //val file = io.Source.fromFile(filename).mkString
    var lines = io.Source.fromFile(filename).getLines
    var space = new Params.Space
    while(lines.hasNext) {
      val line = lines.next()
      println("Loaded: " + line + "\nfrom " + filename)
      Params.deserialize(line,space)
    }
    space
  }
   
  def dump_file(filename: String, design: Params.Space) = {
    val string = Params.serialize(design)
    val writer = new PrintWriter(new File(filename))
    println("Dumping to " + filename + ":\n" + string)
    writer.write(string)
    writer.close()
  }

  def load(filename: String) = {
    buildingSpace = false
    design = load_file(filename)
  }

  def dump(filename: String) = {
    dump_file(filename, space)
  }
 
  def serialize[T<:Param[Any]](myhashmap: HashMap[String,HashMap[String,T]]) : String = {
    var hashmap = myhashmap
    var string = ""
    while(!hashmap.isEmpty){
      var elem = hashmap.head
      hashmap = hashmap.tail
      var mname = elem._1
      var mhash = elem._2
      while(!mhash.isEmpty){
        var elem2 = mhash.head
        mhash = mhash.tail
        string = string + mname + "," + elem2._1 + "," + toStringParam(elem2._2)
        if(!mhash.isEmpty) {
          string = string + " "
        }
      }
      if(!hashmap.isEmpty) {
        string = string + "\n"
      }
    }
    string
  }

  def deserialize(string: String, myhashmap: HashMap[String,HashMap[String,Param[Any]]]) = {
    val args = string.split(",")
    val mname = args(0)
    val pname = args(1)
    val ptype = args(2)
    val param = ptype match {
      case "range" => new RangeParam(args(3).toInt, args(4).toInt, args(5).toInt, args(6).toInt, args(7).toBoolean)
      case "value" => new ValueParam(args(3).toInt)
      case _       => new ValueParam("error")
    }
    if(!myhashmap.contains(mname)) {
      myhashmap(mname) = new HashMap[String,Param[Any]]
    }
    myhashmap(mname)(pname) = param
  }
    
  def toStringParam(param: Param[Any]) = {
    param match {
      //case EnumParam(init, list) =>
        //"(range," + init + "," + list + ")"
      case RangeParam(init, min, max, step, log) =>
        "range," + init + "," + min + "," + max + "," + step + "," + log
      case ValueParam(init) =>
        "value," + init
      case _ =>
        "uhoh "
    }
  }
    
}
