package Chisel
import Params._
import Node._
import Module._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.hashing._
import java.lang.reflect.{Type, ParameterizedType}
import scala.io.Source
import java.io._

object Jackhammer {
  val spaceName = "space.prm"

  def dump(dir: String, mode: String) = {
    mode match {
      case "space" => Params.dump_file(dir + "/" + spaceName, Params.space)
      case "all"   => { for(design <- getDesigns(Params.space)) writeDesign(dir, design) }
    }
  }

  def load(dir: String, designName: String) = {
    Params.buildingSpace = false
    Params.design = Params.load_file(dir + "/" + designName)
  }

  def writeDesign(dir: String, design: Params.Space) = {
    val design_tag = JackFormatter.formatDesign(design)+MurmurHash3.stringHash(design.toString,1).toHexString
    val design_filename = "design"+design_tag+".prm"
    Params.dump_file(dir+"/"+design_filename, design)
  }
    
  def getDesigns(myspace: Params.Space) : List[Params.Space]= {
    var space = myspace
    var designs:List[Params.Space] = Nil
    for((mname,mhash) <- space){
      for((pname, pparam) <- mhash){
        var evals = getEvals(pparam)
        var new_designs: List[Params.Space] = Nil
        for(e <- evals) {  
          val d_temp = updateList(mname,pname,e,designs)
          new_designs = d_temp ::: new_designs
        }
        designs = new_designs
      }
    }
    designs
  }
  
  def getEvals(param: Param[Any]) : List[Any] = {
    var evals:List[Any] = Nil
    param match {
      case ValueParam(init) => {
        init :: evals
      }
      case RangeParam(init, min, max, step, log) => {
        var i = min;
        if(log == true){
          while(i <= max) { evals = i :: evals; i = i*step }
        } else {
          while(i <= max) { evals = i :: evals; i = i+step }
        }
        evals
      }
      case _ => { evals }
    }
  }

  def updateList[T<:Any](mname: String, pname: String, value: T, list: List[Params.Space]): List[Params.Space] = {
    var myList:List[Params.Space] = Nil
    if(list.isEmpty){
      val hash = new Params.Space
      myList = addParam(hash, mname, pname, new ValueParam(value)) :: myList
    } else {
      for(x <- list){
        myList = addParam(x, mname, pname, new ValueParam(value)) :: myList
      }
    }
    myList
  }

  def addParam(myspace: Params.Space, mname: String, pname: String, param: Param[Any]): Params.Space = {
    var space = new Params.Space
    if(!space.contains(mname)) {
      space(mname) = new HashMap[String,Param[Any]]
    }
    for((m,h) <- myspace){
      for((n,p) <- h){
        space(m)(n) = p
      }
    }
    space(mname)(pname) = param
    space
  }
}

object JackFormatter {
  def formatDesign(design: Params.Space) : String = {
    val str = new StringBuilder("_")
    for((mod,param) <- design) {
      for((name,value) <- param) {
        str ++= name+value.value+"_"
      }
    }
    if(str.length<20) str.toString else str.substring(0,20)
  }
}
