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

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.algo.search._
import oscar.util._
import oscar.cp.search.BinaryFirstFailBranching


object Jackhammer {

  val spaceName = "space.prm"

  def dump(dir: String, mode: String) = {
    Params.buildingSpace = false
    mode match {
      case "space" => Params.dump_file(dir + "/" + spaceName, Params.space)
      case "all"   => process(Params.space,dir)
    }
  }

  def process(space: Space, dir: String) = {
    val cp = CPSolver()
    val sol = new ArrayBuffer[ArrayBuffer[Int]]

    var x = new ArrayBuffer[CPVarInt]
    var newspace = new Params.Space
    for(t <- space){
      x = cvt(t,cp,x) 
      newspace += t
    }
    cp.onSolution {
      sol += x.map(x => x.getValue);
    }
    cp.solve subjectTo {
    } search {
      new BinaryFirstFailBranching(x,_.randomValue)
    }
    val stats = cp.start()
    println(stats)

    for(d <- sol){
      var newDesign = new Params.Space
      for(t <- newspace){
        val p = new ValueParam(t._2.pname,d(t._2.index))
        p.gID = t._3
        newDesign += ((t._1,p,t._3))
      }
      writeDesign(dir,newDesign)
    }
  }

  def writeDesign(dir: String, design: Params.Space) = {
    val design_tag = formatDesign(design)
    val design_filename = "design"+design_tag+".prm"
    Params.dump_file(dir+"/"+design_filename, design)
  }

  def formatDesign(design: Params.Space) : String = {
    val str = new StringBuilder("_")
    for((mod,p,gID) <- design) {
      str ++= p.pname+gID+":"+p.init+"_"
    }
    if(str.length<20) {
      str.toString + MurmurHash3.stringHash(design.toString,1).toHexString 
    } else {
      str.substring(0,20) + MurmurHash3.stringHash(design.toString,1).toHexString 
    }
 }

  def load(dir: String, designName: String) = {
    Params.buildingSpace = false
    Params.design = Params.load_file(dir + "/" + designName)
  }



  def cvt(t: Tuple3[String,Param[Any],Int], cp: CPSolver, x: ArrayBuffer[CPVarInt]): ArrayBuffer[CPVarInt] = {
    t._2 match {
      case RangeParam(n,init,min,max)       => println(t._2.index);t._2.index = x.length; println(t._2.index);x += (CPVarInt(cp,min to max)); x
      case LessParam(n,init,min,par)        => t._2.index = x.length; x += (CPVarInt(cp,min to t._2.max)); cp.add(x(x.length-1) < x(par.index)); println("par index: " + par.index); x
      case LessEqParam(n,init,min,par)      => t._2.index = x.length; x += (CPVarInt(cp,min to t._2.max)); cp.add(x(x.length-1) <= x(par.index)); println("par index: " + par.index); x
      case GreaterParam(n,init,par,max)     => t._2.index = x.length; x += (CPVarInt(cp,t._2.min to max)); cp.add(x(x.length-1) > x(par.index)); println("par index: " + par.index); x
      case GreaterEqParam(n,init,par,max)   => t._2.index = x.length; x += (CPVarInt(cp,t._2.min to max)); cp.add(x(x.length-1) >= x(par.index)); println("par index: " + par.index); x
      case DivisorParam(n,init,min,max,par) => t._2.index = x.length; x += (CPVarInt(cp,min to max)); x += (CPVarInt(cp,min to par.max)); cp.add(x(x.length-2)*x(x.length-1)==x(par.index)); x
      case ValueParam(n,value)                   => t._2.index = x.length; x += (CPVarInt(cp,value.toString().toInt)); x
      case EnumParam(n,value, values)            => t._2.index = x.length; val a = values.map(a => a.toInt).toList.toSet; x += (CPVarInt(cp,a)); x
      case _                                   => throw new ParamInvalidException("Invalid param type!"); x
    }
  }

}


/*
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
      case IIRangeParam(init, min, max, func) => {
        var i = min;
        while(i <= max) { evals = i :: evals; i = param.step(i) }
        evals
      }
      case IIRangeParam(init, min, maxR, func) => {
        var i = min;
        while(i <= max) { evals = i :: evals; i = param.step(i) }
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
}
*/
