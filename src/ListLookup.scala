// author: jonathan bachrach
package Chisel {

import Node._
import scala.collection.mutable.ArrayBuffer

object ListLookup {
  def apply[T <: Bits](addr: Bits, default: List[T], mapping: Array[(Bits, List[T])]): List[T] = {
    val defaultNode = ListNode(default)
    val mappingNode = mapping.map(x => MapNode(x))
    val ll = new ListLookup[T]()
    ll.initOf("", widthOf(1), List(addr, defaultNode) ++ mappingNode)
    ll.wires = default.map(x => ListLookupRef(x, ll))
    // TODO: GENERALIZE AND SHARE THIS
    (default zip ll.wires).map{case(x, xRef) => {
      val res = x match {
	case bool: Bool => Bool(OUTPUT);
	case ufix: UFix => UFix(OUTPUT);
	case fix: Fix => Fix(OUTPUT);
	case bits: Bits => Bits(OUTPUT);
	case any => Bits(OUTPUT);
      }
      res.setIsCellIO;
      xRef.nameHolder = res;
      res.inputs += xRef
      res.asInstanceOf[T]
    }}
    
  }
}

class ListLookup[T <: Bits] extends Node {
  var wires: List[ListLookupRef[T]] = null
  
  def addr = inputs(0)

  def defaultWires = inputs(1).inputs

  def map = {
    val mapArray = inputs.slice(2, inputs.length)
    val res = ArrayBuffer[(Node, ArrayBuffer[Node])]()
    for(elm <- mapArray)
      res += (elm.asInstanceOf[MapNode].addr -> elm.asInstanceOf[MapNode].data)
    res
  }

  override def toString: String = "LISTLOOKUP(" + inputs(0) + ")";

  override def isByValue: Boolean = false;
  override def emitDef: String = {
    var res = 
      "  always @(*) begin\n" +
      //"    " + emitRef + " = " + inputs(1).emitRef + ";\n" +
      "    casez (" + inputs(0).emitRef + ")" + "\n";
    
    for ((addr, data) <- map) {
      res = res + "      " + addr.emitRef + " : begin\n";
      for ((w, e) <- wires zip data) 
	if(w.component != null)
          res = res + "        " + w.emitRef + " = " + e.emitRef + ";\n";
      res = res + "      end\n" 
    }
    res = res + "      default: begin\n"
    for ((w, e) <- wires zip defaultWires) {
      if(w.component != null)
	res = res + "        " + w.emitRef + " = " + e.emitRef + ";\n";
    }
    res = res + "      end\n";
    res = res + 
      "    endcase\n" +
      "  end\n";
    res
  }

  override def emitDefLoC: String = {
    var res = "";
    var isFirst = true;
    for (w <- wires)
      if(w.component != null) // TODO: WHY IS COMPONENT EVER NULL?
        res = res + "  dat_t<" + w.width + "> " + w.emitRef + ";\n";
    for ((addr, data) <- map) {
      res = res + "  " + (if (isFirst) { isFirst = false; "" } else "else ");
      res = res + "if ((" + addr.emitRef + " == " + inputs(0).emitRef + ").to_bool()) {\n";
      for ((w, e) <- wires zip data)
	if(w.component != null)
          res = res + "    " + w.emitRef + " = " + e.emitRef + ";\n";
      res = res + "  }\n";
    }
    res = res + "  else {\n";
    for ((w, e) <- wires zip defaultWires)
      if(w.component != null)
        res = res + "    " + w.emitRef + " = " + e.emitRef + ";\n";
    res = res + "  }\n";
    res
  }

}

object ListLookupRef {
  def apply[T <: Bits](x: T, ll: ListLookup[T]): ListLookupRef[T] = {
    val res = new ListLookupRef[T]()
    res.init("", widthOf(1), ll, x)
    res
  }
}

class ListLookupRef[T <: Bits]() extends Node {
  override def isInObject = true;

  // override def toString: String = "W(" + name + ")"
  override def toString: String = name
  override def emitDef = "";
  override def emitDefLoC = "";
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";
  // override def emitDecC: String = 
  //   "  dat_t<" + width + "> " + emitRef + ";\n";
}

object ListNode {
  def apply[T <: Data](nodes: List[T]): ListNode = {
    val res = new ListNode()
    res.init("", widthOf(0), nodes: _*)
    res
  }
}

class ListNode extends Node {
  override def emitDefLoC: String = ""
  override def emitDef: String = ""
  override def emitDec: String = ""
  override def emitDecC: String = ""
}

object MapNode {
  def apply[T <: Bits](map: (Bits, List[T])): MapNode = {
    val res = new MapNode()
    res.initOf("", widthOf(0), List(map._1) ++ map._2)
    res
  }
}

class MapNode extends Node {
  override def emitDefLoC: String = ""
  override def emitDef: String = ""
  override def emitDec: String = ""
  override def emitDecC: String = ""
  def addr = inputs(0)
  def data = inputs.slice(1, inputs.length)
}

/*
// TODO: PARAMETERIZE THIS
class ListLookup(mapping: Array[(Node, List[Node])], defaultVal: List[Node]) extends Node {
  val map = mapping.map{case(n, l_n) => (n, l_n.toBuffer)}.toBuffer;
  val default = defaultVal.toBuffer;
  val wires = defaultVal.map(a => new ListLookupRef(a));
  override def getNode() = {
    for(((addr, data), i) <- map zip map.indices){
      map(i) = ((addr.getNode(), data));
      for(i <- 0 until data.length){
	data(i) = data(i).getNode()
      }
    }
    for(i <- 0 until default.length){
      default(i) = default(i).getNode();
    }
    val addr = inputs(0);
    inputs.clear();
    inputs += addr;
    for (e <- default)
      inputs += e;
    for ((a, data) <- map){
      for (e <- data){
        inputs += e;
      }
    }
    this
  }
  override def toString: String = "LIST-LOOKUP(" + inputs(0) + ")";
  override def emitDef: String = {
    var res = 
      "  always @(*) begin\n" +
      //"    " + emitRef + " = " + inputs(1).emitRef + ";\n" +
      "    casez (" + inputs(0).emitRef + ")" + "\n";
    
    for ((addr, data) <- map) {
      res = res + "      " + addr.emitRef + " : begin\n";
      for ((w, e) <- wires zip data) 
	if(w.component != null)
          res = res + "        " + w.emitRef + " = " + e.emitRef + ";\n";
      res = res + "      end\n" 
    }
    res = res + "      default: begin\n"
    for ((w, e) <- wires zip default) {
      if(w.component != null)
	res = res + "        " + w.emitRef + " = " + e.emitRef + ";\n";
    }
    res = res + "      end\n";
    res = res + 
      "    endcase\n" +
      "  end\n";
    res
  }
  override def emitDefLoC: String = {
    var res = "";
    var isFirst = true;
    for (w <- wires)
      if(w.component != null) // TODO: WHY IS COMPONENT EVER NULL?
        res = res + "  dat_t<" + w.width + "> " + w.emitRef + ";\n";
    for ((addr, data) <- map) {
      res = res + "  " + (if (isFirst) { isFirst = false; "" } else "else ");
      res = res + "if ((" + addr.emitRef + " == " + inputs(0).emitRef + ").to_bool()) {\n";
      for ((w, e) <- wires zip data)
	if(w.component != null)
          res = res + "    " + w.emitRef + " = " + e.emitRef + ";\n";
      res = res + "  }\n";
    }
    res = res + "  else {\n";
    for ((w, e) <- wires zip default)
      if(w.component != null)
        res = res + "    " + w.emitRef + " = " + e.emitRef + ";\n";
    res = res + "  }\n";
    res
  }
}
class ListLookupRef(defaultVal: Node =  Literal(0)) extends Node {
  override def isInObject = true;
  override def getNode() = {
    lookup.getNode()
    this
  }
  def lookup = inputs(0);
  def lookup_= (ll: ListLookup) = { inputs(0) = ll; }
  inputs += null;
  inferWidth = (m: Node) => defaultVal.getWidth; // TODO: PROBABLY NOT RIGHT
  // override def toString: String = "W(" + name + ")"
  override def toString: String = name
  override def emitDef = "";
  override def emitDefLoC = "";
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";
  // override def emitDecC: String = 
  //   "  dat_t<" + width + "> " + emitRef + ";\n";
}
* */

}
