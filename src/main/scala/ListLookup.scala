// author: jonathan bachrach
package Chisel {



class ListLookup(mapping: Array[(Lit, List[Node])], defaultVal: List[Node]) extends Node {
  val map = mapping;
  val default = defaultVal;
  val wires = defaultVal.map(a => new ListLookupRef(a));
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
    for ((w, e) <- wires zip defaultVal) {
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
          res = res + "    " + w.emitRef + "/*" + w.component + "*/" +  " = " + e.emitRef + ";\n";
      res = res + "  }\n";
    }
    res
  }
}
class ListLookupRef(defaultVal: Node =  Lit(0)) extends Delay {
  def lookup = inputs(0);
  def lookup_= (ll: ListLookup) = { inputs(0) = ll; }
  inputs += null;
  inferWidth = (m: Node) => defaultVal.width; // TODO: PROBABLY NOT RIGHT
  // override def toString: String = "W(" + name + ")"
  override def toString: String = name
  override def emitDef = "";
  override def emitDefLoC = "";
  override def emitDec: String = 
    "  reg[" + (width-1) + ":0] " + emitRef + ";\n";
  // override def emitDecC: String = 
  //   "  dat_t<" + width + "> " + emitRef + ";\n";
}

}
