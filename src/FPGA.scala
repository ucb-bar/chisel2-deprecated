package Chisel

class FPGABackend extends VerilogBackend
{
  override def emitDef(node: Node): String = {
    node match {
      case m: MemWrite[_] =>
        assert(Component.isInlineMem)
        // check if byte-wide write enable can be used
        def litOK(x: Node) = x.isLit && (0 until x.width).forall(i => x.litOf.value.testBit(i) == x.litOf.value.testBit(i/8*8))
        def extractOK(x: Node) = x.isInstanceOf[Extract] && x.inputs.length == 3 && x.inputs(2).isLit && x.inputs(2).litOf.value % 8 == 0 && x.inputs(1).isLit && (x.inputs(1).litOf.value+1) % 8 == 0 && nodeOK(x.inputs(0))
        def fillOK(x: Node) = x.isInstanceOf[Fill] && (x.inputs(1).litOf.value % 8 == 0 && x.inputs(0).width == 1 || nodeOK(x.inputs(0)))
        def catOK(x: Node) = x.isInstanceOf[Cat] && x.inputs.forall(i => nodeOK(i))
        def nodeOK(x: Node): Boolean = extractOK(x) || litOK(x) || fillOK(x) || catOK(x) || x.isInstanceOf[Bits] && x.inputs.length == 1 && nodeOK(x.inputs(0))

        if (m.used && Component.isInlineMem && m.isMasked && nodeOK(m.wmask)) {
          val i = "i" + emitTmp(m)
          "  generate\n" +
          "    genvar " + i + ";\n" +
          "    for (" + i + " = 0; " + i + " < " + m.mem.width/8 + "; " + i + " = " + i + " + 1) begin: f" + emitTmp(m) + "\n" +
          "      always @(posedge clk)\n" +
          "        if (" + emitRef(m.cond) + " && " + emitRef(m.wmask) + "[" + i + "*8])\n" +
          "          " + emitRef(m.mem) + "[" + emitRef(m.addr) + "][("+i+"+1)*8-1:"+i+"*8] <= " + emitRef(m.data) + "[("+i+"+1)*8-1:"+i+"*8];\n" +
          "    end\n" +
          "  endgenerate\n"
        } else
          super.emitDef(node)

      case _ =>
        super.emitDef(node)
    }
  }
}
