package Chisel

class FPGABackend extends VerilogBackend
{
  def isMultiWrite(m: Mem[_]) = m.writes.size > 1
  def writen(m: MemWrite) = if (isMultiWrite(m.mem)) m.mem.writes.indexOf(m) else 0
  def writeMap(m: Mem[_], exclude: Int = -1) = {
    if (isMultiWrite(m))
      (0 until m.writes.size).filterNot(_ == exclude).map(emitRef(m) + "_" + _)
    else
      Seq(emitRef(m))
  }

  override def emitDec(node: Node): String = {
    node match {
      case m: Mem[_] =>
        assert(m.isInline)
        "  reg [" + (m.width-1) + ":0] " + writeMap(m).map(_ + " [" + (m.n-1) + ":0]").reduceLeft(_+", "+_) + ";\n"

      case _ =>
        super.emitDec(node)
    }
  }

  override def emitReg(node: Node): String = node match {
    case m: MemWrite => ""
    case _ => super.emitReg(node)
  }

  override def emitDef(node: Node): String = {
    node match {
      case m: MemRead =>
        "  assign " + emitTmp(node) + " = " + writeMap(m.mem).map(_ + "[" + emitRef(m.addr) + "]").reduceLeft(_+" ^ "+_) + ";\n"

      case m: MemWrite =>
        // check if byte-wide write enable can be used
        def litOK(x: Node) = x.isLit && (0 until x.width).forall(i => x.litOf.value.testBit(i) == x.litOf.value.testBit(i/8*8))
        def extractOK(x: Node) = x.isInstanceOf[Extract] && x.inputs.length == 3 && x.inputs(2).isLit && x.inputs(2).litOf.value % 8 == 0 && x.inputs(1).isLit && (x.inputs(1).litOf.value+1) % 8 == 0 && useByteMask(x.inputs(0))
        def fillOK(x: Node) = x.isInstanceOf[Fill] && (x.inputs(1).litOf.value % 8 == 0 && x.inputs(0).width == 1 || useByteMask(x.inputs(0)))
        def catOK(x: Node) = x.isInstanceOf[Cat] && x.inputs.forall(i => useByteMask(i))
        def useByteMask(x: Node): Boolean = extractOK(x) || litOK(x) || fillOK(x) || catOK(x) || x.isInstanceOf[Bits] && x.inputs.length == 1 && useByteMask(x.inputs(0))

        val me = writen(m)
        val mw = isMultiWrite(m.mem)
        val meStr = emitRef(m.mem) + (if (mw) "_" + me else "")
        val i = "i" + emitTmp(m)
        (if (mw) "  wire [" + (m.mem.width-1) + ":0] " + emitRef(m.mem) + "_w" + me + " = " + writeMap(m.mem, me).map(_ + "[" + emitRef(m.addr) + "]").reduceLeft(_+" ^ "+_) + ";\n" else "") +
        (if (m.isMasked) {
          val bm = m.mem.width % 8 == 0 && useByteMask(m.wmask)
          val max = if (bm) m.mem.width/8 else m.mem.width
          val maskIdx = if(bm) i+"*8" else i
          val dataIdx = if (bm) i+"*8+7:"+i+"*8" else i
          "  generate\n" +
          "    genvar " + i + ";\n" +
          "    for (" + i + " = 0; " + i + " < " + max + "; " + i + " = " + i + " + 1) begin: f" + emitTmp(m) + "\n" +
          "      always @(posedge clk)\n" +
          "        if (" + emitRef(m.cond) + " && " + emitRef(m.wmask) + "["+maskIdx+"])\n" +
          "          " + meStr + "["+emitRef(m.addr)+"]["+dataIdx+"] <= " + emitRef(m.data) + "["+dataIdx+"]" + (if (mw) " ^ " + emitRef(m.mem) + "_w" + me + "["+dataIdx+"]" else "") + ";\n" +
          "    end\n" +
          "  endgenerate\n"
        } else {
          "  always @(posedge clk)\n" +
          "    if (" + emitRef(m.cond) + ")\n" +
          "      " + meStr + "["+emitRef(m.addr)+"] <= " + emitRef(m.data) + (if (mw) " ^ " + emitRef(m.mem) + "_w" + me else "") + ";\n"
        })

      case _ =>
        super.emitDef(node)
    }
  }
}
