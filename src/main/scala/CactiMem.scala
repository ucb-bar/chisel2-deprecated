package Chisel {
import Component._;

object CactiMem {
  def apply(depth: Int): CactiMem = {
    new CactiMem(depth);
  }
}

class CactiMem(numWords: Int) extends BlackBox {
  val io = new bundle_t();
  var size = 0;
  var id = 0;
  var num_r_port = 0;
  var num_w_port = 0;
  var num_rw_port = 0;
  override def name_it() = {name = moduleName;
			    className = moduleName;}
  def add_r_port(addr: int_t): int_t = {
    id += 1;
    num_r_port += 1;
    val ce_port = int_t(1, 'input);
    val oeb_port = int_t(1, 'input);
    val csb_port = int_t(1, 'input);
    val a_port = int_t('input);
    val r_port = int_t(size, 'output);
    ce_port.setName("CE" + id);
    oeb_port.setName("OEB" + id);
    csb_port.setName("CSB" + id);
    a_port.setName("A" + id);
    r_port.setName("I" + id);
    a_port := addr;
    io += ce_port;
    io += oeb_port;
    io += csb_port;
    io += a_port;
    io += r_port;
    r_port
  }
  def add_w_port(addr: int_t): int_t = {
    id += 1;
    num_w_port += 1;
    val ce_port = int_t(1, 'input);
    val web_port = int_t(1, 'input);
    val csb_port = int_t(1, 'input);
    val a_port = int_t('input);
    val w_port = int_t(size, 'input);
    ce_port.setName("CE" + id);
    web_port.setName("WEB" + id);
    csb_port.setName("CSB" + id);
    a_port.setName("A" + id);
    w_port.setName("W" + id);
    a_port := addr;
    io += ce_port;
    io += web_port;
    io += csb_port;
    io += a_port;
    io += w_port;
    w_port
  }
  def setSize(s: Int) = size = s;
  override def doCompileV(out: java.io.FileWriter, depth: Int): Unit = {
    val base_name = ensure_dir(targetVerilogRootDir + "/" + targetDir);
    val conf = new java.io.FileWriter(base_name + name + ".conf");
    conf.write("conf:\n");
    conf.write("  baseName:                " + name + "\n");
    conf.write("  wordLength:              " + size + "\n");
    conf.write("  numWords:                " + numWords + "\n");
    conf.write("  numRWPorts:              " + num_rw_port + "\n");
    conf.write("  numRPorts:               " + num_r_port + "\n");
    conf.write("  numWPorts:               " + num_w_port + "\n");
    conf.write("  technology:              65\n");
    conf.write("  opCond:                  Typical\n");
    conf.write("  debug:                   False\n");
    conf.write("  noBM:                    True\n");
    conf.close();
    super.doCompileV(out, depth);
  }
}

}

