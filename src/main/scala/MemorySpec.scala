// MemorySpec -- Memory Generator Specification and Query
// Author: Brian Richards

package Mem {

import Chisel._;
import Component._;
import java.io.File;

class MemorySpecRAMV extends MemorySpec {

}

class MemorySpecUCBSC extends MemorySpec {
  override def emitGeneratedMasterName = {
    var res = master_name + "_" + word_length + "x" + depth;
    if (num_r_ports == 1 && num_w_ports == 1 && num_rw_ports == 0) {
      res += "_2P"
    } else if (num_r_ports == 0 && num_w_ports == 0 && num_rw_ports == 2) {
      res += "_2P"
    } else if (num_r_ports == 0 && num_w_ports == 0 && num_rw_ports == 1) {
      res += "_1P"
    } else {
      res + "_not_supported"
    }
    res
  }

  override def emitDataIn(ind: Int)  = "I"+ind;
  override def emitDataOut(ind: Int) = "O"+ ind;
  override def emitClkEn(ind: Int)   = "CE"+ind;
  override def emitAddr(ind: Int)    = "A"+ind;
  override def emitCS(ind: Int)      = "CSB"+ind;
  override def emitWE(ind: Int)      = "WEB"+ind;
  override def emitOE(ind: Int)      = "OEB"+ind;

  override def emitBuild = {
    val res =
      "conf:\n" +
      "  baseName:   " + master_name + "\n" +
      "  wordLength: " + word_length + "\n" +
      "  numWords:   " + depth + "\n" +
      "  numRWPorts: " + num_rw_ports + "\n" +
      "  numRPorts:  " + num_r_ports + "\n" +
      "  numWPorts:  " + num_w_ports + "\n" +
      "  technology: " + technology + "\n" +
      "  opCond:     " + op_cond + "\n" +
      "  debug:      " + (if (debug) "True" else "False") + "\n" +
      "  noBM:       " + (if (no_bit_mask) "True" else "False") + "\n";
    val base_name = ensure_dir(targetVerilogRootDir + "/" + targetDir);
    val conf = new java.io.FileWriter(base_name + master_name + ".conf");
    conf.write(res);
    conf.close;
    res;
  }
}

abstract class MemorySpec {
  var master_name = "SRAM";
  var depth = 32;
  val min_depth = 4;
  val max_depth = 1024;
  var word_length = 32;
  val min_word_length = 8;
  val max_word_length = 72;
  var mux_factor = 1;
  val min_mux_factor = 1;
  val max_mux_factor = 1;
  var num_rw_ports = 0;
  var num_r_ports = 1;
  var num_w_ports = 1;
  val technology = "65";
  val op_cond = "Typical";
  val debug = false;
  var no_bit_mask = true;
  var data_bits_per_mask_bit = 1;
  val active_high_ctrl = 0;

  def emitDataIn(ind: Int)  = "I"+ind;
  def emitDataOut(ind: Int) = "O"+ ind;
  def emitClkEn(ind: Int)   = "CE"+ind;
  def emitAddr(ind: Int)    = "A"+ind;
  def emitCS(ind: Int)      = "CSB"+ind;
  def emitWE(ind: Int)      = "WEB"+ind;
  def emitOE(ind: Int)      = "OEB"+ind;
  def emitWBM(ind: Int)     = "WBM"+ind;

  def set_master_name(name: String) = {
    master_name = name;
  }
  def set_dimensions(req_depth: Int, req_word_length: Int, req_mux_factor: Int = 1) = {
    depth = req_depth;
    word_length = req_word_length;
    mux_factor = req_mux_factor;
  }
  def set_port_count(read: Int, write: Int, rw: Int) = {
    num_rw_ports = rw;
    num_r_ports = read;
    num_w_ports = write;
    var port_pattern = read * 100 + write * 10 + rw;
    if (port_pattern != 110 && port_pattern != 001 && port_pattern != 002) {
      println("[warning] MemIP can only have 1RW, 2RW, or 1R+1W ports.");
    }
  }

  def ensure_dir(dir: String) = {
    val d = dir + (if (dir == "" || dir(dir.length-1) == '/') "" else "/");
    new File(d).mkdirs();
    d
  }

  def estimate_width: Float = 0;
  def estimate_height: Float = 0;
  def estimate_area: Float = { estimate_width * estimate_height; }

  def emitGeneratedMasterName = master_name;
  def emitBuild: String = "";
  def emitVerilogRef: String = "";
  def emitVerilogParameters: String = "";
}

}
