package com.simplifide.base2

import com.simplifide.base.verilog.parse.base.VerilogParserTop
import com.simplifide.base.sourcefile.antlr.ParseDescriptor
import com.simplifide.generate.util.FileOps
import java.io.{FileReader, StringReader, File}
import project.{SuiteGenerator, VerilogModuleReader}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 3/1/12
 * Time: 8:28 PM
 * To change this template use File | Settings | File Templates.
 */

class TestVerilog {

}

object TestVerilog {

  val linux_base = "/home/andy/simplifide_base/Generator/output2"
  val windows_base = "C:\\designs2\\Generator2\\output2"
  val files = List("testcase.v","sub.v")

  val base = windows_base


  val location = "C:\\WorkingProjects\\project_ddr\\projects\\sa\\fpga\\source\\a80_v6\\a80_v6_procif.v"
  val turbo = "C:\\LTE_Latest\\coregen_v5\\tcc_decoder_3gpplte_v2_0\\tcc_decoder_3gpplte_v2_0.v"

  
  def main(args:Array[String]) = {
    
    val locations = files.map(x => new File(base,x))
    
    val prefix = "com.simplifide.test"
    val suiteGenerator = new SuiteGenerator.Single(prefix,locations)
    val suite = suiteGenerator.createSuite
    suite.create(new File(base))

    System.out.println("Here")

  }
}
