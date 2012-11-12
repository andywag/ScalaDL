package com.simplifide.base2.project

import com.simplifide.base2.project.VerilogModule.Impl
import com.simplifide.base.sourcefile.antlr.ParseDescriptor
import java.io.{File, FileReader}
import com.simplifide.base.verilog.parse.base.VerilogParserTop
import com.simplifide.base.core.instance.Entity
import collection.mutable.ListBuffer

/**
 *
 */

trait VerilogModuleReader {

  val location:File
  val suite:SuiteGenerator
  val project:ProjectGenerator
  
  def parseModule = {
    var reader = new FileReader(location)
    val descriptor = new ParseDescriptor("state_normal",location.toURI,suite,project)

    val parser = new VerilogParserTop()
    parser.compositeBuild(reader,descriptor)
    /*
    val modules = new ListBuffer[VerilogModule]()
    val items = descriptor.getModule.getRealSelfList

    for (i <- 0 until items.size()) {
      items.get(i) match {
        case x:Entity => modules.append(VerilogModule(x.getInstanceModRef.getObject))
        case _ => 
      }
    }
    */
  }
  
}

object VerilogModuleReader {
  
  def apply(location:File, suite:SuiteGenerator,  project:ProjectGenerator) = new Impl(location,suite,project)
  class Impl(override val location:File,
    override val suite:SuiteGenerator,
    override val project:ProjectGenerator) extends VerilogModuleReader
}