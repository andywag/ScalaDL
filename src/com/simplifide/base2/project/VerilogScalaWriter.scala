package com.simplifide.base2.project

import com.simplifide.base.core.`var`.realvars.SystemVar

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 3/2/12
 * Time: 4:20 PM
 * To change this template use File | Settings | File Templates.
 */


class VerilogScalaWriter {}

object VerilogScalaWriter {


  def signals(module:VerilogModule):String = {
    def signal(systemVar:SystemVar):String =  {
      val io    = if (systemVar.isInput) "INPUT" else if (systemVar.isOutput) "OUTPUT"
      val fixed = "U(" + systemVar.getWidth + ",0)"
      "  val " + systemVar.getname() + " = " + "signal(" + systemVar.getname() + "," + io + "," + fixed + ");\n"
    }
    module.vars.map(signal(_)).foldLeft("")(_+_)
  }
  
  def writeFile(module:VerilogModule):String = {
    val head = "Class " + module.name + " extends EntityParser {\n\n"
    val sig  = signals(module)
    val tail = "}\n\n"
    head + sig + tail
  }

  
}
