package com.simplifide.base2.project

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 3/2/12
 * Time: 3:46 PM
 * To change this template use File | Settings | File Templates.
 */

trait VerilogModuleContainer {

  val modules:List[VerilogModule]
}

object VerilogModuleContainer {
  def apply(modules:List[VerilogModule]) = new Impl(modules)
  class Impl(val modules:List[VerilogModule]) extends VerilogModuleContainer
}