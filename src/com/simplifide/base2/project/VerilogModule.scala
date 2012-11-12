package com.simplifide.base2.project

import com.simplifide.base.basic.struct.ModuleObjectNew
import com.simplifide.base.core.module.{InstanceModule, SuperModule}
import scala.collection.JavaConverters._

/**
 *
 */

trait VerilogModule {

  val module:InstanceModule

  val name = module.getname()

  def vars = module.getAllVars.asScala




  
}

object VerilogModule {
  def apply(module:InstanceModule) = new Impl(module)
  class Impl(override val module:InstanceModule) extends VerilogModule
}