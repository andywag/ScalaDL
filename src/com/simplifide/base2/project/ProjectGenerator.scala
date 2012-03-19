package com.simplifide.base2.project

import com.simplifide.base.core.project.CoreProjectBasic
import scala.collection.JavaConverters._
import com.simplifide.generate.util.FileOps
import java.io.File
import com.simplifide.base2.model.{Project, Module}
import com.simplifide.base.core.module.InstanceModule

/**
 */

abstract class ProjectGenerator extends CoreProjectBasic {

  /** Prefix for the files */
  val prefix:String
  /** Locations for all of the files */
  val locations:List[File]

  def createProject(suite:SuiteGenerator):Project = {

    val modules = locations.map(VerilogModuleReader(_,suite,this))
    modules.foreach(_.parseModule)

    val newModules = this.getAllInstanceModules.asScala.map(x => Module(x,prefix)).toList
    new Project.Derived(this,newModules,prefix)
  }

}

object ProjectGenerator {
  class Single(override val prefix:String, override val locations:List[File]) extends ProjectGenerator
}
