package com.simplifide.base2.project

import com.simplifide.base.core.project.CoreProjectSuite
import com.simplifide.base.core.project.define.DefineHolder
import scala.collection.JavaConverters._
import com.simplifide.generate.util.FileOps
import java.io.File
import com.simplifide.base2.model.{Suite, Module}

/**
 *
 */

abstract class SuiteGenerator extends CoreProjectSuite {

  this.setDefineHolder(new DefineHolder());

  /** List of Projects contained in this suite */
  val projects:List[ProjectGenerator]

  def createSuite:Suite = {
    Suite(this,projects.map(_.createProject(this)))
  }


}

object SuiteGenerator {
  
  class Single(val prefix:String,val locations:List[File]) extends SuiteGenerator {
    val projects = List(new ProjectGenerator.Single(prefix,locations))
  }
  

}
