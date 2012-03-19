package com.simplifide.base2.model

import com.simplifide.base2.project.{SuiteGenerator, ProjectGenerator}
import com.simplifide.base2.model.Suite.Derived
import com.simplifide.generate.util.FileOps


/**
  *
  */

trait Suite {
  /** List of Project contained in this suite */
  val projects:List[Project]

  /** Construct ScalaDL */
  def create(location:java.io.File) = {
    FileOps.createDirectory(location)
    val src = FileOps.createDirectory(location,Some("src"))
    projects.foreach(_.create(src))
  }

}

object Suite {
  def apply(suiteGenerator:SuiteGenerator,projects:List[Project]) =
    new Derived(suiteGenerator,projects)

  class Derived(val SuiteGenerator:SuiteGenerator, val projects:List[Project]) extends Suite {

  }
}