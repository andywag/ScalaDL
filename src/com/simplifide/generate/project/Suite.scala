package com.simplifide.generate.project

import com.simplifide.generate.structure.{StructureDefinition, StructureConstants, SuiteStructure}
import java.io.File

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 5/30/11
 * Time: 1:51 PM
 * To change this template use File | Settings | File Templates.
 */

class Suite(val name:String) {
  val structure:StructureDefinition = StructureConstants.defaultSuite(name)
  val projects:List[ProjectProvider] = List()
  val libraries:List[ProjectProvider] = List()

  def createStructure(location:String) {
    structure.create(location)

    val projectLocations = structure.findLocations(StructureConstants.PROJECTS,location)
    projects.foreach(x => x.createStructure(projectLocations(0).getAbsolutePath))

    val libraryLocations = structure.findLocations(StructureConstants.PROJECTS,location)
    libraries.foreach(x => x.createStructure(libraryLocations(0).getAbsolutePath))

    System.out.println("Creating Suite")
  }



}

object Suite {

  def apply(name:String,projects1:List[ProjectProvider],libraries1:List[ProjectProvider]):Suite = {
    new Suite(name) {
      override val projects  = projects1
      override val libraries = libraries1
    }
  }

}