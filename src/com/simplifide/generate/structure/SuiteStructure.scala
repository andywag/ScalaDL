package com.simplifide.generate.structure

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 5/22/11
 * Time: 1:02 PM
 * To change this template use File | Settings | File Templates.
 */

class SuiteStructure(val name:String) {
    /** Location for this project */
    val structure:StructureDefinition = StructureConstants.defaultSuite(name)
    /** List of Projects in the Design */
    val projects:List[ProjectStructure] = List()
    /** List of Libraries in the Design */
    val libraries:List[ProjectStructure] = List()

    /** Create the Directory Structure for a ProjectStructure */
    def createDirectoryStructure(location:String) {
      structure.create(new java.io.File(location),name)
    }

}

object SuiteStructure {
  
}