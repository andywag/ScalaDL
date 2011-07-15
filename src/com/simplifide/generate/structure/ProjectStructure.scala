package com.simplifide.generate.structure

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 5/21/11
 * Time: 10:14 AM
 * To change this template use File | Settings | File Templates.
 */

/** Class which contains utilities for creating a new project inside Simplifide */
class ProjectStructure(val name:String, val location:String) {

    /** Location for this project */
    val structure:StructureDefinition = StructureConstants.defaultProject(name)
    /** Create the Directory Structure for a ProjectStructure */
    def createDirectoryStructure {
      structure.create(new java.io.File(location),name)
    }
}