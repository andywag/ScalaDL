package com.simplifide.generate.project

import com.simplifide.generate.structure.{StructureConstants, StructureDefinition}
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 5/30/11
 * Time: 3:34 PM
 * To change this template use File | Settings | File Templates.
 */

/** Trait which contains methods for generating the structure and contents of a project */
trait ProjectProvider {
  /** Name of the Project */
  val name:String
  /** List of Modules associated with this project. When this project is generated each module
    * will be generate. This should only include leaf modules as connections should be defined
    * in a seperate location
    **/
  val modules:List[ModuleProvider[_]] = List()
  /** Directory Structure Associate with the project */
  val structure:StructureDefinition = StructureConstants.defaultProject(name)
  //val segment:ModuleSegment = null

  def createStructure(location:String) {
    structure.create(new java.io.File(location))
    modules.foreach(x => x.writeModule(CodeWriter.Verilog,getDesignGeneratorLocation(location))) // Create the leaf modules for the project
  }
    /** Returns the location of the design directory */
  protected def getDesignLocation(location:String):String =
    structure.findFirstLocationString(StructureConstants.SOURCE,location)
  /** Returns the location of the design directory */
  protected def getDesignGeneratorLocation(location:String):String =
    structure.findFirstLocationString(StructureConstants.SOURCE_GEN,location)
  /** Returns the test location */
  protected def getTestLocation(location:String):String =
    structure.findFirstLocationString(StructureConstants.TEST,location)
  /** Returns the location of the design directory */
  protected def getTestSourceLocation(location:String):String =
    structure.findFirstLocationString(StructureConstants.TEST_SRC,location)
  /** Returns the location of the design directory */
  protected def getMatlabLocation(location:String):String =
    structure.findFirstLocationString(StructureConstants.TEST_MATLAB,location)

 
}

object ProjectProvider {
  class Basic(override val name:String)
  extends  ProjectProvider with TestProvider {

    

  override def createStructure(location:String) {
    structure.create(new java.io.File(location))
    this.createCode(CodeWriter.Verilog,location)
  }

  def createCode(writer:CodeWriter, location:String)  {
      //if (segment != null) segment.writeModule(writer,getDesignGeneratorLocation(location))
      val locations = new TestProvider.Locations(getTestLocation(location),
                                               getTestSourceLocation(location),
                                               getMatlabLocation(location))
      //this.createTestCode(locations, this.segment,writer)

    }

  }
}