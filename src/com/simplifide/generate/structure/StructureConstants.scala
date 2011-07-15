package com.simplifide.generate.structure

import com.simplifide.generate.structure.StructureDefinition._

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 5/22/11
 * Time: 9:46 AM
 * To change this template use File | Settings | File Templates.
 */

class StructureConstants {

}

object StructureConstants {

  val PROJECTS:Int     = 0
  val LIBRARIES:Int    = 1
  val TESTPROJECTS:Int = 2
  val DESIGN:Int       = 3
  val SOURCE           = 4
  val SOURCE_GEN       = 5
  val TEST             = 10
  val TEST_DATA        = 11
  val TEST_MATLAB      = 12
  val TEST_SRC         = 13


  /** Basic Directory structure of a suite */
  def defaultSuite(name:String) =
    new Directory(name,List(
      new Directory("projects",List(PROJECTS),List()),
      new Directory("libraries",List(LIBRARIES),List()),
      new Directory("script", List(
        new Directory("src", List(SOURCE),List()),
        new Directory("bin", List(SOURCE_GEN),List())
      ))
    ))

  def defaultProject(name:String) =
    new StructureDefinition.Directory(name,List(
      new StructureDefinition.Directory("design", List(
        new StructureDefinition.Directory("src", List(SOURCE),List()),
        new StructureDefinition.Directory("gen", List(SOURCE_GEN),List())
       )),
      new StructureDefinition.Directory("test", List(TEST),List(
        new StructureDefinition.Directory("src", List(TEST_SRC),List()),
        new StructureDefinition.Directory("gen", List(SOURCE_GEN),List()),
        new StructureDefinition.Directory("data", List(TEST_DATA),List()),
        new StructureDefinition.Directory("matlab", List(TEST_MATLAB),List())
      ))
    ))
}