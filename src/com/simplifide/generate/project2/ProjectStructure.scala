package com.simplifide.generate.project2

import com.simplifide.generate.util.FileOps


/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/21/11
 * Time: 2:34 PM
 * To change this template use File | Settings | File Templates.
 */

trait ProjectStructure {

  val separator = "/"

  val project:Project
  /** Location where the design sources are stored */
  val designLocation:String
  /**Location where the docuemnts are stored */
  val docLocation:String
  /** Location where test cases are stored */
  val testLocation:String

  def design = project.location + separator + designLocation
  def doc    = project.location + separator + docLocation
  def test    = project.location + separator + testLocation


  def create = {
    FileOps.createDirectory(project.location)
    FileOps.createDirectory(design)
    FileOps.createDirectory(doc)
    FileOps.createDirectory(test)

  }

}

object ProjectStructure {
  val DESIGN = "design"
  val DOC    = "doc"
  val TEST   = "test"

  def apply(project:Project,
            designLocation:String = DESIGN,
            docLocation:String = DOC,
            testLocation:String = TEST) = new User(project,designLocation,docLocation,testLocation)

  class User(override val project:Project,
    override val designLocation:String = DESIGN,
    override val docLocation:String    = DOC,
    override val testLocation:String   = TEST) extends ProjectStructure


}