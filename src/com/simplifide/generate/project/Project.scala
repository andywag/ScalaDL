package com.simplifide.generate.project

import com.simplifide.generate.parser.SignalHolder
import com.simplifide.generate.test.{SimInterface, Test}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/12/11
 * Time: 2:12 PM
 * To change this template use File | Settings | File Templates.
 */

/** Project which contains a set of modules in the design*/
trait Project extends SignalHolder{

  /** Base Location of the Project */
  val location:String
  /** Structure Defining where the project outputs are written */
  val projectStructure = ProjectStructure(this)
  /** Base Entity for the Project */
  val root:Entity.Root = null
  /** List of Tests for the project */
  val tests:List[Test] = List()
  /** Test Type Structure */
  val testType:Option[SimInterface] = None
  /** Connect all of the modules */
  def expandProject:Project.Expanded = new Project.Expanded(this,root.connect)





  def createProject2 = {

    projectStructure.create
    // Expand the modules and connect the signals -- Emacs Auto Type Stuff
    val expanded = root.connect
    // Create a total list of entities in the design
    val total = List(expanded) ::: expanded.children
    // Write out all of the verilog modules
    total.foreach(x => x.writeModule(projectStructure.design))
    // Write out the documentation
    //total.map(x => new ModuleHtmlGenerator(x.name + ".html",x)).foreach(_.createFile(projectStructure.doc))
    // Create the Extra Files for the Design
    total.map(x => x.createModule).flatMap(x => x.extraFiles).foreach(x => x.createFile(projectStructure.doc))
    // Create the Tests for the Project
    tests.foreach(x => x.createTest(this))
    // Create the test environment
    if (testType.isDefined) {
       val test = testType.get
       val dfiles = total.map(x => projectStructure.design + "/" + x.name + ".v")
       val tfiles = tests.map(x => projectStructure.test + "/" + x.testEntity.name + ".v")
       test.createSimFiles(dfiles ::: tfiles)
       tests.foreach(x => test.compile(x.testEntity))
    }

  }


}

object Project {
  class Expanded(project:Project, root:Entity) {

  }

}