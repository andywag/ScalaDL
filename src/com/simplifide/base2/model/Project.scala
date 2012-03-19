package com.simplifide.base2.model

import com.simplifide.base2.project.ProjectGenerator
import com.simplifide.generate.util.FileOps
import com.simplifide.base2.model.Project.Derived

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 3/6/12
 * Time: 3:17 PM
 * To change this template use File | Settings | File Templates.
 */

trait Project {

  val prefix:String
  /** List of modules contained in this project */
  val modules:List[Module]


  def create(location:java.io.File) = {
    val dirs = prefix.split("\\.")
    var base = location
    for (dir <- dirs) {
      base = FileOps.createDirectory(base,Some(dir))
    }
    modules.foreach(x => x.create(base))
  }

}

object Project {
  def apply(project:ProjectGenerator,modules:List[Module],prefix:String) = new Derived(project,modules,prefix)
  class Derived(val project:ProjectGenerator, val modules:List[Module], val prefix:String) extends Project
}