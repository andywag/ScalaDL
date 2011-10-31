package com.simplifide.generate.test

import com.simplifide.generate.util.{ProcessOps, FileOps}
import com.simplifide.generate.TestConstants
import com.simplifide.generate.project.{Entity, Project}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 9/26/11
 * Time: 9:42 PM
 * To change this template use File | Settings | File Templates.
 */


class Isim(val project:Project) extends SimInterface{

  val separator = "/"

  val PROJECTFILE = "files.prj"
  val testLocation =  project.projectStructure.test



  //

  def projectFile    = project.projectStructure.test + separator + PROJECTFILE

  def createSimFiles(files:List[String]) = {
    def contents = files.map(x => "verilog work " + x + "\n").reduceLeft(_ + _)
    FileOps.createFile(testLocation  + separator,PROJECTFILE,contents)
  }

  def compile(entity:Entity) {
    //def command = "C:\\Xilinx\\13.2\\ISE_DS\\ISE\\bin\\nt64\\fuse.exe work." + entity.name + " -prj " + projectFile + " -o " + testLocation + "\\" +  entity.name + ".exe"
    def command = TestConstants.fuseLocation + " work." + entity.name + " -prj " + projectFile + " -o " + testLocation + separator +  entity.name

    ProcessOps.exec(command,Some(testLocation))(ln => System.out.println(ln))

  }

  def run(entity:Entity) {
    def command = "fuse work." + entity.name + " -prj" + projectFile + " -o " + entity.name //+ ".exe"
  }


}

object Isim


