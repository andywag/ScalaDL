package com.simplifide.generate.structure
import com.simplifide.generate.util.FileOps
import java.io.File

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 5/21/11
 * Time: 10:16 AM
 * To change this template use File | Settings | File Templates.
 */

/** Class which defines the directory structure for an operation */
abstract class StructureDefinition(val name:String, val attributes:List[Int]) {

  /** Create the set of files and directories associated with this location */
  def create(location:String) {
    create(new java.io.File(location))
  }
  /** Create the files and directories associated with this directory structure */
  def create(location:java.io.File)
  /** Create the project contents in a new directory with the name1 given in the input */
  def create(location:java.io.File, name:String)  {
    val dir = new java.io.File(location,name)
    FileOps.createDirectory(dir)
    create(dir)
  }
  def createSkipRoot(location:java.io.File) {}

  def convertToFile(root:String) = {

  }

  /** Get Locations With Attribute */
  def findLocations(attribute:Int, prefix:String):List[java.io.File] =
      if (this.attributes.contains(attribute)) List(new java.io.File(prefix,this.name)) else List()

  def findFirstLocation(attribute:Int, prefix:String):java.io.File = {
    val locations = findLocations(attribute,prefix)
    return locations(0)
  }
   def findFirstLocationString(attribute:Int, prefix:String):String = {
    findFirstLocation(attribute,prefix).getAbsolutePath
  }


}

object StructureDefinition {



  class Directory(name:String, attributes:List[Int], val contents:List[StructureDefinition]) extends StructureDefinition(name,attributes) {
    def this(name:String,contents:List[StructureDefinition]) = this(name,List(),contents)
    //def this(name1:String,contents:List[Int]) = this(name1,contents,List())

    override def createSkipRoot(location:java.io.File) {
       contents.foreach(_.create(location))
    }

    override def create(location:java.io.File) {
      val dir = new java.io.File(location,name)
      dir.mkdir()
      contents.foreach(_.create(dir))  // Create the Contents of the subfiles and directories
    }
    override def findLocations(attribute:Int, prefix:String):List[java.io.File] = {
      val sup = super.findLocations(attribute,prefix)
      contents.flatMap(x => x.findLocations(attribute, prefix + "/" + name)) ::: sup
    }

  }
  class File(name:String,val contents:String, attributes:List[Int]) extends StructureDefinition(name, attributes) {
    def File(name:String) = new File(name,"",List())

    /** Create the contents of the file */
    override def create(location:java.io.File) {
      com.simplifide.generate.util.FileOps.createFile(new java.io.File(location,name),contents)
    }




  }

}

