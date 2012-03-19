package com.simplifide.base2.generator

import com.simplifide.generate.util.FileOps
import com.simplifide.base2.generator.ScalaFile.Impl

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 3/4/12
 * Time: 12:03 PM
 * To change this template use File | Settings | File Templates.
 */

trait ScalaFile {

  val file:java.io.File
  val packageName:String

  val imports:List[ScalaObject]
  val items:List[ScalaObject]
  
  def createContents:String = {
    val title = "package " + packageName + "\n\n"
    val importItem = (imports ::: items.flatMap(_.imports)).map(_.generate).foldLeft("")(_+_)
    val body       = items.map(_.generate).foldLeft("")(_+_)
    title + importItem + body
  }
  
  def createFile = {
    FileOps.createFile(file,createContents)
  }

}

object ScalaFile {
  
  def apply(file:java.io.File,packageName:String,imports:List[ScalaObject],items:List[ScalaObject]) =
    new Impl(file,packageName,imports,items)
  
  class Impl(val file:java.io.File,
    val packageName:String,  
    val imports:List[ScalaObject],
    val items:List[ScalaObject]) extends ScalaFile
}


