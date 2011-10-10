package com.simplifide.generate.language

import com.simplifide.generate.util.FileOps

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/22/11
 * Time: 1:27 PM
 * To change this template use File | Settings | File Templates.
 */

trait ExtraFile {
  /** Contents of the generated file */
  val contents:String
  /** Name of the File to Generate */
  val filename:String

  def createFile(location:String) = {
    FileOps.createFile(location,filename,contents)
  }
}