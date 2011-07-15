/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.simplifide.generate.util

import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter

/** Class which contains methods and utilities for dealing with the file system */
class FileOps {}


object FileOps {


  def getFileContents(location:String):String = scala.io.Source.fromFile(location).mkString


  def createDirectory(file:java.io.File) {
	  if (!file.exists) file.mkdir
  }
  /** Create a directory at the location location if it doesn't already exist */
  def createDirectory(location:String,extra:Option[String])  {
    val ufile = extra match {
      case None    => new File(location)
      case Some(x) => new File(location,x)
    }
    FileOps.createDirectory(ufile)
  }
  /** Create a new file in the input directory with the contents 'contents' */
  def createFile(file:java.io.File, contents:String) {
    val fil = new FileWriter(file)
    fil.write(contents.toString)
    fil.close()
    System.out.println("Wrote File" + file)
    System.out.println(contents)

    //createFile2(file,contents)
  }



  /** Create a new file with the directory from location with filename 'filename' and the contents 'contents' */
  def createFile(location:String,filename:String,contents:String)  {
    createFile(new File(new File(location),filename),contents)
  }



}
