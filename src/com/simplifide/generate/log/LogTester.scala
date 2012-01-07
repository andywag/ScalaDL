package com.simplifide.generate.log

import com.simplifide.generate.util.FileOps

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 12/20/11
 * Time: 6:24 PM
 * To change this template use File | Settings | File Templates.
 */

object LogTester {

  /*
  val  location = "/home/andy/simplifide_base/Generator/src/com/simplifide/generate/log/log.syr"
  val  base = "/home/andy/simplifide_base/Generator/src/com/simplifide/generate/log/"
  */

  val base =  "c:/designs2/Generator/src/com/simplifide/generate/log/"
  val  location = "c:/designs2/Generator/src/com/simplifide/generate/log/log.par"

  def main(args:Array[String]) = {
    val lines = scala.io.Source.fromFile(location).mkString
    val result = LogParser.analyze(lines)
    FileOps.createFile(base,"tree.txt",result)
  }


  
}