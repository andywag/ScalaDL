package com.simplifide.generate.project

import com.simplifide.generate.signal.{SignalDeclaration, SignalTrait}
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter}
import com.simplifide.generate.util.{StringOps, FileOps}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 1/18/12
 * Time: 3:36 PM
 * To change this template use File | Settings | File Templates.
 */

/*
trait EntityGenerator {

  val entity:Entity
  import entity._


  protected def comment:String = {
    "//-----------------------------------------------------------------------------\n" +
    "// Company: 			ACME Corp                                                     \n" +
    "// Author:				Andy                                                          \n" +
    "// Date:                                                                       \n" +
    "// Module Name:       " + entity.name +                                       "\n" +
    "// Description:                                                                \n" +
    "//                                                                             \n" +
    "//-----------------------------------------------------------------------------\n"
  }

  /** Create the code for the head of the module */
  def createHead(writer:CodeWriter):String = {
    def singleDec(index:Int,segment:String):String = {
      if (index != 0) return ",\n" + segment.replaceAll("\\s+$", "")
      else            return segment.replaceAll("\\s+$", "")
    }
    // Return all of the signals contained in this entity
    val tot:List[SignalTrait] = SignalTrait.uniqueSignals( (entitySignals ::: extraSignals).flatMap(_.allSignalChildren))
    // Filter to only input/output types and sort by type then name
    val fil = tot.filter(x => !x.opType.isSignal).sortBy(x => (x.opType.toString, x.name))
    // Create the Declaration
    val dec:List[ScalaDeclaration] = fil.flatMap(ScalaDeclaration.head(_))
    "(\n" + StringOps.accumulate(dec.zipWithIndex.map(x => singleDec(x._2,writer.createCode(x._1).code))) +");\n\n"

  }



  def createCode(writer:CodeWriter):SegmentReturn = {
    implicit val writ:CodeWriter = writer
    val mod = entity.createModule
    return SegmentReturn(comment) + SegmentReturn("module ") + name + this.createHead(writer) + mod + "endmodule\n\n"
  }

  def writeModule(fileLocation:String):SegmentReturn     = {
    val writer = CodeWriter.Verilog
    val ret = createCode(writer)
    this.entity.extraFiles.foreach(_.createFile(fileLocation))
    FileOps.createFile(fileLocation, entity.name + ".v",ret.code)
    ret
  }
 
  
}

object EntityGenerator {

  def apply(entity:Entity) = new Impl(entity)
  class Impl(val entity:Entity) extends EntityGenerator
  
  
}
*/