package com.simplifide.generate.project

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.util.{StringOps, FileOps}
import collection.immutable.List._
import com.simplifide.generate.signal.{SignalTrait, SignalDeclaration, OpType}
import java.lang.StringBuffer
import javax.management.remote.rmi._RMIConnection_Stub
import com.sun.org.apache.xml.internal.security.algorithms.SignatureAlgorithmSpi

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 5/31/11
 * Time: 7:19 PM
 * To change this template use File | Settings | File Templates.
 */

/** Trait describing a Module */
trait ModuleProvider extends SimpleSegment{
  /** Module Name */
  val name:String
  /** Module Header */
  //val header:String
  /** Module Inputs */
  //val inputs:List[SignalTrait]
  /** Module Outputs */
  //val outputs:List[SignalTrait]
  /**  Internal Module Signals */
  val signals:List[SignalTrait]
  /** Segments Associated with this module if it is a leaf*/
  val segments = List[SimpleSegment]()


    /** Convert all the inputs to an actual module input */
  //def getConvertedInput:List[SignalNew] = inputs.map(x => x.copyWithType(OpType.ModuleInput).asInstanceOf[SignalNew])
  /** Convert all the output to an actual module output */
  //def getConvertedOutput:List[SignalNew] = outputs.map(x => x.copyWithType(OpType.ModuleOutput).asInstanceOf[SignalNew])

  private def createSignalDeclaration(signals:List[SignalTrait], writer:CodeWriter):String = {
    val decs = signals.flatMap(x => SignalDeclaration.createSignalDeclarations(x))
    val builder = new StringBuilder
    decs.foreach(x => builder.append(writer.createCode(x).code))
    return builder.toString
  }

  def createHead2(writer:CodeWriter):String = {
    def singleDec(index:Int,segment:String):String = {
      if (index != 0) return ",\n" + StringOps.writeSpaces(segment,name.length() + 7)
      else            return segment
    }
    //val signals = inputs ::: outputs

    val builder = new StringBuilder
    builder.append("(")
    val tot:List[SignalTrait] = signals.filter(x => !x.opType.isSignal).flatMap(_.allSignalChildren)
    val dec:List[SignalDeclaration] = tot.flatMap(SignalDeclaration.createSignalDeclarationsHead(_))
    dec.zipWithIndex.foreach(x => builder.append(singleDec(x._2,writer.createCode(x._1).code)))
    builder.append(");\n\n")
    builder.toString
  }
   /** Create the code for the module head */
  /*def createHead:String = {
    def commaList(signals:List[SignalTrait]):String = {
      val ind = name.length + 7
      val builder = new StringBuilder()
      var first = true
      for (signal <- signals.flatMap(x => x.children)) {
        if (!first) {
          builder.append(",\n");
          builder.append(StringOps.writeSpaces(signal.name,ind))
        }
        else {
          builder.append(signal.name)
        }
        first = false;

      }
      return builder.toString
    }
    val builder = new StringBuilder()
    builder.append("(")
    builder.append(commaList(inputs ::: outputs))
    //builder.append(commaList(getInputs))
    builder.append(");\n\n")
    return builder.toString
  }*/

  def writeModule(writer:CodeWriter, location:String):SegmentReturn = writeVerilogModule(writer,location)

  def writeVerilogModule(writer:CodeWriter, location:String):SegmentReturn     = {
    /*val builder = new StringBuilder()
    builder.append("module ")
    builder.append(name)
    builder.append(this.createHead2(writer))
    builder.append("\n\n// Signal Declarations\n\n")
    val returns:List[SegmentReturn] = segments.map(x => writer.createCode(x))
    val internals = returns.flatMap(x => x.internal)
    builder.append(this.createSignalDeclaration(signals ::: internals,writer))

    builder.append("\n\n// Module Body\n\n")
    returns.foreach(x => builder.append(x.code))
    builder.append("endmodule")
    builder.append("\n\n")
    FileOps.createFile(location, this.name + ".v",builder.toString())
    return SegmentReturn.segment(builder.toString)
    */
    val ret = createCode(writer)
    FileOps.createFile(location, this.name + ".v",ret.code)
    ret

  }

  def createCode(writer:CodeWriter):SegmentReturn     = {
    val builder = new StringBuilder()
    builder.append("module ")
    builder.append(name)
    builder.append(this.createHead2(writer))
    builder.append("\n\n// Signal Declarations\n\n")
    val returns:List[SegmentReturn] = segments.map(x => writer.createCode(x))
    val internals = returns.flatMap(x => x.internal)
    builder.append(this.createSignalDeclaration(signals.filter(x => x.opType.isSignal) ::: internals,writer))

    builder.append("\n\n// Module Body\n\n")
    returns.foreach(x => builder.append(x.code))
    builder.append("endmodule")
    builder.append("\n\n")
    return SegmentReturn.segment(builder.toString)
  }

}

object ModuleProvider {

  def apply(name:String, signals :List[SignalTrait], segments:List[SimpleSegment]) =
    new Module(name,signals,segments)

  class Module(override val name:String,
               override val signals:List[SignalTrait],
               override val segments:List[SimpleSegment]) extends ModuleProvider

}