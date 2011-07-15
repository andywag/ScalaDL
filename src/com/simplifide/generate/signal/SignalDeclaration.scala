package com.simplifide.generate.signal

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 6/8/11
 * Time: 7:24 AM
 * To change this template use File | Settings | File Templates.
 */

class SignalDeclaration(val signal:SignalTrait) extends SimpleSegment{

  /** Returns the verilog declaration type associated with this declaration*/
  val verilogDecType:String = "wire "

  override def createCode(writer:CodeWriter):SegmentReturn = {
       writer.createCode(this)
  }

  // The basic verilog declaration
    def createVerilogSignalItem(signal:SignalTrait):String = {
        def getDecType:String = {
          signal.opType match {
            case OpType.Input           => "input "
            case OpType.ModuleInput     => "input "
            case OpType.ModuleOutput    => "output "
            case OpType.ModuleRegOutput => "output reg"     // Only Works for Ansi Port Declarations
            case _                      => "wire "
          }
        }
        def createWidthDeclaration(signal:SignalTrait):String = {
          val builder = new StringBuilder
          if (signal.fixed.width > 1) {
            builder.append("[")
            builder.append(signal.fixed.width-1)
            builder.append(":0] ")
          }
          return builder.toString
        }
        def createArrayDeclaration(signal:SignalTrait):String = {
          val builder = new StringBuilder
          if (signal.arrayLength > 0) {
            builder.append("[0:")
            builder.append(signal.arrayLength)
            builder.append("] ")
          }
          return builder.toString
        }
        val builder = new StringBuilder
        builder.append(getDecType)
        if (signal.fixed.signed.isSigned) builder.append("signed ")
        builder.append(createWidthDeclaration(signal))
        builder.append(signal.name)
        builder.append(createArrayDeclaration(signal))
        return builder.toString
    }

  def createVerilogSignalItemLine(signal:SignalTrait):String =
    createVerilogSignalItem(signal) + "; // " + signal.fixed.getDescription + "\n"





  override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
    val builder = new StringBuilder
    this.signal.allSignalChildren.foreach(x => builder.append(createVerilogSignalItemLine(x)))
    return SegmentReturn.segment(builder.toString)
  }

   /** Creates the basic signal declaration */
    def createCDeclaration( name:String, prefix:String,postfix:String):String = {
      val builder = new StringBuilder
      builder.append(prefix)
      builder.append(name)
      builder.append(postfix)
      builder.append(";\n")
      builder.toString
    }
    /** Creates the extra registers associated with this signal */
  /*
  def createCExtra(signal:SignalTrait):String = {
      val builder = new StringBuilder
      for (vec <- signal.vector.arr) {
        for (i <- 0 until vec) {
          builder.append(createCDeclaration(this.signal.name + "_" + (i+1).toString,"static float "," = 0.0"))
        }
      }
      return builder.toString
    }
   */


}

object SignalDeclaration {

  def createSignalDeclarations(signal:SignalTrait):List[SignalDeclaration] =
    signal.allSignalChildren.map(x => new SignalDeclaration(x))

  def createSignalDeclarationsHead(signal:SignalTrait):List[SignalDeclaration] =
    signal.allSignalChildren.map(x => new Head(x))

  class Head(signal:SignalTrait) extends SignalDeclaration(signal) {
    override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
      val builder = new StringBuilder
      this.signal.allSignalChildren.foreach(x => builder.append(createVerilogSignalItem(x)))
      return SegmentReturn.segment(builder.toString)
    }
  }


}