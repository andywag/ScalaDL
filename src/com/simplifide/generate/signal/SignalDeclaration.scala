package com.simplifide.generate.signal

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}

/**
 * Class describing segment to create a signal declaration
 *
 * @constructor
 * @parameter signal Signal to be created
 *
 */
class SignalDeclaration(val signal:SignalTrait) extends SimpleSegment{


  // TODO Clean up this method
  // The basic verilog declaration
    private def createSingle(signal:SignalTrait):SegmentReturn = {
        def getDecType:String = {
          signal.opType match {
            case OpType.Input           => "input "
            case OpType.ModuleInput     => "input "
            case OpType.ModuleOutput    => "output "
            case OpType.Output          => "output "
            case OpType.ModuleRegOutput => "output reg "     // Only Works for Ansi Port Declarations
            case OpType.Register        => "reg "     // Only Works for Ansi Port Declarations
            case OpType.Param           => "parameter "
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
        def createAssignment:String = {
           if (signal.isInstanceOf[ParameterTrait]) return " = " + signal.asInstanceOf[ParameterTrait].value
           return ""
        }

        val builder = new StringBuilder
        builder.append(getDecType)
        if (signal.fixed.signed.isSigned) builder.append("signed ")
        builder.append(createWidthDeclaration(signal))
        builder.append(signal.name)
        builder.append(createArrayDeclaration(signal))
        builder.append(createAssignment)
        return builder.toString
    }

  private def createComment:SegmentReturn = {
    signal.description match {
      case Some(x) => SegmentReturn(" // ") + x.woXML + signal.fixed.getDescription
      case None    => SegmentReturn(" // ") + signal.fixed.getDescription
    }
  }

  private def createItem(signal:SignalTrait):SegmentReturn = {
    createSingle(signal) + "; " + this.createComment + "\n"
  }

  override def createCode(writer:CodeWriter):SegmentReturn = {
    this.signal.allSignalChildren.map(createItem(_)).reduceLeft(_+_)
  }



}

object SignalDeclaration {

  def apply(signal:SignalTrait):List[SignalDeclaration] =
    signal.allSignalChildren.map(x => new SignalDeclaration(x))

  def head(signal:SignalTrait):List[SignalDeclaration] =
    signal.allSignalChildren.map(x => new Head(x))

  class Head(signal:SignalTrait) extends SignalDeclaration(signal) {
    override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
      val builder = new StringBuilder
      this.signal.allSignalChildren.foreach(x => builder.append(createSingle(x)))
      return SegmentReturn(builder.toString)
    }
  }


}