package com.simplifide.generate.blocks.proc2.parser

import com.simplifide.generate.blocks.basic.flop.ClockControl
import collection.mutable.ListBuffer
import com.simplifide.generate.blocks.proc.ProcessorBus
import com.simplifide.generate.blocks.proc2.RegisterGroup._
import com.simplifide.generate.blocks.proc2._
import com.simplifide.generate.signal.SignalTrait._
import com.simplifide.generate.signal.{SignalTrait, FixedType, OpType}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 1/5/12
 * Time: 11:40 AM
 * To change this template use File | Settings | File Templates.
 */
      /*
trait RegisterParserOld extends RegisterParser.Builder{

  implicit val clk:ClockControl
  /** Groups of registers */
  val groups       = new ListBuffer[RegisterGroup]()

  /** Bus which is required for this processor interface */
  val processorBus:ProcessorBus


  /** Creates a set of registers defined in a group */
  def registerGroup(baseAddress:Int)(registers:FullRegister*) =
    groups.append(RegisterGroup(baseAddress,registers.toList))

  lazy val registerMap  = new RegisterMapNew(groups.toList)
  lazy val readDecoder  = new AddressDecoderNew.Read(registerMap,processorBus)
  lazy val writeDecoder = new AddressDecoderNew.Write(registerMap,processorBus)

}

object RegisterParserOld {
  trait Builder {
    /** Creates a read register for the processor interface */
    def read(name:String, width:Int)       = new RegisterModel.Read(SignalTrait(name,OpType.Input,FixedType.unsigned(width,0)))
    def read(signal:SignalTrait)           = new RegisterModel.Read(signal)
    /** Creates a write register for the processor interface */
    def write(name:String, width:Int)      = new RegisterModel.Write(SignalTrait(name,OpType.ModuleRegOutput,FixedType.unsigned(width,0)))
    def write(signal:SignalTrait)          = new RegisterModel.Write(signal)
    /** Creates a read-write register for the processor interface */
    def readWrite(name:String, width:Int)  = new RegisterModel.ReadWrite(SignalTrait(name,OpType.ModuleRegOutput,FixedType.unsigned(width,0)))
    def readWrite(signal:SignalTrait)      = new RegisterModel.ReadWrite(signal)

    def address(location:Int)(registers:List[RegisterNew])
  }
}
      */