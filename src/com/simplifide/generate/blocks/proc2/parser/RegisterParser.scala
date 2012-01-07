package com.simplifide.generate.blocks.proc2.parser

import collection.mutable.ListBuffer
import com.simplifide.generate.blocks.proc.ProcessorBus
import com.simplifide.generate.signal.SignalTrait._
import com.simplifide.generate.blocks.proc2.RegisterGroup._
import com.simplifide.generate.blocks.proc2._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.signal.{OpType, SignalTrait, FixedType}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 1/3/12
 * Time: 11:15 AM
 * To change this template use File | Settings | File Templates.
 */

trait RegisterParser extends RegisterParser.Builder{

  implicit val clk:ClockControl
  /** Groups of registers */
  val groups       = new ListBuffer[RegisterGroup]()

  /** Bus which is required for this processor interface */
  val processorBus:ProcessorBus


  /** Creates a set of registers defined in a group */
  def registerGroup(baseAddress:Int)(section:RegisterModel.Section) =
    groups.append(section.createGroup(baseAddress))
  
  /** Creates an address definition */
  //def address(baseAddress:Int, section:RegisterModel.Section) =
  //  new RegisterModel.AddressSection(baseAddress,List())


  lazy val registerMap  = new RegisterMapNew(groups.toList)
  lazy val readDecoder  = new AddressDecoderNew.Read(registerMap,processorBus)
  lazy val writeDecoder = new AddressDecoderNew.Write(registerMap,processorBus)

}

object RegisterParser {
  trait Builder {
    /** Create a section */
    def sectionOpen(register:RegisterNew)  = RegisterModel.Section(FullRegister(register))
    /** Creates a read register for the processor interface */
    def read(name:String, width:Int)       = sectionOpen(new RegisterNew.Read(SignalTrait(name,OpType.Input,FixedType.unsigned(width,0))))
    def read(signal:SignalTrait)           = sectionOpen(new RegisterNew.Read(signal))
    /** Creates a write register for the processor interface */
    def write(name:String, width:Int)      = sectionOpen(new RegisterNew.Write(SignalTrait(name,OpType.ModuleRegOutput,FixedType.unsigned(width,0))))
    def write(signal:SignalTrait)          = sectionOpen(new RegisterNew.Write(signal))
    /** Creates a read-write register for the processor interface */
    def readWrite(name:String, width:Int)  = sectionOpen(new RegisterNew.ReadWrite(SignalTrait(name,OpType.ModuleRegOutput,FixedType.unsigned(width,0))))
    def readWrite(signal:SignalTrait)      = sectionOpen(new RegisterNew.ReadWrite(signal))

    //def address(location:Int)(registers:List[RegisterNew])
  }
}