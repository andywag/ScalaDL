package com.simplifide.generate.language

import com.simplifide.generate.project.ModuleProvider
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.parser.{ObjectFactory, SignalParser}
import com.simplifide.generate.signal._
import com.simplifide.generate.parser.condition.Condition
import com.simplifide.generate.parser.model.{Expression, Signal, Model, SignalType}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/15/11
 * Time: 9:11 PM
 * To change this template use File | Settings | File Templates.
 */

class Module(val name:String) extends SignalParser {



  val INPUT  = OpType.Input
  val OUTPUT = OpType.Output
  val WIRE   = OpType.Signal
  val REG    = OpType.Signalr

  def signed(width:Int,fraction:Int)   = FixedType.signed(width,fraction)
  def unsigned(width:Int,fraction:Int) = FixedType.unsigned(width,fraction)

    /** Convenience method for creating a signal */
  def bus(name:String, typ:BusType):Bus =  {
    val bus = Bus(name,typ)
    signals.append(bus)
    bus
  }
  /** Convenience method for creating a signal */
  def busArray(name:String, typ:BusType)(arr:Int*):SignalTrait =  {
    val bus = Bus(name,typ)
    val sig = ArrayTrait(bus,arr(0))
    signals.append(sig)
    sig
  }


  def createModule:ModuleProvider = {
      ModuleProvider(name,
        this.signals.toList.map(x => x.asInstanceOf[SignalTrait]),
        this.statements.toList.map(x => x.asInstanceOf[SimpleSegment]))
  }

}