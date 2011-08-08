package com.simplifide.generate.language

import com.simplifide.generate.project.ModuleProvider
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.parser.condition.Condition
import com.simplifide.generate.parser.model.{Expression, Signal, Model, SignalType}
import com.simplifide.generate.parser.{ModuleParser, ObjectFactory, SignalParser}
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.block.state.StateModel
import com.simplifide.generate.blocks.statemachine.StateMachine
import complex.ComplexSignal

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/15/11
 * Time: 9:11 PM
 * To change this template use File | Settings | File Templates.
 */

class Module(val name:String) extends ModuleParser {



  val INPUT  = OpType.Input
  val OUTPUT = OpType.Output
  val WIRE   = OpType.Signal
  val REG    = OpType.Register

  def S(width:Int,fraction:Int)        = signed(width,fraction)
  def U(width:Int,fraction:Int)        = unsigned(width,fraction)
  def signed(width:Int,fraction:Int)   = FixedType.signed(width,fraction)
  def unsigned(width:Int,fraction:Int) = FixedType.unsigned(width,fraction)

    override def constant(value:Double) = {
     val values = List.tabulate(32)(i => value*scala.math.pow(2.0,i-16))
     //val floors = values.map(x => scala.math.floor(x))
     val intValue = values.reverse.indexWhere(x => scala.math.floor(x) == 0)
     val fracValue = values.indexWhere(x => (x - scala.math.floor(x) == 0))
     Constant(value,signed(fracValue - intValue-1,fracValue-16))
   }

  //def param(name:String, value:Int) = signal(ParameterTrait(name,value))

  /** Create a register from a signal while specifying the clock */
  def register(signal1:SignalTrait, clock:ClockControl) (length:Int):Signal = {
    signal(RegisterTrait(signal1,length,clock))
  }

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

  def complex(name:String, typ:OpType = OpType.Signal,fixed:FixedType = FixedType.None):Signal = {
    val sig = ComplexSignal(name,typ,fixed)
    signals.append(sig)
    sig
  }


  /** Create a state machine based on a state model */
  def state_machine(model:StateModel,clk:ClockControl,state:SignalTrait, next:SignalTrait) = {
    this.assign(new StateMachine(model,clk,state,next))
  }


  def createModule:ModuleProvider = {
      this.transform
      ModuleProvider(name,
        this.signals.toList.map(x => x.asInstanceOf[SignalTrait]),
        this.statements.toList.map(x => x.asInstanceOf[SimpleSegment]))
  }

}