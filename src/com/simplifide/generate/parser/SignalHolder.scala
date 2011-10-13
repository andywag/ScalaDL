package com.simplifide.generate.parser

import collection.mutable.ListBuffer
import model.{Model, SignalType, Signal}
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.signal.OpType.Constant
import com.simplifide.generate.signal.{Constant, OpType, SignalTrait}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 8/9/11
 * Time: 3:14 PM
 * To change this template use File | Settings | File Templates.
 */

/** Trait which holds a list buffer of signals as well as containing convenience methods
 *  for dealing with appendSignal creation.
 **/
trait SignalHolder extends SignalMethods{
  /** Main item which contains a listbuffer of signals*/
  val signals    = new ListBuffer[Signal]()

  /*** Adds a appendSignal to the module */
  override def appendSignal[T <: Signal](signal:T):T = {
    signals.append(signal)
    signal
  }



  /** Appends a list of signals*/
  def signal(values:SignalTrait*):SignalTrait = {
    signals.appendAll(values.toList)
    values(0)
  }

  def inputs(values:SignalTrait*) {
    signals.appendAll(values.map(_.changeType(OpType.Input)))
  }

  def outputs(values:SignalTrait*) {
    signals.appendAll(values.map(_.changeType(OpType.Output)))
  }
    /** Assign the clock to the module */
  def assignClock(clock:ClockControl):ClockControl = {
    appendSignal(clock.getBus(OpType.Input))
    clock
  }
  /** Creates a Constant */
  def C(width:Int, value:Int) = com.simplifide.generate.signal.Constant(value,width)




}