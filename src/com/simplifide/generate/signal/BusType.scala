package com.simplifide.generate.signal

import com.sun.istack.internal.Pool.Impl

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/16/11
 * Time: 2:25 PM
 * To change this template use File | Settings | File Templates.
 */

/** Class which contains the type of signals included in a bus */
trait BusType {
  val signals:List[SignalTrait]

  /** Number of signals in the bus */
  lazy val length:Int = signals.size
  /** Create the signals associated with the name1 */
  def createSignals(name:String):List[SignalTrait] = {
    def signalName(name:String) = if (name.equalsIgnoreCase("")) "" else name + "_"
    signals.map(x => x.newSignal(signalName(name) + x.name))
  }
}

object BusType {
  def apply(signals:List[SignalTrait]) = new Impl(signals)

  class Impl(override val signals:List[SignalTrait]) extends BusType
}