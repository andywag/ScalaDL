package com.simplifide.generate.blocks.proc

import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.signal.{OpType, SignalTrait}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/18/11
 * Time: 8:14 AM
 * To change this template use File | Settings | File Templates.
 */

class ProcessorBus(val clk:ClockControl,
  val writeAddress:SignalTrait,
  val writeValid:SignalTrait,
  val writeData:SignalTrait,
  val readAddress:SignalTrait,
  val readValid:SignalTrait,
  val readData:SignalTrait) {

  def signals = List(clk.getBus(OpType.Input),writeAddress,writeValid,writeData,readAddress,readValid,readData)


}

object ProcessorBus {
  def apply(clk:ClockControl,
            writeAddress:SignalTrait,
            writeValid:SignalTrait,
            writeData:SignalTrait,
            readAddress:SignalTrait,
            readValid:SignalTrait,
            readData:SignalTrait) = new ProcessorBus(clk,writeAddress,writeValid,writeData,readAddress,readValid,readData)
}