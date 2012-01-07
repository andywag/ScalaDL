package com.simplifide.generate.blocks.basic.flop

import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.generator.{BasicSegments, SimpleSegment}
import com.simplifide.generate.signal.{SignalTrait, Constant}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 12/16/11
 * Time: 9:22 AM
 * To change this template use File | Settings | File Templates.
 */

class SimpleFlopSegment(val clk:ClockControl,
                        val internal:SimpleSegment) extends SimpleSegment.Combo {

  val flop = {
    val resets = SignalTrait.uniqueSignals(internal.outputs).map(x => new SimpleStatement.Reg(x,Constant(0,x.fixed.width)))
    new SimpleFlop(None,clk,BasicSegments.List(resets),internal)
  }

  override def split = flop.split
  
}