package com.simplifide.generate.blocks.basic.misc

import com.simplifide.generate.blocks.basic.flop.{SimpleFlop,  ClockControl}
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.signal.{Constant, SignalTrait}
import com.simplifide.generate.blocks.basic.operator.BinaryOperator
import com.simplifide.generate.generator.{SimpleSegment, SegmentReturn, CodeWriter}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/23/11
 * Time: 1:39 PM
 * To change this template use File | Settings | File Templates.
 */

class Counter(val counter:SignalTrait)(implicit clk:ClockControl) extends SimpleSegment {

  override def createCode(writer:CodeWriter):SegmentReturn = {
      val flop = new SimpleFlop(None,
        clk,
        new SimpleStatement.Reg(counter,Constant(0,counter.fixed)),
        new SimpleStatement.Reg(counter,BinaryOperator.Plus(counter,Constant(1,counter.fixed)))
      )
      writer.createCode(flop)
  }

}

object Counter {

}