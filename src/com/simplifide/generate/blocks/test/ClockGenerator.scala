package com.simplifide.generate.blocks.test

import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.blocks.test.ClockGenerator.ClockCreate

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/23/11
 * Time: 10:03 AM
 * To change this template use File | Settings | File Templates.
 */

class ClockGenerator(val clk:ClockControl, val period:Int = 10) extends SimpleSegment {

  override def createCode(writer:CodeWriter):SegmentReturn = {
     return new ClockCreate(clk,period).createCode(writer)
  }


}

object ClockGenerator  {

  class ClockCreate(val clk:ClockControl, val period:Int) extends SimpleSegment {
    override def createCode(writer:CodeWriter):SegmentReturn = {
      return SegmentReturn("always #" + period + " ") + clk.clock.name + " <= ~" + clk.clock.name + ";\n\n"
    }
  }

}