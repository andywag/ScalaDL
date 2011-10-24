package com.simplifide.generate.blocks.basic.misc

import java.sql.Time
import com.simplifide.generate.blocks.basic.misc.TimingControl.TimeSignalValue
import com.simplifide.generate.blocks.basic.condition.ConditionStatement
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.blocks.basic.operator.BinaryOperator
import com.simplifide.generate.blocks.basic.flop.{ClockControl, SimpleFlop}
import com.simplifide.generate.signal.{Constant, SignalTrait}
import com.simplifide.generate.generator.{BasicSegments, SegmentReturn, CodeWriter, SimpleSegment}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/23/11
 * Time: 1:55 PM
 * To change this template use File | Settings | File Templates.
 */


/** Flop which assigns statemetns as a function of time */
class TimingControl(val counter:SignalTrait,val values:List[TimeSignalValue])(implicit clk:ClockControl) extends SimpleSegment {

  override def createCode(writer:CodeWriter):SegmentReturn = {
     val signals = values.map(_.signal).toSet.toList
     val groups = values.groupBy(_.time).toList.sortBy(_._1) // Create a list of time - values
     val condition = groups.map(x => ( Some(BinaryOperator.LTE(counter,Constant(x._1,counter.fixed.width))),
       x._2.map(y => new SimpleStatement.Reg(y.signal,y.value))))

     val resets = signals.map(x => new SimpleStatement.Reg(x,Constant(0,x.fixed.width)))
     val conditional = ConditionStatement(condition)
     val flop = new SimpleFlop(None,clk,BasicSegments.List(resets),conditional)

     writer.createCode(flop)
  }


}

object TimingControl {


  class TimeSignalValue(val time:Int,val signal:SimpleSegment,val value:SimpleSegment)

  implicit def Signal2SignalWrap(signal:SignalTrait):SignalWrap = new SignalWrap(signal)

  class SignalWrap(signal:SignalTrait) {
    def --> (values:List[(Int,SimpleSegment)]):List[TimeSignalValue] = values.map(x => new TimeSignalValue(x._1,signal,x._2))
  }

}
