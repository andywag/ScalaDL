package com.simplifide.generate.blocks.test

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.blocks.basic.SimpleStatement

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/23/11
 * Time: 11:20 AM
 * To change this template use File | Settings | File Templates.
 */

class Initial(val segments:List[SimpleSegment]) extends SimpleSegment {

  override def split:List[Expression] =
    List(new Initial(segments.flatMap(_.split).map(_.asInstanceOf[SimpleSegment])))


  override def createCode(writer:CodeWriter):SegmentReturn = {
    def segmentList = segments.map(x => writer.createCode(x)).reduceLeft(_ + _)
    SegmentReturn.segment("initial begin\n") ++
      segmentList +
    "end\n\n"
  }

}

object Initial {
  def apply(segments:List[SimpleSegment]) = new Initial(segments)


  class Delay(val signal:SignalTrait, val value:Long, val delay:Int) extends SimpleSegment {
    override def createCode(writer:CodeWriter):SegmentReturn = {
      SegmentReturn(signal.name) + " = #" + delay.toString + " " +  value.toString + ";\n"
    }
  }

  class Assignment(val signal:SignalTrait, val value:Long) extends SimpleSegment {
    override def createCode(writer:CodeWriter):SegmentReturn = {
      SegmentReturn(signal.name) + " = " + value.toString + ";\n"
    }
  }

  class AssignSegment(val signal:SignalTrait, val value:SimpleSegment) extends SimpleSegment {

    override def split:List[Expression] = new SimpleStatement.Body(signal,value).split
    override def createCode(writer:CodeWriter):SegmentReturn = {
      null
    }
  }


}