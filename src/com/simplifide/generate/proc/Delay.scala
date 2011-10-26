package com.simplifide.generate.proc

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.parser.SegmentHolder

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 9/19/11
 * Time: 9:26 PM
 * To change this template use File | Settings | File Templates.
 */

class Delay(val signal:SignalTrait, val delay:Int) extends SimpleSegment {

  override def createCode(writer:CodeWriter):SegmentReturn = {
    signal(delay).asInstanceOf[SignalTrait].createCode(writer)
  }


  override def controlMatch(actual:SimpleSegment,statements:SegmentHolder):Boolean = {
    return signal.controlMatch(actual,statements)
  }

  override def createControl(actual:SimpleSegment,statements:SegmentHolder, index:Int):List[Controls] = {
    statements.getStatement(signal) match {
      case Some(x) => x.input.createControl(actual,statements,index-delay)
      case None    => List()
    }
  }


}

object Delay {
  def apply(signal:SignalTrait, delay:Int) = new Delay(signal,delay)
}