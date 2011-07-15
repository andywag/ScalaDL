package com.simplifide.generate.blocks.basic.operator

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.signal.SignalTrait

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 2/14/11
 * Time: 8:23 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class UnaryOperator(val in:SimpleSegment) extends SimpleSegment {

  val operator:String
  override def createCode(writer:CodeWriter):SegmentReturn  = {
    return writer.createCode(new SimpleSegment.Code(operator) ++ in)
  }


}

object UnaryOperator  {

  def Negative(in:SimpleSegment) = new Negative(in)
  def NotLogical(in:SimpleSegment) = new NotLogical(in)
  def Not(in:SimpleSegment) = new Not(in)
  def And(in:SimpleSegment) = new And(in)
  def NotAnd(in:SimpleSegment) = new NotAnd(in)
  def Or(in:SimpleSegment) = new Or(in)


  class Negative(in:SimpleSegment) extends UnaryOperator(in) {
      override val operator = "-"
  }

  class NotLogical( in:SimpleSegment) extends UnaryOperator(in) {
      override val operator = "!"
  }

  class Not( in:SimpleSegment) extends UnaryOperator(in) {
      override val operator = "~"
  }

  class And( in:SimpleSegment) extends UnaryOperator(in) {
      override val operator = "&"
  }

  class NotAnd( in:SimpleSegment) extends UnaryOperator(in) {
      override val operator = "~&"
  }

  class Or( in:SimpleSegment) extends UnaryOperator(in) {
      override val operator = "|"
  }



}