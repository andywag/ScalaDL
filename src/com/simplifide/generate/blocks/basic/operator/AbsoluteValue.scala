package com.simplifide.generate.blocks.basic.operator

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.blocks.basic.condition.QuestionStatement
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}



/**
 * Absolute Value Statement. Converts a twos complement number to an absolute value
 **/
class AbsoluteValue(val signal:SignalTrait) extends SimpleSegment.Combo {

  val mux = new QuestionStatement(signal.sign,UnaryOperator.Negative(signal),signal)

  override def split:List[Expression] = {
    mux.split
  }

}