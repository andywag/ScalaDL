package com.simplifide.generate.blocks.basic.operator

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.blocks.basic.condition.SimpleMux
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 10/4/11
 * Time: 3:50 PM
 * To change this template use File | Settings | File Templates.
 */

class AbsoluteValue(val signal:SignalTrait) extends SimpleSegment.Combo {

  val mux = new SimpleMux(signal.sign,UnaryOperator.Negative(signal),signal)

  override def split:List[Expression] = {
    mux.split
  }

  override def createCode(writer:CodeWriter):SegmentReturn = {
    mux.createCode(writer)
  }



}