package com.simplifide.generate.language

import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.parser.model.{SignalType, Model, Clock, Expression}
import com.simplifide.generate.signal.{OpType, FixedType}
import com.simplifide.generate.language.LanguageFactory.ExpressionConversion
import scala.None

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/28/11
 * Time: 7:18 PM
 * To change this template use File | Settings | File Templates.
 */

class Conversions {

}

object Conversions {
    implicit def Expression2Segment(expression:Expression):SimpleSegment = {
    if (expression.isInstanceOf[SimpleSegment]) expression.asInstanceOf[SimpleSegment]
    else new ExpressionConversion(expression)
  }

  implicit def Clock2FlopControl(clock:Clock):ClockControl = {
    if (clock.isInstanceOf[ClockControl]) clock.asInstanceOf[ClockControl]
    else ClockControl.default
  }

  implicit def OptionModelFixed2Fixed(fixed:Option[Model.Fixed]):Option[FixedType] = {
    fixed match {
      case None    => None
      case Some(x) => Some(if (x.isInstanceOf[FixedType]) x.asInstanceOf[FixedType] else FixedType.unsigned(1,0))
    }
  }


  implicit def ModelFixed2Fixed(fixed:Model.Fixed):FixedType = {
    if (fixed.isInstanceOf[FixedType]) fixed.asInstanceOf[FixedType]
    else FixedType.None
  }

  implicit def SignalType2OpType(op:SignalType):OpType = {
    if (op.isInstanceOf[OpType]) op.asInstanceOf[OpType]
    else OpType.Signal
  }
}