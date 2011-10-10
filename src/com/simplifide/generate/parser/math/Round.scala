package com.simplifide.generate.parser.math

import com.simplifide.generate.parser.model.{Model, Expression}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/18/11
 * Time: 5:42 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Round(val expression:Expression, val fixed:Model.Fixed) extends Expression{
  val prefix:String
  override def toString:String = prefix + "(" + expression + "," + fixed + ")"
}

object Round {

  class Truncate(expression:Expression, fixed:Model.Fixed) extends Round(expression,fixed) {
    override val prefix = "T"
  }

  class TruncateClip(expression:Expression, fixed:Model.Fixed) extends Round(expression,fixed) {
    override val prefix = "TC"
  }

  class RoundInt(expression:Expression, fixed:Model.Fixed) extends Round(expression,fixed) {
    override val prefix = "R"
  }

  class RoundClip(expression:Expression, fixed:Model.Fixed) extends Round(expression,fixed) {
    override val prefix = "RC"
  }


}