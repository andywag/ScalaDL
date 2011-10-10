package com.simplifide.generate.parser.operator

import com.simplifide.generate.parser.model.Expression


/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/15/11
 * Time: 5:52 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class UnaryOperator(val expression:Expression) extends Expression {
  val prefix:String
  override def toString =  prefix + "(" + expression + ")"

}

object UnaryOperator {

  class Bang(expression:Expression)  extends UnaryOperator(expression) {
    val prefix = "!"
  }
  class Tilda(expression:Expression) extends UnaryOperator(expression) {
    val prefix = "~"
  }
  class Plus(expression:Expression)  extends UnaryOperator(expression) {
    val prefix = "+"
  }
  class Minus(expression:Expression) extends UnaryOperator(expression) {
    val prefix = "-"
  }

}