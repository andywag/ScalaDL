package com.simplifide.generate.parser.condition

import com.simplifide.generate.parser.model.Expression


/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/15/11
 * Time: 3:57 PM
 * To change this template use File | Settings | File Templates.
 */

class Case(val condition:Expression, val statements:List[Expression]) extends Expression {

}

object Case {
  def apply(condition:Expression, statements:List[Expression]) = new Case(condition, statements)

  case class State(condition:Expression, result:Expression) extends Expression
}