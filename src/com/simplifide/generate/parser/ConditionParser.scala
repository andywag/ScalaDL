package com.simplifide.generate.parser

import collection.mutable.ListBuffer
import condition.{Case, Condition}
import model.{Clock, Expression}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/14/11
 * Time: 3:58 PM
 * To change this template use File | Settings | File Templates.
 */

class ConditionParser extends BaseParser {

  var baseCondition:Condition = null

  private def removeExpressions(values:List[Expression]) {
    val indexes = values.map(x => this.statements.findIndexOf(x == _)).sortBy(-_)
    indexes.foreach(x => if (x >= 0) this.statements.remove(x))
  }


  def $always_star(expressions:Expression*) {
    val expr = expressions.toList.filter(_ != null)
    removeExpressions(expr)
    val always = ObjectFactory.AlwaysStar(expr)
    scope.assign(always)
  }

  def $always(sensitivity:Expression*)(expressions:Expression*) {
    val expr = expressions.toList.filter(_ != null)
    removeExpressions(expr)
    val always = ObjectFactory.Always(sensitivity.toList)(expr)
    scope.assign(always)
  }

  /** Create a flop without a reset */
  // TODO - Issue related to converting clock and expressions
  def $flop(head:Clock)(expressions:Expression*):Expression = {
     null
  }
  /** Create a flop */
  def $flop(head:Clock)(reset:Expression*)(expressions:Expression*):Expression = {
    null
  }

  def $case(condition:Expression)(statements:Expression*):Case = {
    val ca = new Case(condition,statements.toList)
    //this.statements.append(ca)
    ca
  }


  /** Create the Condition Statements */
  def $if(statements:Expression)(values:Expression*):Expression = {
    baseCondition = ObjectFactory.ConditionIf(statements)(values.toList)
    removeExpressions(values.toList)  // This is needed to remove the expressions from the module
    this.statements.append(baseCondition)
    baseCondition
  }

  def $else_if(condition:Expression)(values:Expression*):Expression = {
    removeExpressions(values.toList)
    baseCondition.elseIf(condition)(values.toList)
    null
  }

  def $else(values:Expression*):Expression = {
    removeExpressions(values.toList)
    baseCondition.els(values.toList)
    null
  }
}

object ConditionParser {

}