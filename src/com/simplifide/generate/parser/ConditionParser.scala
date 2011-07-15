package com.simplifide.generate.parser

import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/14/11
 * Time: 3:58 PM
 * To change this template use File | Settings | File Templates.
 */

class ConditionParser extends BaseParser {

  var baseCondition:Condition.First = null

  /** Create the Condition Statements */
  def v_if(statements:Expression)(values:Expression*):Expression = {
    baseCondition = new Condition.First(statements,values.toList)
    this.statements.append(baseCondition)
    baseCondition
  }

  def v_else_if(statements:Expression)(values:Expression*):Expression = {
    val condition = new Condition.Middle(statements,values.toList)
    baseCondition.children.append(condition)
    condition
  }

  def v_else(values:Expression*):Expression = {
    val condition = new Condition.Last(values.toList)
    baseCondition.children.append(condition)
    condition
  }
}

object ConditionParser {

}