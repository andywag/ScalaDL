package com.simplifide.generate.parser

import condition.{Condition}
import model.{Clock, Expression}
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.blocks.basic.flop.{SimpleFlop, ClockControl}
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.language.FlopFactory
import collection.mutable.Stack
import com.simplifide.generate.blocks.basic.condition.{ConditionStatementBuilder, ConditionStatement, NewCaseStatement}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/14/11
 * Time: 3:58 PM
 * To change this template use File | Settings | File Templates.
 */


/**
 * Parser section which handles the parsing of condition statements and case statements as well as always
 * blocks
 **/
trait ConditionParser extends BaseParser {

  var baseCondition:ConditionStatementBuilder = null



  private def removeExpressions(values:List[Expression]) {
    val indexes = values.map(x => this.statements.findIndexOf(x == _)).sortBy(-_)
    indexes.foreach(x => if (x >= 0) this.statements.remove(x))
  }


  /** Creation of Always Block containing the expressions given by expressions */
  def $always_star(expressions:Expression*) {
    val expr = expressions.toList.filter(_ != null)
    removeExpressions(expr)
    val always = ObjectFactory.AlwaysStar(expr)
    scope.assign(always)
  }

  /** Creation of Always Block with a sensitivity list containing the expressions given by expressions */
  def $always(sensitivity:Expression*)(expressions:Expression*) {
    val expr = expressions.toList.filter(_ != null)
    removeExpressions(expr)
    val always = ObjectFactory.Always(sensitivity.toList)(expr)
    scope.assign(always)
  }

  /** Create a flop without a reset
   *  TODO Need to clean up the expression
   * */
  def flop(expressions:Expression*)(implicit clk:ClockControl):SimpleSegment = {
    val statements = expressions.map(_.asInstanceOf[SimpleStatement])
    val flo =  FlopFactory.simpleFlopList(statements.toList)
    this.assign(flo)
    flo
  }
  /** Create a flop */
  def $flop(head:Clock)(reset:Expression*)(expressions:Expression*):Expression = {
    null
  }

  /** Creates a case statement.  */
  def $case(condition:Expression)(statements:Expression*):Expression = {
    NewCaseStatement(condition.asInstanceOf[SimpleSegment],
      statements.toList.map(NewCaseStatement.Item(_)))

  }


  /** Creation of the condition statement */
  def $if(statements:Expression)(values:Expression*):Expression = {
    baseCondition = ObjectFactory.ConditionIf(statements)(values.toList)
    this.statements.append(baseCondition)
    baseCondition

  }
  /** Else Clause for the Condition Statement */
  def $else_if(condition:Expression)(values:Expression*):Expression = {
    removeExpressions(values.toList)
    baseCondition.elseIf(condition)(values.toList)
    null
  }
  /** Default Else clause for the condition statement */
  def $else(values:Expression*):Expression = {
    removeExpressions(values.toList)
    baseCondition.els(values.toList)
    null
  }
}

object ConditionParser {

}