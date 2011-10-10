package com.simplifide.generate.parser.condition

import collection.mutable.ListBuffer
import com.simplifide.generate.parser.model.Expression

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/14/11
 * Time: 3:43 PM
 * To change this template use File | Settings | File Templates.
 */

trait Condition extends Expression{
  val statements = new ListBuffer[Expression]()

  def elseIf(cond:Expression)(states:List[Expression]) {
    val condition = new Condition.Middle(cond,states)
    statements.append(condition)
  }
  /** Add the final else statement */
  def els(states:List[Expression]) {
    val condition = new Condition.Last(states)
    statements.append(condition)
  }


   val statementString = {
      val builder = new StringBuilder
      builder.append(" {\n")
      statements.foreach(x => builder.append("   " + x + "\n"))
      builder.append("}")
      builder.toString
    }

}

object Condition {

  def apply(first:Condition.First):Condition = {
    val condition = new ConditionImpl()
    condition.statements.append(condition)
    condition
  }

  def apply(condition:Expression,values:List[Expression]):Condition = {
    val first = new Condition.First(condition, values)
    Condition(first)
  }



  class ConditionImpl extends Condition {}

  class ConditionState(val statements:List[Expression]) extends Expression {
    val statementString = {
      val builder = new StringBuilder
      builder.append(" {\n")
      statements.foreach(x => builder.append("   " + x + "\n"))
      builder.append("}")
      builder.toString
    }
  }

  class First(val condition:Expression, statements:List[Expression]) extends ConditionState(statements) {
    //val children = new ListBuffer[Condition]()
    override def toString = {
      val builder = new StringBuilder
      builder.append("if (" + condition + ")" + statementString + "\n")
      //children.foreach(x => builder.append(x.toString))
      builder.toString
    }


  }

  class Middle(val condition:Expression, statements:List[Expression]) extends ConditionState(statements) {
    override def toString = "else if (" + condition + ")" + statementString
  }

  class Last(statements:List[Expression]) extends ConditionState(statements) {
    override def toString = "else"  + statementString
  }

}