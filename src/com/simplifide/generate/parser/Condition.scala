package com.simplifide.generate.parser

import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/14/11
 * Time: 3:43 PM
 * To change this template use File | Settings | File Templates.
 */

class Condition(val statements:List[Expression]) extends Expression{
    val statementString = {
      val builder = new StringBuilder
      builder.append(" {\n")
      statements.foreach(x => builder.append("   " + x + "\n"))
      builder.append("}")
      builder.toString
    }

}

object Condition {


  class First(val condition:Expression, statements:List[Expression]) extends Condition(statements) {
    val children = new ListBuffer[Condition]()
    override def toString = {
      val builder = new StringBuilder
      builder.append("if (" + condition + ")" + statementString + "\n")
      children.foreach(x => builder.append(x.toString))
      builder.toString
    }

  }

  class Middle(val condition:Expression, statements:List[Expression]) extends Condition(statements) {
    override def toString = "else if (" + condition + ")" + statementString
  }

  class Last(statements:List[Expression]) extends Condition(statements) {
    override def toString = "else"  + statementString
  }

}