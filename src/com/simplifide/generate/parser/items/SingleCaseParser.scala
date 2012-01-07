package com.simplifide.generate.parser.items

import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.blocks.basic.state.Always
import com.simplifide.generate.blocks.basic.condition.{NewCaseStatement, ConditionStatement}
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.parser.model.{BasicExpressions, Expression}
import com.simplifide.generate.parser.items.SingleCaseParser.Close


/**
 * Parser class which contains the handling for case statements
 */

trait SingleCaseParser {
  def $cases(condition:Expression) = new SingleCaseParser.Open(List(),Some(condition))

}

object SingleCaseParser {

  def Top(baseCondition:Expression,statements:List[Close]) = {
    
  }
  
  /** Individual case Class */
  class Case(val condition:Option[Expression],  val result:Expression) extends Expression {

    override def create = NewCaseStatement.Item(condition,
      result.create)

    override def create(output:Expression) = NewCaseStatement.Item(condition,
      result.create(output))
  }
  /** Open Case Class - Case without result */
  class Open(cases:List[Case],val condition:Option[Expression]) {
    def $then(result:Expression*) = new Close(cases ::: List(new Case(condition,BasicExpressions.List(result.toList))))
    def $then(result:List[Expression]) = new Close(cases ::: List(new Case(condition,BasicExpressions.List(result))))

  }
  /** Closed Case Class */
  class Close(val cases:List[Case]) {
    def $cases(condition:Expression) = new Open(cases,Some(condition))
    def $default(result:Expression*) = new Close(cases ::: List(new Case(None,BasicExpressions.List(result.toList))))
    def + (close:Close)              = new Close(cases ::: close.cases)
  }

  /** Main class for the case statement. Called with the $match statement */
  class Top(baseCondition:Expression, statement:Close) extends Expression  {
    
    override def create:SimpleSegment =
      new NewCaseStatement(baseCondition.asInstanceOf[SimpleSegment],statement.cases.map(_.create))

    override def create(lhs:Expression):SimpleSegment =
      new NewCaseStatement(baseCondition.asInstanceOf[SimpleSegment],statement.cases.map(_.create(lhs)))


  }
  

}
