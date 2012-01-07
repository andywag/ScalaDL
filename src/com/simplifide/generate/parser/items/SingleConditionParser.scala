package com.simplifide.generate.parser.items

import com.simplifide.generate.blocks.basic.condition.ConditionStatement
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.generator.{BasicSegments, SimpleSegment}
import com.simplifide.generate.parser.model.{BasicExpressions, Expression}


/**
 * Parser which handles a single condition statment for a variable assignment meaning
 *
 * y := $iff (aaa) {} $else_iff {} ..
 *
 **/
trait SingleConditionParser {



  /** Beginning of Condition Statement */
  def $iff (condition:Expression) = {
    new SingleConditionParser.Open(List(),Some(condition)) // Creates a Single Condition
  }

}


object SingleConditionParser {

  /** If Condition Statement */
  class IfStatement (val condition:Option[Expression], val result:Expression) extends Expression {

    /** Convert this segment to an output statement */
    private def createInternalSegment(state:SimpleSegment,index:Int) = {
      if (index == 0) ConditionStatement.First(condition.get.create,List(state))
      else condition match {
        case Some(x) => ConditionStatement.Middle(x.create,List(state))
        case None    => ConditionStatement.Last(List(state))
      }
    }
    
    def createSegment(index:Int) = {
      createInternalSegment(result.create,index)
    }

    /** Convert this segment to an output statement */
    def createSegment(output:Expression,index:Int) = {
      val state = result.create(output)//new SimpleStatement.Reg(output,result)
      createInternalSegment(state,index)
    }

  }

  class Open(val statements:List[IfStatement], val condition:Option[Expression]) {
    /** Closes out the condition statement with the results condition */
    def $then(result:Expression*) = new Close(statements ::: List(new IfStatement(condition,BasicExpressions.List(result.toList))))

  }


  class Close(val statements:List[IfStatement]) extends Expression with RegisterAtParser {
    /** Creates an else condition */
    def $else_if (condition:Expression) = new Open(statements,Some(condition))
    /** Creates an else condition */
    def $else (result:Expression*)       = new Close(statements ::: List(new IfStatement(None,BasicExpressions.List(result.toList))))
    /** Create a flop from this condition */
    //def $at(clk:ClockControl)

    override def create:SimpleSegment =
      new ConditionStatement(statements.zipWithIndex.map(x => x._1.createSegment(x._2)))

    override def create(lhs:Expression):SimpleSegment =
      new ConditionStatement(statements.zipWithIndex.map(x => x._1.createSegment(lhs,x._2)))

    def createFlop(output:Expression):SimpleSegment =
      new ConditionStatement(statements.zipWithIndex.map(x => x._1.createSegment(output,x._2)))
    

  }
  


}