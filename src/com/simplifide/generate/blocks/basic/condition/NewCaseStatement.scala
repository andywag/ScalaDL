package com.simplifide.generate.blocks.basic.condition

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.util.StringOps
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.parser.condition.Case
import com.simplifide.generate.generator.{BasicSegments, SimpleSegment, CodeWriter, SegmentReturn}
import com.simplifide.generate.blocks.basic.operator.BeginEndSegment


/** Case Statement which doesn't include the head ie
 *  case (_)
 *
 *  endcase
 *
 *  @constructur
 *  @parameter condition Condition for the case statement
 *  @parameter statements List of statements for this case statement -- Should be NewCaseStatement.Item
 *
 **/
class NewCaseStatement(val condition:SimpleSegment, val statements:List[SimpleSegment]) extends SimpleSegment{

  override def split:List[Expression] =   {
    val states =statements.flatMap(_.split).map(_.asInstanceOf[SimpleSegment])
    List(new NewCaseStatement(condition,
      states))
  }

  
  def createCode(writer:CodeWriter):SegmentReturn =  {
    def body = statements.map(x => writer.createCode(x)).reduceLeft(_+_)
    SegmentReturn("case(") + writer.createCode(condition) + ")\n" ++ body + "endcase\n"
  }


}


/** Factory Methods and classes used for the case construction */
object NewCaseStatement {

  /** Case Statement Constructor */
  def apply(condition:SimpleSegment, statements:List[SimpleSegment]) = {
    new NewCaseStatement(condition,statements)
  }



  class Item(val condition:Option[SimpleSegment],result:SimpleSegment) extends SimpleSegment {

    override def split:List[Expression] = {
      val results = result.split.map(_.asInstanceOf[SimpleSegment])
      val res = if (results.length > 0) new BeginEndSegment(results) else results(0)

      List(new Item(condition,res))

    }

    def createCode(writer:CodeWriter):SegmentReturn = {
      def conditionExpression = writer.createCode(condition.getOrElse(BasicSegments.Ident("default")))

      conditionExpression + " : " + writer.createCode(result)
    }



  }

  /** Factory Methods for Creating a case Item */
  object Item {
    /** Method to create a case item from an condition and a result */
    def apply(condition:SimpleSegment, result:SimpleSegment) = new Item(Some(condition), result)
    /** Method to create a case item called from the parser. It either takes a case method or a segment */
    def apply(expression:Expression) =  {
      def state(cas:Case.State) = new Item(Some(cas.condition.asInstanceOf[SimpleSegment]), cas.result.asInstanceOf[SimpleSegment])
      if (expression.isInstanceOf[Case.State]) state(expression.asInstanceOf[Case.State])
      else if (expression.isInstanceOf[SimpleSegment]) new Item(None,expression.asInstanceOf[SimpleSegment])
      else null
    }

  }

}

