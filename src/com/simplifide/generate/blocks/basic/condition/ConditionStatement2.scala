package com.simplifide.generate.blocks.basic.condition

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.util.StringOps
import scala.collection.mutable.ListBuffer
import com.simplifide.generate.generator._
import com.simplifide.generate.parser.condition.Condition
import com.simplifide.generate.parser.model.Expression

class ConditionStatement2 extends SimpleSegment with Condition {

  //val statements = new ListBuffer[ConditionStatement.First]();

  implicit def Expression2SimpleSegment(expression:Expression) = expression.asInstanceOf[SimpleSegment]
  /** Add a condition statement */
  override def elseIf(cond:Expression)(states:List[Expression]) {
    val condition = new ConditionStatement2.Middle(cond.asInstanceOf[SimpleSegment],
      BasicSegments.List(states.map(_.asInstanceOf[SimpleSegment])));
    statements.append(condition)
  }
  /** Add the final else statement */
  override  def els(states:List[Expression]) {
    val condition = new ConditionStatement2.Last(BasicSegments.List(states.map(_.asInstanceOf[SimpleSegment])))
    statements.append(condition)
  }

   override def split:List[SimpleSegment] = {
    val lis =  statements.toList.flatMap(_.split).map(_.asInstanceOf[SimpleSegment])
    return  lis
   }

  override def createCode(writer:CodeWriter):SegmentReturn = {
    val st = this.statements.toList.map(x => x.asInstanceOf[SimpleSegment])
    SegmentReturn.combineFinalReturns(writer,st,List())
  }



}

object ConditionStatement2 {

  def apply(cond:SimpleSegment,statements:List[SimpleSegment]) = {
    val condition = new ConditionStatement2()
    val first = new First(cond,BasicSegments.List(statements))
    condition.statements.append(first)
    condition
  }



  class First(condition:SimpleSegment,body:SimpleSegment) extends SimpleSegment {

   override def split:List[SimpleSegment] = {
    return List(new First(condition,BasicSegments.ListExpression(body.split)))
   }

   override def createCode(writer:CodeWriter):SegmentReturn =  {
      val build = new StringBuilder();
      build.append("if (");
      build.append(writer.createCode(condition).code);
      build.append(") begin\n")
      build.append(StringOps.indentLines(writer.createCode(body).code, 1))
      build.append("end\n");
      return SegmentReturn.segment(build.toString)
    }

  }

  class Middle(condition:SimpleSegment,body:SimpleSegment) extends SimpleSegment {

    override def split:List[SimpleSegment] = {
      return List(new Middle(condition,BasicSegments.ListExpression(body.split)))
    }

    override def createCode(writer:CodeWriter):SegmentReturn =  {
      val build = new StringBuilder();
      build.append("else if (");
      build.append(writer.createCode(condition).code);
      build.append(") begin\n")
      build.append(StringOps.indentLines(writer.createCode(body).code, 1))
      build.append("end\n");
      return SegmentReturn.segment(build.toString)
    }


  }

  class Last(body:SimpleSegment) extends SimpleSegment {

    override def split:List[SimpleSegment] = {
      return List(new Last(BasicSegments.ListExpression(body.split)))
    }

     override def createCode(writer:CodeWriter):SegmentReturn =  {
      val build = new StringBuilder();
      build.append("else begin\n")
      build.append(StringOps.indentLines(writer.createCode(body).code, 1))
      build.append("end\n");
      return SegmentReturn.segment(build.toString)
    }

  }

}


