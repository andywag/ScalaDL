package com.simplifide.generate.blocks.basic.condition

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.generator._
import com.simplifide.generate.parser.condition.Condition

/**
 * Condition Statement -- If Else Clause
 *
 * @constructor
 * @parameters conditions : List of Condition Statements
 */
class ConditionStatementFunctional(val conditions:List[SimpleSegment]) extends SimpleSegment with Condition {


   override def split:List[SimpleSegment] = {
    val lis =  conditions.toList.flatMap(_.split).map(_.asInstanceOf[SimpleSegment])
    return  lis
   }

  override def createCode(writer:CodeWriter):SegmentReturn = {
    val st = this.conditions.toList.map(x => x.asInstanceOf[SimpleSegment])
    SegmentReturn.combineFinalReturns(writer,st,List())
  }



}

/** Factory methods and classes to aid in creation of the condition statement */
object ConditionStatementFunctional {

  /** Method for creating the condition statement based on a list of expressions.
   *
   *  The input contains a list of conditions
   **/
  def apply(conditions:List[(Option[SimpleSegment],List[SimpleSegment])]):SimpleSegment = {
    def condition(index:Int,cond:(Option[SimpleSegment],List[SimpleSegment])):SimpleSegment = {
      if (index == 0) return new First(cond._1.get,BasicSegments.ListExpression(cond._2))
      else {
        cond._1 match {
          case Some(x) => return new Middle(x,BasicSegments.ListExpression(cond._2))
          case None    => return new Last(BasicSegments.ListExpression(cond._2))
        }
      }
    }
    if (!conditions(0)._1.isDefined) BasicSegments.ListExpression(conditions(0)._2)       // No Condition Statement Condition (Occurs with Flop)
    else new ConditionStatementFunctional(conditions.zipWithIndex.map(x => condition(x._2,x._1)))
  }


  /** Class describing the first condition  */
  class First(condition:SimpleSegment,body:SimpleSegment) extends SimpleSegment {

   override def toString = "if (" + condition + ")" + body

   override def split:List[SimpleSegment] =
    return List(new First(condition,BasicSegments.ListExpression(body.split)))


   override def createCode(writer:CodeWriter):SegmentReturn =
    return SegmentReturn.segment("if (") + writer.createCode(condition) + ") begin\n" ++ writer.createCode(body) + "end\n"

  }

  class Middle(condition:SimpleSegment,body:SimpleSegment) extends SimpleSegment {

    override def split:List[SimpleSegment] = {
      return List(new Middle(condition,BasicSegments.ListExpression(body.split)))
    }

    override def createCode(writer:CodeWriter):SegmentReturn =
      return SegmentReturn.segment("else if (") + writer.createCode(condition) + ") begin \n" ++ writer.createCode(body) + "end\n"

  }

  class Last(body:SimpleSegment) extends SimpleSegment {

    override def split:List[SimpleSegment] = {
      return List(new Last(BasicSegments.ListExpression(body.split)))
    }

     override def createCode(writer:CodeWriter):SegmentReturn =
      return SegmentReturn.segment("else begin\n") ++ writer.createCode(body) + "end\n"


  }

}


