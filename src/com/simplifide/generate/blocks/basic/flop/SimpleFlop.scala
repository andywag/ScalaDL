package com.simplifide.generate.blocks.basic.flop

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.blocks.basic.operator._
import com.simplifide.generate.generator._
import com.simplifide.generate.blocks.basic.condition.{ConditionStatementFunctional, ConditionStatement2, ConditionStatement}
import com.simplifide.generate.blocks.basic.state.AlwaysProcess

/**
 * Flop which contains the structure of the flop but has a unique reset and enable
 * statement
 */
class SimpleFlop(val name1:Option[String],
					  val head:ClockControl,
					  val res:SimpleSegment,
					  val ena:SimpleSegment) extends SimpleSegment {

    private val resetCondition:Option[(Option[SimpleSegment],List[SimpleSegment])] = {
       head.reset match {
         case Some(x) => {
           val condition = if (x.activeLow) new UnaryOperator.NotLogical(x) else x;
           Some( ( Some(condition),List(res)) )
         }
         case None    => None
       }
    }

    private val enableCondition:Option[(Option[SimpleSegment],List[SimpleSegment])] = {
       head.enable match {
         case Some(x) => Some( (Some(x),List(ena)) )
         case None    => Some( (None,List(ena)))
       }
    }

    val flop = {
     val conditions:List[(Option[SimpleSegment],List[SimpleSegment])] =
       if (resetCondition.isDefined && enableCondition.isDefined) List(resetCondition.get,enableCondition.get)
       else if (resetCondition.isDefined) List(resetCondition.get)
       else  List(enableCondition.get)

     val conditionStatement = ConditionStatementFunctional(conditions)
     AlwaysProcess.Sensitivity(name1,conditionStatement,head.createSensitivityList().toList)
    }

    override def split:List[SimpleSegment] = flop.split




  /** No Longer in use */
  override def createCode(writer:CodeWriter):SegmentReturn = {

     val conditions:List[(Option[SimpleSegment],List[SimpleSegment])] =
       if (resetCondition.isDefined && enableCondition.isDefined) List(resetCondition.get,enableCondition.get)
       else if (resetCondition.isDefined) List(resetCondition.get)
       else  List(enableCondition.get)

     val conditionStatement = ConditionStatementFunctional(conditions)

     val alw = AlwaysProcess.Sensitivity(name1,conditionStatement,head.createSensitivityList().toList)
     return writer.createCode(alw)

      null
  }




}
