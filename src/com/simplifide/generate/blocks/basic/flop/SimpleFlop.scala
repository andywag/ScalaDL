package com.simplifide.generate.blocks.basic.flop

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.blocks.basic.operator._
import com.simplifide.generate.generator._
import com.simplifide.generate.blocks.basic.condition.{ConditionStatementFunctional, ConditionStatement2, ConditionStatement}

/**
 * Flop which contains the structure of the flop but has a unique reset and enable
 * statement
 */
class SimpleFlop(val name1:Option[String],
					  val head:ClockControl,
					  val res:SimpleSegment,
					  val ena:SimpleSegment) extends SimpleSegment {

  override def createCode(writer:CodeWriter):SegmentReturn = {
    def resetCondition:Option[SimpleSegment] = {
       head.reset match {

      }
    }

      val body = head.reset match {
        case Some(x) => {  // If a reset exists create an initial segment
          //new ConditionStatementFunctional()
          val ifelse = new ConditionStatement()
            val condition = if (x.activeLow) new UnaryOperator.NotLogical(x) else x;
            ifelse.addClause(Some(condition), res)       // Reset Clause
            ifelse.addClause(head.enable,ena)  // Enable Clause
            ifelse
        }
        case None => {     // If there isn't a reset create the enable statement
          head.enable match {
            case Some(x) => {
               val ifelse = new ConditionStatement()
               ifelse.addClause(Some(x),ena)
               ifelse
            }
            case None    => ena
          }

        }
      }

      val fl = new TopFlop(name1,head,body)
      writer.createCode(fl)
  }


   private def createEnable:SimpleSegment = {
      val clkTick = Operators.Tick(head.clock,"event")
      val clkOne  = new BinaryOperator.EQ(head.clock,new BasicSegments.QuoteSegment("1"))
      val cond:SimpleSegment = {
      head.enable match {
          case Some(x) => new BinaryOperator.AND(new BinaryOperator.AND(clkTick,clkOne),x)
          case None    => new BinaryOperator.AND(clkTick,clkOne)
          }
      }
      return cond
   }



}
