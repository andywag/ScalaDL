package com.simplifide.generate.blocks.basic.flop

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.blocks.basic.operator._
import com.simplifide.generate.blocks.basic.condition.ConditionStatement
import com.simplifide.generate.generator._

/**
 * Flop which contains the structure of the flop but has a unique reset and enable
 * statement
 */
class ResetEnableFlop(val name:Option[String],
					  val head:FlopControl,
					  val res:SimpleSegment,
					  val ena:SimpleSegment) extends SimpleSegment {

  override def createCode(writer:CodeWriter):SegmentReturn = {

      val body = head.getReset match {
        case Some(x) => {  // If a reset exists create an initial segment
            val ifelse = new ConditionStatement()
            val cond = if (x.activeLow) new UnaryOperator.NotLogical(x) else x;
            ifelse.addClause(Some(cond), res)       // Reset Clause
            ifelse.addClause(head.getEnable(),ena)  // Enable Clause
            ifelse
        }
        case None => {     // If there isn't a reset create the enable statement
          head.getEnable match {
            case Some(x) => {
               val ifelse = new ConditionStatement()
               ifelse.addClause(Some(x),ena)
               ifelse
            }
            case None    => ena
          }

        }
      }

      val fl = new TopFlop(name,head,body)
      writer.createCode(fl)
  }


   private def createEnable:SimpleSegment = {
      val clkTick = Operators.Tick(head.clock,"event")
      val clkOne  = new BinaryOperator.EQ(head.clock,new BasicSegments.QuoteSegment("1"))
      val cond:SimpleSegment = {
      head.enable match {
          case Some(x) => new BinaryOperator.And(new BinaryOperator.And(clkTick,clkOne),x)
          case None    => new BinaryOperator.And(clkTick,clkOne)
          }
      }
      return cond
   }

  override def createVhdlCode(writer:CodeWriter):SegmentReturn = {

      val ifelse = new ConditionStatement()
      head.getReset match {
        case Some(x) => {
            val cond = new BinaryOperator.EQ(x,BasicSegments.Ident(if (x.activeLow) "'0'" else "'1'"));
            ifelse.addClause(Some(cond), res)
        }
        case None =>
      }
      ifelse.addClause(Some(createEnable),ena)
      val fl = new TopFlop(name,head,ifelse)
      fl.createVhdlCode(writer)

  }

}
