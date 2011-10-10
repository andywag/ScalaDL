package com.simplifide.generate.parser.condition

import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.parser.ObjectFactory


/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/13/11
 * Time: 4:16 PM
 * To change this template use File | Settings | File Templates.
 */

class Question(val condition:Expression, val tru:Expression, val fal:Expression) extends Expression {
  override def toString = condition  + " ? " + tru + " : " + fal

}

object Question {

  def Item(tru:Expression, fal:Expression):Expression = {
    fal match {
      case Question.Temp(x,y) => return ObjectFactory.Question(x,y,tru)
      case _                  => return new Question.Item(tru,fal)
    }
  }

  case class Temp(val condition:Expression, val tru:Expression) extends Expression {
    override def :: (rhs:Expression):Question  = new Question(this.condition, this.tru, rhs)
  }

  class Item(val tru:Expression, val fal:Expression) extends Expression {
    override def toString =  tru + " : " + fal
  }

}