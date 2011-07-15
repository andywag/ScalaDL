package com.simplifide.generate.parser

import collection.mutable.ListBuffer

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 6/21/11
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */

class SignalParser extends ConditionParser {


  def R(expression:Expression,fixed:Model.Fixed):Expression = round(expression,fixed)
  def RC(expression:Expression,fixed:Model.Fixed):Expression = roundClip(expression,fixed)
  def T(expression:Expression,fixed:Model.Fixed):Expression = truncate(expression,fixed)
  def TC(expression:Expression,fixed:Model.Fixed):Expression = truncateClip(expression,fixed)

  def round(expression:Expression,fixed:Model.Fixed):Expression = {
    expression match {
      case Adder(x,y)      => return new Adder.Round(x,y,fixed)
      case Multiplier(x,y) => return new Multiplier.Round(x,y,fixed)
    }
  }

  def roundClip(expression:Expression,fixed:Model.Fixed):Expression = {
    expression match {
      case Adder(x,y)      => return new Adder.RoundClip(x,y,fixed)
      case Multiplier(x,y) => return new Multiplier.RoundClip(x,y,fixed)
    }
  }

  def truncate(expression:Expression,fixed:Model.Fixed):Expression = {
    expression match {
      case Adder(x,y)      => return new Adder.Truncation(x,y,fixed)
      case Multiplier(x,y) => return new Multiplier.Truncation(x,y,fixed)
    }
  }

  def truncateClip(expression:Expression,fixed:Model.Fixed):Expression = {
    expression match {
      case Adder(x,y)      => return new Adder.TruncationClip(x,y,fixed)
      case Multiplier(x,y) => return new Multiplier.TruncationClip(x,y,fixed)
    }
  }


  case class MathFunction(lhs:Expression) {
     def +(rhs:Expression):Expression = new Adder(lhs,rhs)
     def *(rhs:Expression):Expression = new Multiplier(lhs,rhs)
  }




}

object SignalParser {

  def signal(name:String):String = name

  implicit def Integer2Expression(value:Int):Expression = new Model.IntegerExpression(value)
  implicit def Double2Expression(value:Double):Expression = new Model.DoubleExpression(value)

}