package com.simplifide.generate.parser

import collection.mutable.ListBuffer
import math.{Multiplier, Adder}
import model.{Model, Expression}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 6/21/11
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */

class SignalParser extends ConditionParser {


  def  R(expression:Expression,fixed:Model.Fixed,internal:Option[Model.Fixed]  = None):Expression = round(expression,fixed,internal)
  def RC(expression:Expression,fixed:Model.Fixed,internal:Option[Model.Fixed] = None):Expression = roundClip(expression,fixed,internal)
  def  T(expression:Expression,fixed:Model.Fixed,internal:Option[Model.Fixed]  = None):Expression = truncate(expression,fixed,internal)
  def TC(expression:Expression,fixed:Model.Fixed,internal:Option[Model.Fixed] = None):Expression = truncateClip(expression,fixed,internal)

  def round(expression:Expression,fixed:Model.Fixed,internal:Option[Model.Fixed]  = None):Expression = {
    expression match {
      case Adder(name,x,y,sign) => return ObjectFactory.AdderRound(x, y, sign, fixed, internal)
      case Multiplier(x,y)      => return ObjectFactory.MultRound(x,y,fixed)
      case _                    => return ObjectFactory.RoundInt(expression,fixed)
    }
  }

  def roundClip(expression:Expression,fixed:Model.Fixed,internal:Option[Model.Fixed]  = None):Expression = {
    expression match {
      case Adder(name,x,y,sign)      => return ObjectFactory.AdderRoundClip(x,y,sign, fixed, internal)
      case Multiplier(x,y)      => return ObjectFactory.MultRoundClip(x,y,fixed)
      case _                    => return ObjectFactory.RoundClip(expression,fixed)
    }
  }

  def truncate(expression:Expression,fixed:Model.Fixed,internal:Option[Model.Fixed]  = None):Expression = {
    expression match {
      case Adder(name,x,y,sign)      => return  ObjectFactory.AdderTrunc(x,y,sign,fixed,internal)
      case Multiplier(x,y)      => return ObjectFactory.MultTrunc(x,y,fixed)
      case _                    => return ObjectFactory.Truncate(expression,fixed)
    }
  }

  def truncateClip(expression:Expression,fixed:Model.Fixed,internal:Option[Model.Fixed]  = None):Expression = {
    expression match {
      case Adder(name,x,y,sign)    => return ObjectFactory.AdderTruncClip(x,y,sign,fixed,internal)
      case Multiplier(x,y)         => return ObjectFactory.MultTruncClip(x,y,fixed)
      case _                       => return ObjectFactory.TruncateClip(expression,fixed)
    }
  }


  case class MathFunction(lhs:Expression) {
     def +(rhs:Expression):Expression = ObjectFactory.Adder(lhs,rhs)
     def *(rhs:Expression):Expression = ObjectFactory.Mult(lhs,rhs)
  }




}

object SignalParser {

  def signal(name:String):String = name

  implicit def Integer2Expression(value:Int):Expression = new Model.IntegerExpression(value)
  implicit def Double2Expression(value:Double):Expression = new Model.DoubleExpression(value)

}