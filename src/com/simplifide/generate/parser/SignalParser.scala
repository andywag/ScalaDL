package com.simplifide.generate.parser

import collection.mutable.ListBuffer
import math.{Multiplier, Adder}
import model.{Model, Expression}
import javax.xml.ws.Service.Mode
import com.simplifide.generate.generator.CodeWriter.Fixed

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 6/21/11
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */

class SignalParser extends ConditionParser {


  def  R(expression:Expression,fixed:Model.Fixed=Model.NoFixed,internal:Model.Fixed  = Model.NoFixed):Expression =
    round(expression,fixed,internal)
  def RC(expression:Expression,fixed:Model.Fixed=Model.NoFixed,internal:Model.Fixed = Model.NoFixed):Expression =
    roundClip(expression,fixed,internal)
  def  T(expression:Expression,fixed:Model.Fixed=Model.NoFixed,internal:Model.Fixed  = Model.NoFixed):Expression =
    truncate(expression,fixed,internal)
  def TC(expression:Expression,fixed:Model.Fixed=Model.NoFixed,internal:Model.Fixed = Model.NoFixed):Expression =
    truncateClip(expression,fixed,internal)

  def round(expression:Expression,fixed:Model.Fixed,internal:Model.Fixed = Model.NoFixed):Expression = {
    expression match {
      case Adder(name,x,y,sign) => return ObjectFactory.AdderRound(x, y, sign, fixed, internal)
      case Multiplier(x,y)      => return ObjectFactory.MultRound(x,y,fixed,internal)
      case _                    => return ObjectFactory.RoundInt(expression,fixed,internal)
    }
  }

  def roundClip(expression:Expression,fixed:Model.Fixed,internal:Model.Fixed  = Model.NoFixed):Expression = {
    expression match {
      case Adder(name,x,y,sign) => return ObjectFactory.AdderRoundClip(x,y,sign, fixed, internal)
      case Multiplier(x,y)      => return ObjectFactory.MultRoundClip(x,y,fixed,internal)
      case _                    => return ObjectFactory.RoundClip(expression,fixed,internal)
    }
  }

  def truncate(expression:Expression,fixed:Model.Fixed,internal:Model.Fixed  = Model.NoFixed):Expression = {
    expression match {
      case Adder(name,x,y,sign) => return  ObjectFactory.AdderTrunc(x,y,sign,fixed,internal)
      case Multiplier(x,y)      => return ObjectFactory.MultTrunc(x,y,fixed,internal)
      case _                    => return ObjectFactory.Truncate(expression,fixed,internal)
    }
  }

  def truncateClip(expression:Expression,fixed:Model.Fixed,internal:Model.Fixed  = Model.NoFixed):Expression = {
    expression match {
      case Adder(name,x,y,sign)    => return ObjectFactory.AdderTruncClip(x,y,sign,fixed,internal)
      case Multiplier(x,y)         => return ObjectFactory.MultTruncClip(x,y,fixed,internal)
      case _                       => return ObjectFactory.TruncateClip(expression,fixed,internal)
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