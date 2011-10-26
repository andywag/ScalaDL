package com.simplifide.generate.parser

import collection.mutable.ListBuffer
import model.{Model, Expression}
import com.simplifide.generate.generator.CodeWriter.Fixed
import com.simplifide.generate.language.SignalFactory

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 6/21/11
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */

trait SignalParser  {


  def  R(expression:Expression,fixed:Model.Fixed=Model.NoFixed,internal:Model.Fixed  = Model.NoFixed):Expression =
    SignalFactory.round(expression,fixed,internal)
  def RC(expression:Expression,fixed:Model.Fixed=Model.NoFixed,internal:Model.Fixed = Model.NoFixed):Expression =
    SignalFactory.roundClip(expression,fixed,internal)
  def  T(expression:Expression,fixed:Model.Fixed=Model.NoFixed,internal:Model.Fixed  = Model.NoFixed):Expression =
    SignalFactory.truncate(expression,fixed,internal)
  def TC(expression:Expression,fixed:Model.Fixed=Model.NoFixed,internal:Model.Fixed = Model.NoFixed):Expression =
    SignalFactory.truncateClip(expression,fixed,internal)





}

object SignalParser {

  def signal(name:String):String = name

  implicit def Integer2Expression(value:Int):Expression = new Model.IntegerExpression(value)
  implicit def Double2Expression(value:Double):Expression = new Model.DoubleExpression(value)

}