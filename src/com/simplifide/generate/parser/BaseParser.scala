package com.simplifide.generate.parser

import collection.mutable.ListBuffer
import model._
import operator.BitOperations
import com.simplifide.generate.signal.FixedType
import com.sun.xml.internal.fastinfoset.util.ValueArray

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/14/11
 * Time: 2:57 PM
 * To change this template use File | Settings | File Templates.
 */

class BaseParser extends SegmentHolder{

  /** Scope which is used for addition of statements */
  implicit val scope = this




  private def debugState(statement:Expression) {
    System.out.println(statement)
    statement.split.foreach(x => System.out.println("   " + x))
  }
  def debug {
    statements.foreach(System.out.println(_))
  }


  /*
  def constant(value:Double) = {
     val values = List.tabulate(32)(i => value*scala.math.pow(2.0,i-16))
     //val floors = values.map(x => scala.math.floor(x))

     val intValue = values.reverse.indexWhere(x => scala.math.floor(x) == 0)
     val fracValue = values.indexWhere(x => (x - scala.math.floor(x) == 0))

     ObjectFactory.Constant("",value,Model.Fixed(fracValue - intValue-1,fracValue - 16))

  }

  def constant(value:Double,fixed:Model.Fixed = Model.NoFixed) =
    ObjectFactory.Constant("",value,fixed)

  */





  def $cat(expressions:Expression*):Expression              =  new BitOperations.Concatenation(expressions.toList)
  def $repeat(expression:Expression, length:Int):Expression =  new BitOperations.Repeat(expression)



}

object BaseParser {

}