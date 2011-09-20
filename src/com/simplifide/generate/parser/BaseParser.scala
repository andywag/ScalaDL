package com.simplifide.generate.parser

import collection.mutable.ListBuffer
import model._
import operator.BitOperations
import com.simplifide.generate.signal.FixedType

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/14/11
 * Time: 2:57 PM
 * To change this template use File | Settings | File Templates.
 */

class BaseParser extends SegmentHolder with InstanceHolder {

  /** Params which is used for addition of statements */
  implicit val scope = this




  private def debugState(statement:Expression) {
    System.out.println(statement)
    statement.split.foreach(x => System.out.println("   " + x))
  }
  def debug {
    statements.foreach(System.out.println(_))
  }

  def $cat(expressions:Expression*):Expression              =  new BitOperations.Concatenation(expressions.toList)
  def $repeat(expression:Expression, length:Int):Expression =  new BitOperations.Repeat(expression)



}

object BaseParser {

}