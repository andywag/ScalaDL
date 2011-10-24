package com.simplifide.generate.parser

import collection.mutable.ListBuffer
import model._
import operator.BitOperations
import com.simplifide.generate.signal.FixedType

/**
 * Parser which contains contructs for basic low level operations
 */

trait BaseParser extends SegmentHolder  {

  /** Params which is used for addition of statements */
  implicit val scope = this

  private def debugState(statement:Expression) {
    System.out.println(statement)
    statement.split.foreach(x => System.out.println("   " + x))
  }
  def debug {
    statements.foreach(System.out.println(_))
  }

  /** Concatenate the List of Expressions */
  def $cat(expressions:Expression*):Expression              =  new BitOperations.Concatenation(expressions.toList)
  /** Repeat the expression */
  def $repeat(expression:Expression, length:Int):Expression =  new BitOperations.Repeat(expression)



}

object BaseParser {

}