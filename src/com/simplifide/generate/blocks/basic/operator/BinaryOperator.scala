package com.simplifide.generate.blocks.basic.operator

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 2/14/11
 * Time: 8:23 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class BinaryOperator(val in1:SimpleSegment,val in2:SimpleSegment) extends SimpleSegment {

  val operator:String
  override def createCode(writer:CodeWriter):SegmentReturn  = {
      return writer.createCode(in1 ++ operator ++ in2)
  }


}

object BinaryOperator  {

  def And(in1:SimpleSegment,in2:SimpleSegment)        = new And(in1,in2)
  def LogicalAnd(in1:SimpleSegment,in2:SimpleSegment) = new LogicalAnd(in1,in2)
  def GT(in1:SimpleSegment,in2:SimpleSegment) = new GT(in1,in2)
  def GTE(in1:SimpleSegment,in2:SimpleSegment) = new GTE(in1,in2)
  def LT(in1:SimpleSegment,in2:SimpleSegment) = new LT(in1,in2)
  def LTE(in1:SimpleSegment,in2:SimpleSegment) = new LTE(in1,in2)
  def EQ(in1:SimpleSegment,in2:SimpleSegment) = new EQ(in1,in2)
  def NEQ(in1:SimpleSegment,in2:SimpleSegment) = new NEQ(in1,in2)
  def Plus(in1:SimpleSegment,in2:SimpleSegment) = new Plus(in1,in2)

  class And( in1:SimpleSegment, in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " & "
  }

  class LogicalAnd( in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " && "
  }

  class GT(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " > "
  }
  class GTE(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " >= "
  }
  class LT(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " < "
  }
  class LTE(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " <= "
  }
  class EQ(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " == "
  }
  class NEQ(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " != "
  }

  class Plus(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " + "
  }

  class Minus(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " - "
  }

  class Or(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " | "
  }

  class Xor(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " ^ "
  }



}