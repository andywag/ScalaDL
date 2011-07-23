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
  def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment
  override def numberOfChildren:Int = in1.numberOfChildren
  override def child(index:Int):SimpleSegment = newSegment(in1.child(index),in2.child(index))

  override def createCode(writer:CodeWriter):SegmentReturn  = {
      return writer.createCode(in1 ++ operator ++ in2)
  }


}

object BinaryOperator  {

  def AND(in1:SimpleSegment,in2:SimpleSegment)        = new AND(in1,in2)
  def OR(in1:SimpleSegment,in2:SimpleSegment)         = new OR(in1,in2)
  def NAND(in1:SimpleSegment,in2:SimpleSegment)        = new NAND(in1,in2)
  def NOR(in1:SimpleSegment,in2:SimpleSegment)         = new NOR(in1,in2)
  def XOR(in1:SimpleSegment,in2:SimpleSegment)        = new XOR(in1,in2)
  def NXOR(in1:SimpleSegment,in2:SimpleSegment)         = new NXOR(in1,in2)

  def LogicalAnd(in1:SimpleSegment,in2:SimpleSegment) = new LogicalAnd(in1,in2)
  def GT(in1:SimpleSegment,in2:SimpleSegment) = new GT(in1,in2)
  def GTE(in1:SimpleSegment,in2:SimpleSegment) = new GTE(in1,in2)
  def LT(in1:SimpleSegment,in2:SimpleSegment) = new LT(in1,in2)
  def LTE(in1:SimpleSegment,in2:SimpleSegment) = new LTE(in1,in2)
  def EQ(in1:SimpleSegment,in2:SimpleSegment) = new EQ(in1,in2)
  def NEQ(in1:SimpleSegment,in2:SimpleSegment) = new NEQ(in1,in2)
  def EQ3(in1:SimpleSegment,in2:SimpleSegment) = new EQ3(in1,in2)
  def NEQ3(in1:SimpleSegment,in2:SimpleSegment) = new NEQ3(in1,in2)
  def NOT(in1:SimpleSegment,in2:SimpleSegment) = new NEQ3(in1,in2)
  def SL(in1:SimpleSegment,in2:SimpleSegment) = new SL(in1,in2)
  def SR(in1:SimpleSegment,in2:SimpleSegment) = new SR(in1,in2)

  def Plus(in1:SimpleSegment,in2:SimpleSegment) = new Plus(in1,in2)

  class AND( in1:SimpleSegment, in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " & "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new AND(in1,in2)
  }
  class OR( in1:SimpleSegment, in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " | "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new OR(in1,in2)
  }
  class NAND( in1:SimpleSegment, in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " ~& "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new NAND(in1,in2)
  }
  class NOR( in1:SimpleSegment, in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " ~| "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new NOR(in1,in2)
  }
  class XOR( in1:SimpleSegment, in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " ^ "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new XOR(in1,in2)
  }
  class NXOR( in1:SimpleSegment, in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " ~^ "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new NXOR(in1,in2)
  }
  class SL( in1:SimpleSegment, in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " << "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new SL(in1,in2)
  }
  class SR( in1:SimpleSegment, in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " >> "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new SR(in1,in2)
  }

  class LogicalAnd( in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " && "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new LogicalAnd(in1,in2)
  }

  class GT(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " > "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new GT(in1,in2)
  }
  class GTE(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " >= "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new GTE(in1,in2)
  }
  class LT(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " < "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new LT(in1,in2)
  }
  class LTE(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " <= "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new LTE(in1,in2)
  }
  class EQ(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " == "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new EQ(in1,in2)
  }
  class NEQ(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " != "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new NEQ(in1,in2)
  }
  class EQ3(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " === "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new EQ3(in1,in2)
  }
  class NEQ3(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " !== "
    def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new NEQ3(in1,in2)
  }
  class NOT(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " ~ "
    def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new NOT(in1,in2)
  }

  class Plus(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " + "
    def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new Plus(in1,in2)
  }

  class Minus(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " - "
    def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new Minus(in1,in2)
  }

  class Or(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " | "
    def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new Or(in1,in2)

  }

  class Xor(  in1:SimpleSegment,  in2:SimpleSegment) extends BinaryOperator(in1,in2) {
      override val operator = " ^ "
      def newSegment(in1:SimpleSegment,in2:SimpleSegment):SimpleSegment = new Xor(in1,in2)

  }



}