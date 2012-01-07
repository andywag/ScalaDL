package com.simplifide.generate.parser.model


/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 6/23/11
 * Time: 8:28 AM
 * To change this template use File | Settings | File Templates.
 */

class Model

object Model {

  /*
  trait Expression{
    def +  (rhs:MemoryModel.Expression):MemoryModel.Expression = new MemoryModel.Adder(this,rhs)
    def *  (rhs:MemoryModel.Expression):MemoryModel.Expression = new MemoryModel.Mult(this,rhs)
  }
  */

  trait Fixed {
      val width:Int
      val fraction:Int
      override def toString = "<" + width + "," + fraction + ">"
  }

  object NoFixed extends Fixed {
    val width = 1;
    val fraction = 0;
  }

  def Fixed(width:Int, fraction:Int) = new FixedImp(width,fraction)

  class FixedImp(override val width:Int, override val fraction:Int) extends Fixed




  class IntegerExpression(val value:Int) extends Expression {
    override def toString = value.toString
  }

  class DoubleExpression(val value:Double) extends Expression {
    override def toString = value.toString
  }

}