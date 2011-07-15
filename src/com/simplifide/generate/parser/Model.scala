package com.simplifide.generate.parser

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
    def +  (rhs:Model.Expression):Model.Expression = new Model.Adder(this,rhs)
    def *  (rhs:Model.Expression):Model.Expression = new Model.Mult(this,rhs)
  }
  */

  class Fixed(val width:Int, val fraction:Int) {
      override def toString = "<" + width + "," + fraction + ">"
  }

  class Clock(val name:String, val delay:Int = 0) extends Expression {
     def -(rhs:Int) = new Clock(name, rhs)
     override def toString = name + (if (delay != 0) "-" + delay else "")

  }

  /*
  class Sig(val name:String, val scope:SignalParser) extends Expression {
    override def toString = name
    def apply(clk:Clock) = new Sig(name,scope)
    def apply(top:Int,bot:Int) = new Sig(this.name,scope)

    def <= (rhs:Expression):Statement = {
      val state = new Statement(this,rhs)
      scope.assign(state)
      state
    }

        // Unary Operators
    override def unary_- : Expression = new Adder.NegativeTerm(this)
  }
  */



  /*
  class Adder(val lhs:Expression, rhs:Expression) extends Expression {
    override def toString =  lhs.toString + " + " + rhs.toString
  }
  */

  case class Mult(val lhs:Expression, rhs:Expression) extends Expression {
    override def toString = lhs.toString + " * " + rhs.toString
  }

  class RoundMult(val lhs:Expression, rhs:Expression, internal:Fixed) extends Expression {
    override def toString = "roundMult(" + lhs + "," + rhs + "," + internal + ")"
  }

  class Round(val lhs:Expression, fixed:Model.Fixed) extends Expression {
    override def toString = "round(" + lhs + "," + fixed +  ")"
  }

  class IntegerExpression(val value:Int) extends Expression {
    override def toString = value.toString
  }

  class DoubleExpression(val value:Double) extends Expression {
    override def toString = value.toString
  }

}