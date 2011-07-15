package com.simplifide.generate.parser

import collection.immutable.List._

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/12/11
 * Time: 2:40 PM
 * To change this template use File | Settings | File Templates.
 */


  case class Adder(val lhs:Expression, val rhs:Expression) extends Expression {
    def newAdder(lhs:Expression, rhs:Expression) = new Adder(lhs,rhs)
    override def toString =  lhs.toString + " + " + rhs.toString

    override def split(output:Signal,index:Int):ExpressionReturn = {

       val out   = if (index == -1) output else output.copy(index) // Internal Output
       val lp    = lhs.split(out,0)
       val rp    = rhs.split(out,1)
       val adder = new Statement(out,newAdder(lp.output,rp.output))

      new ExpressionReturn(out,lp.states ::: rp.states ::: List(adder)  )
    }
  }

  object Adder {

    abstract class Fixed(lhs:Expression, rhs:Expression, fixed:Model.Fixed) extends Adder(lhs,rhs) {
      val prefix:String
      override def toString = prefix + fixed + "(" + lhs.toString + " + " + rhs.toString + ")"
    }

    class Truncation(lhs:Expression, rhs:Expression, fixed:Model.Fixed) extends Fixed(lhs,rhs,fixed) {
      override val prefix = "T"
      override def newAdder(lhs:Expression, rhs:Expression) = new Truncation(lhs,rhs,fixed)
    }
    class TruncationClip(lhs:Expression, rhs:Expression, fixed:Model.Fixed) extends Fixed(lhs,rhs,fixed) {
      override val prefix = "TC"
      override def newAdder(lhs:Expression, rhs:Expression) = new TruncationClip(lhs,rhs,fixed)
    }
    class Round(lhs:Expression, rhs:Expression, fixed:Model.Fixed) extends Fixed(lhs,rhs,fixed) {
      override val prefix = "R"
      override def newAdder(lhs:Expression, rhs:Expression) = new Round(lhs,rhs,fixed)
    }
    class RoundClip(lhs:Expression, rhs:Expression, fixed:Model.Fixed) extends Fixed(lhs,rhs,fixed) {
      override val prefix = "RC"
      override def newAdder(lhs:Expression, rhs:Expression) = new RoundClip(lhs,rhs,fixed)
    }

    class NegativeTerm(lhs:Expression) extends Expression {
      override def toString =  "-" + "(" + lhs + ")"
    }

  }


