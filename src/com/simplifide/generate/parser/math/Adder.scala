package com.simplifide.generate.parser.math

import com.simplifide.generate.parser.{ObjectFactory, ExpressionReturn}
import com.simplifide.generate.parser.model.{Model, Expression}
import com.simplifide.generate.parser.block.Statement

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/12/11
 * Time: 2:40 PM
 * To change this template use File | Settings | File Templates.
 */


  case class Adder(val name:String,
                   val lhs:Expression,
                   val rhs:Expression,
                   val negative:Boolean = false) extends Expression {

    def newAdder(lhs:Expression, rhs:Expression) = new Adder(name,lhs,rhs,this.negative)

    val sign:String = if (negative) " - " else " + "
    override def toString =  lhs.toString + sign + rhs.toString

    override def split(output:Expression,index:Int):ExpressionReturn = {

       val out   = if (index == -1) output else output.copy(index) // Internal Output
       val lp    = lhs.split(out,0)
       val rp    = rhs.split(out,1)
       val adder = ObjectFactory.Statement(out,newAdder(lp.output,rp.output))

      new ExpressionReturn(out,lp.states ::: rp.states ::: List(adder)  )
    }

  }

  object Adder {

    abstract class Fixed(name:String,lhs:Expression, rhs:Expression, fixed:Model.Fixed, negative:Boolean = false) extends Adder(name,lhs,rhs,negative) {
      val prefix:String
      override def toString = prefix + fixed + "(" + lhs.toString + sign + rhs.toString + ")"
    }

    class Truncation(name:String,lhs:Expression, rhs:Expression, negative:Boolean = false,fixed:Model.Fixed,internal:Option[Model.Fixed]) extends Fixed(name,lhs,rhs,fixed,negative) {
      override val prefix = "T"
      override def newAdder(lhs:Expression, rhs:Expression) = new Truncation(name,lhs,rhs,negative,fixed,internal)
    }
    class TruncationClip(name:String,lhs:Expression, rhs:Expression, negative:Boolean = false,fixed:Model.Fixed,internal:Option[Model.Fixed]) extends Fixed(name,lhs,rhs,fixed,negative) {
      override val prefix = "TC"
      override def newAdder(lhs:Expression, rhs:Expression) = new TruncationClip(name,lhs,rhs,negative,fixed,internal)
    }
    class Round(name:String,lhs:Expression, rhs:Expression, negative:Boolean = false,fixed:Model.Fixed,internal:Option[Model.Fixed]) extends Fixed(name,lhs,rhs,fixed,negative) {
      override val prefix = "R"
      override def newAdder(lhs:Expression, rhs:Expression) = new Round(name,lhs,rhs,negative,fixed,internal)
    }
    class RoundClip(name:String,lhs:Expression, rhs:Expression, negative:Boolean = false,fixed:Model.Fixed,internal:Option[Model.Fixed]) extends Fixed(name,lhs,rhs,fixed,negative) {
      override val prefix = "RC"
      override def newAdder(lhs:Expression, rhs:Expression) = new RoundClip(name,lhs,rhs,negative,fixed,internal)
    }

    class NegativeTerm(lhs:Expression) extends Expression {
      override def toString =  "-" + "(" + lhs + ")"
    }

  }


