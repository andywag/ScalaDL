package com.simplifide.generate.parser.model

import com.simplifide.generate.parser.block.Statement
import com.simplifide.generate.parser.operator.UnaryOperator
import com.simplifide.generate.parser.condition.{Question, Case}
import com.simplifide.generate.parser.{SegmentHolder, ExpressionReturn, ObjectFactory, BaseParser}
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.proc.ProcProgram
import com.simplifide.generate.blocks.basic.SimpleStatement

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/12/11
 * Time: 2:31 PM
 * To change this template use File | Settings | File Templates.
 */

trait Expression {

    def copy(index:Int):Expression = null


    def createExpression(rhs:Expression):Expression = {
       rhs match {
        case Case.State(Case.State(reset,input),clock) => ObjectFactory.Flop(clock.asInstanceOf[Clock],this,reset,input)
        case Case.State(input,clock) => ObjectFactory.Flop(clock.asInstanceOf[Clock],this,input)
        case _                       => ObjectFactory.Statement(this,rhs)
      }
    }
    // TODO Kludgey way to deal with conversions
    def createStatement(rhs:Expression):Option[SimpleStatement] = {
      val state = createExpression(rhs)
      if (state.isInstanceOf[SimpleStatement]) Some(state.asInstanceOf[SimpleStatement]) else None
    }

    // Statement
    def := (rhs:Expression)(implicit scope:SegmentHolder):Expression = {
      val state = createExpression(rhs)
      if (scope != null) scope.assign(state)
      state
    }
    def ::= (rhs:Expression):Expression = {
      createExpression(rhs)
    }

      // Statement



    // Unary Operators
    def unary_! : Expression = new UnaryOperator.Bang(this)
    def unary_~ : Expression = new UnaryOperator.Tilda(this)
    def unary_+ : Expression = new UnaryOperator.Plus(this)
    def unary_- : Expression = new UnaryOperator.Minus(this)
    // Binary Operators
    def - (rhs:Expression):Expression = ObjectFactory.Adder(this,rhs,true)
    def + (rhs:Expression):Expression = ObjectFactory.Adder(this,rhs)
    def * (rhs:Expression)(implicit clk:ClockControl):Expression = ObjectFactory.Mult(this,rhs)
    def / (rhs:Expression):Expression = ObjectFactory.Div(this,rhs)
    // Comparison Operators
    def < (rhs:Expression):Expression = ObjectFactory.LT(this,rhs)
    def > (rhs:Expression):Expression = ObjectFactory.GT(this,rhs)
    def <= (rhs:Expression):Expression = ObjectFactory.LTE(this,rhs)  // See <= Assign Statement
    def >= (rhs:Expression):Expression = ObjectFactory.GTE(this,rhs)
    def == (rhs:Expression):Expression = ObjectFactory.EQ(this,rhs)
    def != (rhs:Expression):Expression = ObjectFactory.NEQ(this,rhs)
    def === (rhs:Expression):Expression = ObjectFactory.EQ3(this,rhs)
    def !== (rhs:Expression):Expression = ObjectFactory.NEQ3(this,rhs)
    // Logical and Reduction Operators
    def ~ (rhs:Expression):Expression = ObjectFactory.NOT(this,rhs)
    def & (rhs:Expression):Expression = ObjectFactory.AND(this,rhs)
    def ~& (rhs:Expression):Expression = ObjectFactory.NAND(this,rhs)
    def | (rhs:Expression):Expression = ObjectFactory.OR(this,rhs)
    def ~| (rhs:Expression):Expression = ObjectFactory.NOR(this,rhs)
    def ^ (rhs:Expression):Expression = ObjectFactory.XOR(this,rhs)
    def ^~ (rhs:Expression):Expression = ObjectFactory.NXOR(this,rhs)
    def ~^ (rhs:Expression):Expression = ObjectFactory.NXOR(this,rhs)
    // Shift Operators
    def >> (rhs:Expression):Expression = ObjectFactory.SL(this,rhs)
    def << (rhs:Expression):Expression = ObjectFactory.SR(this,rhs)


    // Conditional Operators
    def ?  (rhs:Expression)               = new Question.Temp(this,rhs)
    def :: (rhs:Expression):Expression    = Question.Item(this, rhs)

    // Case Operators
    def -> (rhs:Expression):Expression    = new Case.State(this,rhs)
    // Clock Event Signal
    def @@ (rhs:Expression):Expression    = new Case.State(this,rhs)



    def split:List[Expression] = List()
    def split(output:Expression,index:Int):ExpressionReturn = {
      new ExpressionReturn(this,List())
    }

}