package com.simplifide.generate.parser

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/12/11
 * Time: 2:31 PM
 * To change this template use File | Settings | File Templates.
 */

trait Expression {

    // Unary Operators
    def unary_- : Expression = new Adder.NegativeTerm(this)

    // Binary Operators
    def - (rhs:Expression):Expression = new Adder(this,rhs)
    def + (rhs:Expression):Expression = new Adder(this,rhs)
    def * (rhs:Expression):Expression = new Multiplier(this,rhs)

    // Conditional Operators
    //def ? (rhs:Expression):Expression
    def ?  (rhs:Expression)               = new Question.Temp(this,rhs)
    def :: (rhs:Expression):Expression    = Question.Item(this, rhs)

    def split:List[Statement] = List()
    def split(output:Signal,index:Int):ExpressionReturn = new ExpressionReturn(this,List())

}