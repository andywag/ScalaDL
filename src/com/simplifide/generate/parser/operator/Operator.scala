package com.simplifide.generate.parser.operator

import com.simplifide.generate.parser.model.Expression

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/19/11
 * Time: 3:30 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Operator(lhs:Expression, rhs:Expression) extends Expression {
   val operator:String
   override def toString = lhs + " " + operator+ " " + rhs
}

