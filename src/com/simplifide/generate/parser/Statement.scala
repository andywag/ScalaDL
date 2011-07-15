package com.simplifide.generate.parser

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/14/11
 * Time: 3:37 PM
 * To change this template use File | Settings | File Templates.
 */

class Statement(val output:Signal, val input:Expression) extends Expression {
    override def toString = "assign " + output + " = " + input
    override def split:List[Statement] = {
      val ret = input.split(output,-1)
      return ret.states
    }
}

object Statement {
  class Internal(output:Signal,input:Expression) extends Statement(output,input) {

  }
}