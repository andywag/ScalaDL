package com.simplifide.generate.parser.block

import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.generator.SimpleSegment


/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/14/11
 * Time: 3:37 PM
 * To change this template use File | Settings | File Templates.
 */

trait Statement extends Expression {

  val output:Expression
  val input:Expression

    override def toString = "assign " + output + " = " + input
    /** Split this statement into a group of statements */
    override def split:List[SimpleSegment] = {
      val ret = input.split(output,-1)
      if (ret.states.size == 0) List(this.asInstanceOf[SimpleSegment]) else ret.states
    }
}

object Statement {
  def apply(output:Expression, input:Expression) = new Internal(output,input)

  class Internal(override val output:Expression,override val input:Expression) extends Statement {

  }
}