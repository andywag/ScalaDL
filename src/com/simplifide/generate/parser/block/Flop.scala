package com.simplifide.generate.parser.block

import com.simplifide.generate.parser.model.{Clock, Expression}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/18/11
 * Time: 3:22 PM
 * To change this template use File | Settings | File Templates.
 */

class Flop(val clk:Clock, val output:List[Expression], val reset:Option[List[Expression]],val input:List[Expression]) extends Expression{
    override def toString = {
      val builder = new StringBuilder
      builder.append("flop(" + clk + ")\n")
      reset match {
        case Some(res) => (output zip res zip input).map(x => builder.append("   " + x._1._1 + " <= " + x._1._2 + " -> " + x._2 + "\n"))
        case None      => (output zip input).map(x => builder.append("   " + x._1 + " <= " + x._2 + "\n"))
      }

      builder.toString
    }
}

object Flop {

}