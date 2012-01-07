package com.simplifide.generate.parser.items

import com.simplifide.generate.generator.{BasicSegments, SimpleSegment}
import com.simplifide.generate.blocks.basic.flop.{SimpleFlopSegment, SimpleFlop, ClockControl}
import com.simplifide.generate.parser.model.Expression

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 12/16/11
 * Time: 9:09 AM
 * To change this template use File | Settings | File Templates.
 */

trait RegisterAtParser {

  def createFlop(output:Expression):SimpleSegment

  //def $at(clk:ClockControl) = new RegisterAtParser.Flop(this,clk)
}

object RegisterAtParser {
  
  class Flop(val register:Expression, val clk:ClockControl) extends Expression {
    override def create(output:Expression):SimpleSegment = {
      val internal = register.create(output)
      BasicSegments.List (new SimpleFlopSegment(clk,internal).split)
    }
  }
}