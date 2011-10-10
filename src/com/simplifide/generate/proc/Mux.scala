package com.simplifide.generate.proc

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.blocks.basic.condition.NewCaseStatement
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.parser.block.Statement
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.blocks.basic.state.AlwaysProcess.AlwaysStar
import com.simplifide.generate.parser.{SegmentHolder, ObjectFactory, ExpressionReturn}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/15/11
 * Time: 2:32 PM
 * To change this template use File | Settings | File Templates.
 */

class Mux(val ctrl:SignalTrait,val results:List[SimpleSegment]) extends SimpleSegment {

  override def controls:List[Controls] = List(Controls(ctrl,0,0))

  override def createCode(writer:CodeWriter):SegmentReturn = {
    val cas = new NewCaseStatement(ctrl,results)
    val alw = new AlwaysStar(None,cas,List())
    return alw.createCode(writer)
  }

  override def split(output:Expression,index:Int):ExpressionReturn = {

    val out    = output.copy(if (index > 0) index else 0).asInstanceOf[SimpleSegment]       // Always Split the Output
    val splits = results.zipWithIndex.map(x => x._1.split(out,x._2))  // Create the subset of expression returns
    val expressions = splits.map(x => new SimpleStatement.Reg(out,x.output))
    val cas = new NewCaseStatement(ctrl,expressions)
    val alw = new AlwaysStar(None,cas,List())

    val extra = splits.map(_.states)
    val realExtra = if (extra.length == 0) List() else extra.reduceLeft(_ ::: _)
    val cur = if (index < 0) List(new SimpleStatement.Assign(output,out)) else List()
    new ExpressionReturn(out, List(alw) :::  realExtra ::: cur)
  }

  override def createControl(actual:SimpleSegment,statements:SegmentHolder, index:Int):List[Controls] = {
    val location = this.results.indexWhere(_.controlMatch(actual,statements))
    if (location >= 0)
      List(Controls(ctrl,index,location)) //::: results(location).createControl(actual,statements,index)
    else
      List()
  }


}

object Mux {
  def apply(ctrl:SignalTrait,results:SimpleSegment*) = new Mux(ctrl,results.toList)
}