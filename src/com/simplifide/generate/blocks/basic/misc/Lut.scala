package com.simplifide.generate.blocks.basic.misc

import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.blocks.basic.condition.NewCaseStatement
import com.simplifide.generate.signal.{Constant, SignalTrait}
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.blocks.basic.state.AlwaysProcess
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}

/**
 * Created by IntelliJ IDEA.
 * User: Andy
 * Date: 9/28/11
 * Time: 2:25 AM
 * To change this template use File | Settings | File Templates.
 */

class Lut(val output:SignalTrait,
          val condition:SimpleSegment,
          val inputs:List[SimpleSegment]
          //val creator:(Int)=>SimpleSegment,
          //val length:Int
           ) extends SimpleSegment{

  override def split:List[Expression] = {
    val values = inputs.zipWithIndex.map(x => NewCaseStatement.Item(Constant(x._2,condition.fixed.width),
      new SimpleStatement.Reg(output,x._1)))
    AlwaysProcess.Star(NewCaseStatement(condition,values)).split
  }

  /** Not Used */
  def createCode(writer:CodeWriter):SegmentReturn = {
    null
  }

}

object Lut {

  def apply(output:SignalTrait,
            condition:SimpleSegment,
            values:List[SimpleSegment]) =
    new Lut(output,condition,values)

  def apply(output:SignalTrait,
            condition:SimpleSegment,
            creator:(Int)=>SimpleSegment,
            length:Int) =
    new Lut(output,condition,List.tabulate(length)(creator(_)))

}