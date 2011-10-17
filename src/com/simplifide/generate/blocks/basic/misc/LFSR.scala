package com.simplifide.generate.blocks.basic.misc

import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.blocks.basic.operator.BinaryOperator
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.parser.ExpressionReturn
import com.simplifide.generate.signal.Signing.UnSigned
import com.simplifide.generate.signal._

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 10/10/11
 * Time: 10:14 AM
 * To change this template use File | Settings | File Templates.
 */

class LFSR(val output:SignalTrait,
           val poly:List[Int],
           val length:Int,
           val init:List[Int])(implicit clk:ClockControl) extends SimpleSegment.Combo {

  override def createCode(writer:CodeWriter):SegmentReturn = {
    val input = SignalTrait("dline_input")
    val reg   = RegisterTrait(input,length,clk)

    val bin = poly.map(x => reg(x)).reduceLeft[SimpleSegment](BinaryOperator.XOR(_,_))
    val dline = reg.createFlop(init.map(Constant(_,FixedType.unsigned(1,0))))
    val ass1 = new SimpleStatement.Assign(input,bin)
    val ass2 = new SimpleStatement.Assign(output,reg(length))

    return writer.createCode(dline) + writer.createCode(ass1) + writer.createCode(ass2) + new SegmentReturn("",List(),List(),List(input,reg))
  }

}

object LFSR {

}