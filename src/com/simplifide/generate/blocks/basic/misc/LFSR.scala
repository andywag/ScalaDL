package com.simplifide.generate.blocks.basic.misc

import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.blocks.basic.operator.BinaryOperator
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.signal._



/** Linear Feedback Shift Register
 *
 *  @constructor Constructor for the Linear Feedback Shift Register
 *  @param output Output Signal
 *  @param poly Polynomial for the shift register larger number is closer to output
 *  @param length of the shift register
 *  @param init Initial Value of the Shift Register
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