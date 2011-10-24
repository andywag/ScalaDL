package com.simplifide.generate.blocks.test

import com.simplifide.generate.blocks.basic.operator.BinaryOperator
import com.simplifide.generate.signal.Constant._
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.blocks.basic.condition.ConditionStatement._
import com.simplifide.generate.generator.{BasicSegments, SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.signal.OpType.Signal
import com.simplifide.generate.blocks.test.ClockGenerator.ClockCreate
import com.simplifide.generate.blocks.basic.flop.{ClockControl, SimpleFlop}
import com.simplifide.generate.signal.{Constant, SignalTrait}
import com.simplifide.generate.blocks.basic.operator.Operators.Slice

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/26/11
 * Time: 2:53 PM
 * To change this template use File | Settings | File Templates.
 */

class FileLoad(val signal:SignalTrait, val filename:String, val length:Int) extends SimpleSegment{

  override def createCode(writer:CodeWriter):SegmentReturn = {
      null
  }

}

object FileLoad {
  class ReadMemH(val array:SignalTrait,val filename:String) extends SimpleSegment {

    override def createCode(writer:CodeWriter):SegmentReturn =
      SegmentReturn.segment("$readmemh(\"" + filename + "\"," + array.name + ");\n")

  }

  class LoadCommand(val signal:SignalTrait,
                    val array:SignalTrait,
                    val control:SignalTrait)
                   (implicit clk:ClockControl) extends SimpleSegment {
    override def createCode(writer:CodeWriter):SegmentReturn =  {
      val reset  = new SimpleStatement.Reg(signal,Constant(0,signal.fixed))
      val enable = new SimpleStatement.Reg(signal,new Slice(array,control))
      val flop = new SimpleFlop(None,clk,reset,enable)
      writer.createCode(flop)
    }
  }

}