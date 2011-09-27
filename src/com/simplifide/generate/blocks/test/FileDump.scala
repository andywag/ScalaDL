package com.simplifide.generate.blocks.test

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.blocks.basic.flop.{SimpleFlop, ClockControl}
import com.simplifide.generate.generator.{BasicSegments, SegmentReturn, CodeWriter, SimpleSegment}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 9/26/11
 * Time: 8:26 PM
 * To change this template use File | Settings | File Templates.
 */

class FileDump  {

}

object FileDump {

  class FOpen(val filename:String, val fptr:SignalTrait) extends SimpleSegment {
    override def createCode(writer:CodeWriter):SegmentReturn =
      writer.createCode(fptr) + " = $fopen(" + filename + ",\"w\");\n"
  }

  class FDisplay(val fptr:SignalTrait,signals:List[SignalTrait]) extends SimpleSegment {
    override def createCode(writer:CodeWriter):SegmentReturn = {
      def createQuotes(len:Int):String = {
        def vals:String = List.fill(len)("%d ").reduceLeft(_+_)
        "\"" + vals + "\""
      }
      def createSignals:String = {
        def createSignal(signal:SignalTrait, index:Int) = if (index == 0) signal.name else "," + signal.name
        signals.zipWithIndex.map(x => createSignal(x._1,x._2)).reduceLeft(_ + _)
      }
      SegmentReturn.segment("$fdisplay(") + fptr.name + "," + createQuotes(signals.length) + "," + createSignals + ");\n"
    }
  }

  class DisplayFlop(val fptr:SignalTrait,signals:List[SignalTrait])(implicit clk:ClockControl) extends SimpleSegment {
    override def createCode(writer:CodeWriter):SegmentReturn = {
      val flop = new SimpleFlop(None,clk,BasicSegments.List(List()),new FDisplay(fptr,signals))
      writer.createCode(flop)
    }
  }


}