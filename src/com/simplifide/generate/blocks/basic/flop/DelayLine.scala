package com.simplifide.generate.blocks.basic.flop

import collection.mutable.{ListBuffer, LinkedHashMap}
import com.simplifide.generate.generator.{SimpleSegment, CodeWriter, SegmentReturn}
import com.simplifide.generate.signal.SignalTrait

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 3/8/11
 * Time: 7:26 AM
 * To change this template use File | Settings | File Templates.
 */

class DelayLine(val clk:ClockControl,out:SignalTrait,in:SignalTrait) extends SimpleSegment {

  override def createCode(writer:CodeWriter):SegmentReturn = {
    val segments = new ListBuffer[SimpleSegment]()

    val map = new LinkedHashMap[SignalTrait,SimpleSegment]()
    out.arrayLength match {
      case 0 => map.put(out,in)
      case _ => {
        map.put(out.slice(out.arrayLength-1),in)
        for (i <- 1 until out.arrayLength-2) {
          map.put(out.slice(i),out.slice(i+1))
        }
        map.put(out.slice(0),out.slice(1))
      }
    }

    val flop = SimpleFlopList.newFlop(clk,map)
    segments.append(flop)
    return SegmentReturn.combineReturns(writer,segments)

  }

}