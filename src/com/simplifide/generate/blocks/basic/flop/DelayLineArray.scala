package com.simplifide.generate.blocks.basic.flop

import collection.mutable.{ListBuffer, LinkedHashMap}
import com.simplifide.generate.generator.{SimpleSegment, CodeWriter, SegmentReturn}
import com.simplifide.generate.signal.{SignalTrait, ArrayTrait}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 3/8/11
 * Time: 7:26 AM
 * To change this template use File | Settings | File Templates.
 */

/** Only used if there is an array of 1 or greater. */
/*
class DelayLineArray(val clk:ClockControl,out:ArrayTrait,in:SignalTrait) extends StatementSegment.Simple {

  override def createCode(writer:CodeWriter):SegmentReturn = {

    val map = new LinkedHashMap[SignalTrait,SimpleSegment]()
    val uOut = out.children
    val uIn  = List(in) ::: out.children.slice(1,uOut.length)
    (uOut zip uIn).foreach(x => map.put(x._1,x._2))

    val flop = SimpleFlopList.newFlop(clk,map)
    writer.createCode(flop)

  }

}

object DelayLineArray {
}
*/