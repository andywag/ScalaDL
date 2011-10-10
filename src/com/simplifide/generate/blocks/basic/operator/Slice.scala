package com.simplifide.generate.blocks.basic.operator

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/26/11
 * Time: 3:25 PM
 * To change this template use File | Settings | File Templates.
 */

class Slice(val signal:SignalTrait, val index:SimpleSegment) extends SimpleSegment {

  override def createCode(writer:CodeWriter):SegmentReturn = {
    writer.createCode(signal) + "[" + writer.createCode(index) + "]"
  }
}