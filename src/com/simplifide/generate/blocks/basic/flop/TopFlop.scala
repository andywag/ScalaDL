package com.simplifide.generate.blocks.basic.flop

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.blocks.basic.state.AlwaysProcess
import com.simplifide.generate.generator.{SimpleSegment, SegmentReturn, CodeWriter, BaseCodeSegment}

/**
 *   Flop which contains a flop head and body
 */
class TopFlop(val name1:Option[String],head:ClockControl,body:SimpleSegment) extends BaseCodeSegment {
  override def createCode(writer:CodeWriter):SegmentReturn = {
     val alw = AlwaysProcess.Sensitivity(name1,body,head.createSensitivityList().toList)
     return writer.createCode(alw)
  }
  
}
