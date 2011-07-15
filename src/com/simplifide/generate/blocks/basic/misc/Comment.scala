package com.simplifide.generate.blocks.basic.misc

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.generator.{SimpleSegment, SegmentReturn, CodeWriter}

abstract class Comment(val text:String) extends SimpleSegment{

}

object Comment {
  class SingleLine(override val text:String) extends Comment(text) {
     
    
    
     def createItem(prefix:String):SegmentReturn = {
       return SegmentReturn.segment(prefix + " " + text + "\n")
     }

    override def createCode(writer:CodeWriter):SegmentReturn = return createItem("//")


     override def createFloatCode(writer:CodeWriter):SegmentReturn = return createItem("//")
     override def createFixedCode(writer:CodeWriter):SegmentReturn = return createItem("//")
     override def createVerilogCode(writer:CodeWriter):SegmentReturn = return createItem("//")
     override def createVhdlCode(writer:CodeWriter):SegmentReturn     = return createItem("--")
  }
}
