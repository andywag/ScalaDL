package com.simplifide.generate.blocks.basic.misc

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.generator.{SimpleSegment, SegmentReturn, CodeWriter}

/**
 * Class which defines a comment operation
 */
abstract class Comment(val text:String) extends SimpleSegment{

}

object Comment {
  /** Single Line Comment */
  class SingleLine(text:String) extends Comment(text)  {
    override def createCode(writer:CodeWriter):SegmentReturn =
      SegmentReturn("// " + text + "\n")
  }

  object SingleLine {
    def apply(text:String) = new SingleLine(text)
  }

}
