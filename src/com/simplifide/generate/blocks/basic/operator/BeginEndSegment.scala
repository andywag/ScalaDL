package com.simplifide.generate.blocks.basic.operator

import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}


/**
 * Set of Code Segments which are surrounded by a begin and end
 */
class BeginEndSegment(val segments:List[SimpleSegment]) extends SimpleSegment {

  override def split:List[Expression] =
    return segments.flatMap(_.split)

  override def createCode(writer:CodeWriter) =
    SegmentReturn("begin\n") ++ segments.map(writer.createCode(_)).reduceLeft(_+_) + "end\n"


}