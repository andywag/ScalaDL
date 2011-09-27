package com.simplifide.generate.blocks.basic.operator

import com.simplifide.generate.generator.{BaseCodeSegment, CodeWriter, SegmentReturn, SimpleSegment}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 5/29/11
 * Time: 7:24 PM
 * To change this template use File | Settings | File Templates.
 */

class Operators {

}

object Operators {


  def Concat(states:List[SimpleSegment]) = new Concat(states)
  def Paren(in:SimpleSegment)            = new Paren(in)
  def Tick(in:SimpleSegment,op:String)   = new Tick(in,op)
  def Slice(in:SimpleSegment,slice:SimpleSegment) = new Slice(in,slice)

  /** Concatenation Operator for verilog {a,b,c} */
  class Concat(val states:List[SimpleSegment]) extends SimpleSegment {
    override def createCode(writer:CodeWriter):SegmentReturn = {
      val builder = new StringBuilder
      builder.append("{")
      var first = true
      for (state <- states) {
        if (!first) builder.append(",")
        builder.append(writer.createCode(state))
        first = false
      }
      builder.append("}")
      return SegmentReturn.segment(builder.toString)
    }
  }

  /** Parenthesis Operator for ( {in} )*/
  class Paren(val in:SimpleSegment) extends SimpleSegment {
    override def createCode(writer:CodeWriter):SegmentReturn  = {
      val builder = new StringBuilder
      builder.append("(")
      builder.append(writer.createCode(in))
      builder.append(")")
      return SegmentReturn.segment(builder.toString)
    }
  }

  class Tick(in:SimpleSegment,op:String) extends BaseCodeSegment{
    override def createCode(writer:CodeWriter):SegmentReturn = {
      SegmentReturn.segment(writer.createCode(in).code + "'" + op);
   }
  }

  class Slice(in:SimpleSegment,slice:SimpleSegment) extends SimpleSegment{

    override def createCode(writer:CodeWriter):SegmentReturn = {
      val builder = new StringBuilder()
      builder.append(in.createVerilogCode(writer))
      builder.append("[")
      builder.append(writer.createCode(slice))
      builder.append("]")
      SegmentReturn.segment(builder.toString)
    }
    override def createVhdlCode(writer:CodeWriter):SegmentReturn     = {
      val builder = new StringBuilder()
      builder.append(in.createVhdlCode(writer))
      builder.append("(")
      builder.append(slice.createVhdlCode(writer))
      builder.append(")")
      SegmentReturn.segment(builder.toString)
    }

  }


}