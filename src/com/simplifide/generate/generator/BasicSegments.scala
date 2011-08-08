package com.simplifide.generate.generator

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import scala.collection.mutable.ListBuffer
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.parser.model.Expression

abstract class BasicSegments extends SimpleSegment {


}

object BasicSegments {

  def Ident(in:String):Ident = new Ident(in)
  def String(in:String):StringSegment = new StringSegment(in)
  def Quote(in:String):QuoteSegment = new QuoteSegment(in)
  def Number(in:Int):NumberSegment = new NumberSegment(in)

  def List(terms:List[SimpleSegment]) = new ListSegment(terms)
  def List(terms:SimpleSegment*) = new ListSegment(terms.toList)
  def ListSurround(terms:List[SimpleSegment]) = new ListSurround(terms.toList)

  def ListExpression(terms:List[Expression]) = new ListSegment(terms.map(_.asInstanceOf[SimpleSegment]))

  class Ident(override val name:String) extends BasicSegments {

    override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn.segment(name)

  }

  class NumberSegment(val value:Int) extends BasicSegments {
    override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn.segment(value.toString)
  }

  class QuoteSegment(override val name:String) extends BasicSegments {
    override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn.segment( "'" + name + "'")
  }

  class StringSegment(override val name:String) extends BasicSegments {
     override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn.segment(name)
  }

  class ListSegment(val segments:List[SimpleSegment]) extends BasicSegments {

    override def split:List[SimpleSegment] = {
      val lis:scala.List[SimpleSegment] = segments.flatMap(_.split).map(_.asInstanceOf[SimpleSegment])
      lis

    }

    override def createCode(writer:CodeWriter):SegmentReturn = {
       val builder = new StringBuilder();
       for (segment <- segments) {
        if (segment != null) builder.append(writer.createSimpleCode(segment));
        }
        return SegmentReturn.segment(builder.toString())
    }

  }

  class ListSurround(val segments:List[SimpleSegment]) extends BasicSegments {
      override def createCode(writer:CodeWriter):SegmentReturn = {
        val list = new ListSegment(segments)
        return "begin\n   " + writer.createCode(list) + "end\n"
      }
  }


  /** @deprecated : Use List Segement Instead */
  class ListBufferSegment extends BasicSegments {

    val segments = new ListBuffer[BaseCodeSegment]

    override def createCode(writer:CodeWriter):SegmentReturn = {
       val builder = new StringBuilder();
       for (segment <- segments) {
         builder.append(writer.createSimpleCode(segment));
        }
        return SegmentReturn.segment(builder.toString)
    }


}



 

}




