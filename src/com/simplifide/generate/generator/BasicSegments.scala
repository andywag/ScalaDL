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
       if (segments.length > 0) segments.map(x => writer.createCode(x)).reduceLeft( _ + _ )
       else SegmentReturn("")
    }

  }

  class ListSurround(segments:List[SimpleSegment]) extends ListSegment(segments) {
      override def createCode(writer:CodeWriter):SegmentReturn = {
        val list = new ListSegment(segments)
        return SegmentReturn.segment("begin\n") ++ writer.createCode(list) + "end\n"
      }
  }





 

}




