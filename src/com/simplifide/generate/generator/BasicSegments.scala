package com.simplifide.generate.generator

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import scala.collection.mutable.ListBuffer
import com.simplifide.generate.signal.SignalTrait

abstract class BasicSegments extends SimpleSegment {


}

object BasicSegments {

  def Ident(in:String):Ident = new Ident(in)
  def String(in:String):StringSegment = new StringSegment(in)
  def Quote(in:String):QuoteSegment = new QuoteSegment(in)
  def Number(in:Int):NumberSegment = new NumberSegment(in)
  def List(terms:List[SimpleSegment]) = new ListSegment(terms)

  class Ident(val name:String) extends BasicSegments {

    override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn.segment(name)

  }

  class NumberSegment(val value:Int) extends BasicSegments {
    override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn.segment(value.toString)
  }

  class QuoteSegment(val name:String) extends BasicSegments {
    override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn.segment( "'" + name + "'")
  }

  class StringSegment(val name:String) extends BasicSegments {
     override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn.segment(name)
  }

  class ListSegment(val segments:List[SimpleSegment]) extends BasicSegments {

    override def createCode(writer:CodeWriter):SegmentReturn = {
       val builder = new StringBuilder();
       for (segment <- segments) {
         builder.append(writer.createSimpleCode(segment));
        }
        return SegmentReturn.segment(builder.toString())
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




