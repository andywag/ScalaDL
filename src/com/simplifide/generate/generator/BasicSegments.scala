package com.simplifide.generate.generator



import com.simplifide.generate.parser.model.Expression


abstract class BasicSegments extends SimpleSegment {


}
 /**
 * Object containing covenience classes for code generation
 */
object BasicSegments {
  /** Factory method to create a simple identifier */
  def Identifier(in:String):Identifier = new Identifier(in)
  /** Factory method to create a string {"in"} */
  def String(in:String):StringSegment = new StringSegment(in)
  /** Factory method to create a single quote output {'in'} */
  def Quote(in:String):QuoteSegment = new QuoteSegment(in)
  /** Factory method to create a simple number output */
  def Number(in:Int):NumberSegment = new NumberSegment(in)
  /** Factory method for creating a list of segments */
  def List(terms:List[SimpleSegment]) = new ListSegment(terms)
  /** Factory method for creating a list of segments */
  def List(terms:SimpleSegment*) = new ListSegment(terms.toList)
  /** Factory method for creating a list of segments */
  def ListExpression(terms:List[Expression]) = new ListSegment(terms.map(_.asInstanceOf[SimpleSegment]))

  /** Factory method to create a block of data surrounded by a begin end */
  def BeginEnd(terms:List[SimpleSegment]) = new BeginEnd(terms.toList)

  /** Class which creates a simple identifier */
  class Identifier(override val name:String) extends BasicSegments {
    override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn(name)
  }

  /** Class which creates a simple number */
  class NumberSegment(val value:Int) extends BasicSegments {
    override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn(value.toString)
  }

  /** Class which creates a single quote output {'in'} */
  class QuoteSegment(override val name:String) extends BasicSegments {
    override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn( "'" + name + "'")
  }

  /** Class which creates a single string output {"in"} */
  class StringSegment(override val name:String) extends BasicSegments {
     override def createCode(writer:CodeWriter):SegmentReturn = SegmentReturn(name)
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
  /** Block which contains segments surrounded by a begin end */
  class BeginEnd(val segments:List[SimpleSegment]) extends SimpleSegment {
    override def split:List[Expression] =
      return scala.List(new BeginEnd(segments.flatMap(_.split).map(_.asInstanceOf[SimpleSegment])))

    override def createCode(writer:CodeWriter):SegmentReturn =
      return SegmentReturn("begin\n") ++ writer.createCode(new ListSegment(segments)) + "end\n"
  }





 

}




