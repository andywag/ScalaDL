package com.simplifide.generate.blocks.basic.state



import com.simplifide.generate.generator._

/**
 * Class which defines an always or process body
 *
 * @constructor
 * @parameter name1 Optional name of the block
 * @parameter body  Body inside the always body
 */
abstract class Always(val name1:Option[String],body:SimpleSegment) extends SimpleSegment{

}

object Always {

  /*
  def Sensitivity(body:List[SimpleSegment],senseList:List[SimpleSegment]):Sensitivity =
    new Sensitivity(None,BasicSegments.List(body),senseList)

  def Sensitivity(name:Option[String],body:SimpleSegment,senseList:List[SimpleSegment]):Sensitivity =
    new Sensitivity(name,body,senseList)

  def Star(name:Option[String],body:SimpleSegment, senseList:List[SimpleSegment]):Star =
    new Star(name,body,senseList)

  def Star(body:SimpleSegment):Star = Star(None,body,List[SimpleSegment]())
  def Star(segments:List[SimpleSegment]):Star =
    Star(None,BasicSegments.List(segments),List())

  */

  /**
   * Always block which contains a sensitivity list
   * @constructor
   * @parameter senseList Sensitivity List for the always block
   */
  class Sensitivity(name:Option[String],
                    body:SimpleSegment,
                    senseList:List[SimpleSegment]) extends Always(name,body) {

    override def split:List[SimpleSegment] = {
      val bod =  BasicSegments.List(body.split.map(_.asInstanceOf[SimpleSegment]))
      return List(new Sensitivity(name,bod,senseList))
    }


    protected def createVerilogSenseList(writer:CodeWriter):SegmentReturn = {
      def sense:SegmentReturn = senseList.map(writer.createCode(_)).zipWithIndex.map(x => (if (x._2 == 0) x._1 else SegmentReturn(",") + x._1)).reduceLeft(_+_)
      return SegmentReturn("always @(") + sense + ")"
    }


    override def createCode(implicit writer:CodeWriter):SegmentReturn = {
      val ret:SegmentReturn =  this.createVerilogSenseList(writer) + "begin\n" ++ writer.createCode(body) + "end\n"
      return ret
    }
  }

  /** Factory Methods for creating an always block with a sensitivity list */
  object Sensitivity {
    /** Always Block with Sensitivity List (senseList) containing Body body */
    def apply(body:List[SimpleSegment],senseList:List[SimpleSegment]):Sensitivity =
      new Sensitivity(None,BasicSegments.List(body),senseList)

    /** Always Block with Sensitivity List (senseList) containing Body body */
    def apply(name:Option[String],body:SimpleSegment,senseList:List[SimpleSegment]):Sensitivity =
      new Sensitivity(name,body,senseList)
  }

  class Star(name:Option[String],body:SimpleSegment, senseList:List[SimpleSegment]) extends Sensitivity(name,body,senseList) {

    override def split:List[SimpleSegment] = {
      val bod = BasicSegments.List(body.split.map(_.asInstanceOf[SimpleSegment]))
      return List(new Star(name,bod,senseList))
    }

    override def createVerilogSenseList(writer:CodeWriter):SegmentReturn = {
      return SegmentReturn("always @* ")
    }
  }

  object Star {
    def apply(name:Option[String],body:SimpleSegment, senseList:List[SimpleSegment]):Star =
      new Star(name,body,senseList)

    def apply(body:SimpleSegment):Star = Star(None,body,List[SimpleSegment]())

    def apply(segments:List[SimpleSegment]):Star =
      Star(None,BasicSegments.List(segments),List())
  }


}



