package com.simplifide.generate.blocks.basic.state

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.util.StringOps
import scala.collection.mutable.ListBuffer
import com.simplifide.generate.generator._

class AlwaysProcess(val name1:Option[String],body:SimpleSegment) extends BaseCodeSegment{

}

object AlwaysProcess {

 def Sensitivity(body:List[SimpleSegment],senseList:List[SimpleSegment]):AlwaysSensitivity =
    new AlwaysSensitivity(None,BasicSegments.List(body),senseList)

  def Sensitivity(name:Option[String],body:SimpleSegment,senseList:List[SimpleSegment]):AlwaysSensitivity =
    new AlwaysSensitivity(name,body,senseList)

  def Star(name:Option[String],body:SimpleSegment, senseList:List[SimpleSegment]):AlwaysStar =
    new AlwaysStar(name,body,senseList)

  def Star(body:SimpleSegment):AlwaysStar = Star(None,body,List[SimpleSegment]())
  def Star(segments:List[SimpleSegment]):AlwaysStar =
    Star(None,BasicSegments.List(segments),List())


  class AlwaysSensitivity(name:Option[String],
                          body:SimpleSegment,
                          senseList:List[SimpleSegment]) extends AlwaysProcess(name,body) {

    override def split:List[SimpleSegment] = {
      val bod =  BasicSegments.List(body.split.map(_.asInstanceOf[SimpleSegment]))
      return List(new AlwaysSensitivity(name,bod,senseList))
    }


    def createVerilogSenseList(writer:CodeWriter):String = {
        val build = new StringBuilder()
        build.append("always @(");
        build.append(StringOps.repeatAfterFirst(senseList, " or ", writer))
        build.append(") ")
        return build.toString
    }



    def createVhdlSenseList(writer:CodeWriter):String = {
      val build = new StringBuilder()
        if (name != None) build.append(name.get + " : ")
        build.append("process (");
        build.append(StringOps.repeatAfterFirst(senseList, ",", writer))
        build.append(") begin\n")
        return build.toString
    }

    override def createVerilogCode(writer:CodeWriter):SegmentReturn = {
      val build = new StringBuilder();
      build.append(createVerilogSenseList(writer))
      build.append("begin\n")
      build.append(StringOps.indentCode(writer, body))
      build.append("end\n")
      return SegmentReturn.segment(build.toString());
   }

    override def createVhdlCode(writer:CodeWriter):SegmentReturn = {
      val build = new StringBuilder()
      build.append(createVhdlSenseList(writer))
      build.append(StringOps.indentCode(writer, body))
      build.append("end process;\n")
      SegmentReturn.segment(build.toString())
    }

    override def createFloatCode(writer:CodeWriter):SegmentReturn       = {
	    writer.createCode(body)
    }
    override def createFixedCode(writer:CodeWriter):SegmentReturn       = {
	    writer.createCode(body)
    }

  }

  class AlwaysStar(name:Option[String],body:SimpleSegment, senseList:List[SimpleSegment]) extends AlwaysSensitivity(name,body,senseList) {

    override def split:List[SimpleSegment] = {
      val bod = BasicSegments.List(body.split.map(_.asInstanceOf[SimpleSegment]))
      return List(new AlwaysStar(name,bod,senseList))
    }

    override def createVerilogSenseList(writer:CodeWriter):String = {
      return "always @* "
    }

    override def createVhdlSenseList(writer:CodeWriter):String = {
      val builder = new StringBuilder()
      if (name != None) builder.append(name.get + " : ")
      builder.append("process(all) begin\n")
      builder.toString
    }
  }


}



