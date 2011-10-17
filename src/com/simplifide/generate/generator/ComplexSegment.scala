package com.simplifide.generate.generator

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.generator.ComplexSegment.Holder
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.{ConditionParser, SegmentHolder}


/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/8/11
 * Time: 7:45 PM
 * To change this template use File | Settings | File Templates.
 */

trait ComplexSegment extends ConditionParser with SimpleSegment{

  //implicit val scope = this


  def createBody

  override def split  = {
    this.createBody
    List(new ComplexSegment.Holder(this.allStatements.toList.flatMap(_.split),this.signals.map(_.asInstanceOf[SignalTrait]).toList))
  }

  override def createCode(writer:CodeWriter):SegmentReturn  = {
    System.out.print("Error in Class" + this + this.getClass)
    null
  }

}

object ComplexSegment {

  class Holder(val statements:List[SimpleSegment], val signals:List[SignalTrait]) extends SimpleSegment {
    override def createCode(writer:CodeWriter) = {
      val total = this.statements.map(writer.createCode(_)) ::: List(new SegmentReturn("",List(),List(),signals))
      total.reduceLeft(_ + _ )
    }
  }

}