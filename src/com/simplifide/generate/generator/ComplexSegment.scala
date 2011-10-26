package com.simplifide.generate.generator

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.generator.ComplexSegment.Holder
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.{ConditionParser, SegmentHolder}


/**
 * Trait which allows complex segments to be built using the more descriptive syntax from the module rather than
 * building off subblocks. The body of the block should be defined using the createbody method
 */

trait ComplexSegment extends ConditionParser with SimpleSegment{

  /** Defines the body in the block */
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

  /** Class which is used to contain the body of the complex value after the split operation */
  class Holder(val statements:List[SimpleSegment], val signals:List[SignalTrait]) extends SimpleSegment {
    override def createCode(writer:CodeWriter) = {
      val total = this.statements.map(writer.createCode(_)) ::: List(new SegmentReturn("",List(),List(),signals))
      total.reduceLeft(_ + _ )
    }
  }

}