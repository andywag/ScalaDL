package com.simplifide.generate.generator

import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser._
import com.simplifide.generate.generator.ComplexSegment.Holder
import collection.mutable.ListBuffer
import factory.CreationFactory
import model.Expression

/**
 * Trait which allows complex segments to be built using the more descriptive syntax from the module rather than
 * building off subblocks. The body of the block should be defined using the createbody method
 */

trait ComplexSegment extends ConditionParser with SignalHolder with SimpleSegment{

  /** Defines the body in the block */
  def createBody

  override def create(implicit creator:CreationFactory) = {
    this.createBody
    new ComplexSegment.Holder(this.allStatements.map(_.create), this.signals.toList)
  }


  override def createCode(implicit writer:CodeWriter):SegmentReturn  = {
    System.out.print("Error in Class" + this + this.getClass)
    null
  }

}

object ComplexSegment {

  /** Class which is used to contain the body of the complex value after the split operation */
  class Holder(val statements:List[SimpleSegment], 
               val signals:List[SignalTrait]) extends SimpleSegment {
    
    def newHolder(statements:List[SimpleSegment] = this.statements,
      signals:List[SignalTrait] = this.signals) = new Holder(statements,signals)
    
    
    override def createSplit  = List(this.newHolder(statements = this.statements.flatMap(_.createSplit)))
    override def createVector = List(this.newHolder(statements = this.statements.flatMap(_.createVector)))

    override def createCode(implicit writer:CodeWriter) = {
      val total = this.statements.map(writer.createCode(_)) ::: List(new SegmentReturn("",List(),List(),signals))
      total.reduceLeft(_ + _ )
    }
  }

}