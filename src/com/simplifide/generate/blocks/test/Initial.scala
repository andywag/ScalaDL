package com.simplifide.generate.blocks.test

import com.simplifide.generate.generator.{SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.blocks.basic.Statement
import com.simplifide.generate.generator.SegmentReturn._
import com.simplifide.generate.blocks.test.Initial.Impl
import com.simplifide.generate.signal.{NewConstant, Constant, SignalTrait}

/**
 * Initial statement used to initialize signals
 */

trait Initial extends SimpleSegment {
  
  val segments:List[SimpleSegment]

  //override def createVector = Initial(segments.flatMap(_.createVector))
  //override def createSplit  = Initial(segments.flatMap(_.createSplit))
  
  
  override def createCode(implicit writer:CodeWriter):SegmentReturn = {
    def segmentList = if (segments.size == 0) SegmentReturn("") else segments.map(x => writer.createCode(x)).reduceLeft(_ + _)
    SegmentReturn("initial begin\n") ++
      segmentList +
    "end\n\n"
  }

}

object Initial {
  def apply(segments:List[SimpleSegment]) = new Impl(segments)

  // TODO Doesn't Currently Work
  def apply(segments:List[InitialSegment],signals:List[SignalTrait]) = {
    val segmentSignals = segments.map(_.signal).flatMap(_.allSignalChildren)
    val uniqueSignals  = SignalTrait.uniqueSignals(segmentSignals.flatMap(_.allSignalChildren))
    val allSignals     = SignalTrait.uniqueSignals(signals.flatMap(_.allSignalChildren))
    val extraSignals   = allSignals.filter(x => !uniqueSignals.map(_.name).contains(x))
    val newSegments    = extraSignals.map(x => new Initial.AssignSegment(x,NewConstant(0,x.width)))
    new Impl(segments ::: newSegments)
  }


  class Impl(override val segments:List[SimpleSegment]) extends Initial


  class InitialSegment(val signal:SignalTrait) extends SimpleSegment {
    override def createCode(implicit writer:CodeWriter):SegmentReturn = {
      this match {
        case x:Delay         => SegmentReturn(x.signal.name) + " = #" + x.delay.toString + " " +  x.value.toString + ";\n"
        case x:Assignment    => SegmentReturn(signal.name) + " = " + x.value.toString + ";\n"
        case x:AssignSegment => new Statement.FunctionBody(x.signal,x.value).createCode
        case _               => SegmentReturn("")
      }
    }
  }
  
  class Delay(signal:SignalTrait, val value:Long, val delay:Int) extends InitialSegment(signal)
  class Assignment(signal:SignalTrait, val value:Long) extends InitialSegment(signal)
  class AssignSegment(signal:SignalTrait, val value:SimpleSegment) extends InitialSegment(signal)

}

