package com.simplifide.generate.blocks.basic.fixed





import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.signal.{Constant, SignalTrait, OpType, FixedType}
import com.simplifide.generate.generator.{BasicSegments, SimpleSegment, CodeWriter, SegmentReturn}
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.parser.{ObjectFactory, ExpressionReturn}
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.block.Statement

/**
 * Class which defines a round operation
 *
 * @constructor
 * @parameter name Name of Round Segment
 * @parameter fixed Output Fixed Type
 * @parameter internal Internal Fixed Type
 */
case class RoundSegment(override val name:String,
                        val in1:SimpleSegment,
                        override val fixed:FixedType = FixedType.None,
                        val internal:FixedType       = FixedType.None) extends SimpleSegment{

  lazy val round:Boolean = false
  lazy val clip:Boolean  = false

  override def numberOfChildren:Int = in1.numberOfChildren
  override def child(index:Int):SimpleSegment = newSegment(name,in1.child(index),index)

  def newSegment(name:String,in1:SimpleSegment,index:Int):RoundSegment =
      new RoundSegment(name,in1,fixed,internal)

  def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment) =
    new RoundSegment(output.name,input1,output.fixed,this.internal)


  override def split(output:Expression,index:Int):ExpressionReturn = {

    val out   = (if (index == -1) output else output.copy(index)).asInstanceOf[SimpleSegment]
    val lp    = this.in1.split(out,0)
    val adder = ObjectFactory.Statement(out,newAdder(output.name,out,lp.output.asInstanceOf[SimpleSegment])).asInstanceOf[Statement]

    new ExpressionReturn(out,lp.states ::: List(adder)  )
  }


  /** Output of the initial round block */
  private val realInternal:FixedType = internal.getOrElse(this.in1.fixed)
  private val internalSignal:SignalTrait = SignalTrait(name + "_i",OpType.Signal,realInternal)
  private val realRound:Boolean = round && (in1.fixed.fraction > fixed.fraction)
  private val realClip:Boolean  = clip  && (in1.fixed.integer > fixed.integer)
  private val shift = realInternal.fraction-fixed.fraction
  private val roundTerm:SimpleSegment = new AdditionTerm.AddTerm(Constant(math.pow(2.0,shift-1).toInt,realInternal.width))


  override def createCode(writer:CodeWriter):SegmentReturn = {


    if (this.fixed == this.in1.fixed) {
      return writer.createCode(in1)
    }
    else if (realRound && realClip) {
      val state = BasicSegments.List(in1.sliceFixed(this.realInternal),new AdditionTerm.AddTerm(this.roundTerm))
      val extra = new SimpleStatement.Assign(internalSignal,state)
      val cl = new ClipSegment(internalSignal,this.fixed)
      return new SegmentReturn(writer.createCode(cl).code,List(),List(extra),List(internalSignal))
    }
    else if (realClip) {
      val cl = new ClipSegment(in1,this.fixed)
      return new SegmentReturn(writer.createCode(cl).code,List(),List(),List())
    }
    else {
      val state = BasicSegments.List(in1.sliceFixed(this.realInternal),new AdditionTerm.AddTerm(this.roundTerm))
      val extra = new SimpleStatement.Assign(internalSignal,state)
      val cl = new FixedSelect(internalSignal,this.fixed)
      return new SegmentReturn(writer.createCode(cl).code,List(),List(extra),List(internalSignal))
    }

    null
  }

}

object RoundSegment {





  /** Truncation Addition Segment */
  class Truncate(name:String,in1:SimpleSegment,fixed:FixedType,internal:FixedType)
    extends RoundSegment(name,in1,fixed,internal) {

    override def newSegment(name:String,in1:SimpleSegment, index:Int) =
      new Truncate(name,in1,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment) =
      new Truncate(output.name,input1,output.fixed,this.internal)

  }

  class TruncateClip(name:String,in1:SimpleSegment,fixed:FixedType,internal:FixedType)
    extends RoundSegment(name,in1,fixed,internal) {

    override lazy val clip = true

    override def newSegment(name:String,in1:SimpleSegment,index:Int) =
      new TruncateClip(name,in1,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment) =
      new TruncateClip(output.name,input1,output.fixed,this.internal)
  }

  class Round(name:String,in1:SimpleSegment, fixed:FixedType,internal:FixedType)extends RoundSegment(name,in1,fixed,internal) {
    override lazy val round = true

    override def newSegment(name:String,in1:SimpleSegment,index:Int) =
      new Round(name,in1,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment) =
      new Round(output.name,input1,output.fixed,this.internal)

  }

  class RoundClip(name:String,in1:SimpleSegment, fixed:FixedType,internal:FixedType)
    extends RoundSegment(name,in1,fixed,internal) {

    override lazy val round = true
    override lazy val clip = true


    override def newSegment(name:String,in1:SimpleSegment, index:Int) =
      new RoundClip(name,in1,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment) =
      new RoundClip(output.name,input1,output.fixed,this.internal)
  }
  
  
}

