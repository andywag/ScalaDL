package com.simplifide.generate.blocks.basic.fixed

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*

import com.simplifide.generate.blocks.basic.SimpleStatement
import java.lang.Boolean
import com.simplifide.generate.signal.{Constant, SignalTrait, OpType, FixedType}
import com.simplifide.generate.generator.{BasicSegments, SimpleSegment, CodeWriter, SegmentReturn}
import com.simplifide.generate.parser.math.Adder

case class AdditionSegmentNew(val name:String,
                              val in1:SimpleSegment,
                              val in2:SimpleSegment,
                              val negative:Boolean = false,
                              val internal:Option[FixedType] = None) extends Adder(name,in1,in2,negative) with SimpleSegment{

  override val fixed:FixedType = in1.fixed // Kludge
  override def numberOfChildren:Int = in1.numberOfChildren
  override def child(index:Int):SimpleSegment = newSegment(name,in1.child(index),in2.child(index),index)

  private val terms = List(in1,in2)

  def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int):AdditionSegmentNew =
      new AdditionSegmentNew(name,in1,in2)

  def round:Boolean = false
  def clip:Boolean  = false

  /** Calculates the Real Internal Value which is used for the initial calculation */
  val realInternal:FixedType = {
    if (internal != None) internal.get    // If the Internal Exists Return this
    terms.map(_.fixed).reduceLeft((x:FixedType,y:FixedType) => x.union(y))
  }

  val realRound:Boolean = round && (realInternal.fraction > outFixed.fraction)
  val realClip:Boolean  = clip  && (realInternal.integer > outFixed.integer)

  private val shift = realInternal.fraction-fixed.fraction

  /** Rounding Term if round is required */
  val roundTerm:SimpleSegment =
    new AdditionTerm.AddTerm(Constant.newConstant(math.pow(2.0,shift-1).toInt,realInternal.width))

  /** Output of the initial round block */
  val internalSignal = SignalTrait(name + "_i",OpType.Signal,realInternal)




  override def createCode(writer:CodeWriter):SegmentReturn = {
    val internalRound = if (realRound) List(roundTerm) else List()
    val addTerms:List[SimpleSegment] = List(in1.sliceFixed(realInternal), in2.sliceFixed(realInternal))  ::: internalRound
    val baseStatement = BasicSegments.List(addTerms)

    if (this.realClip) {
        val extra = new SimpleStatement.Assign(internalSignal,baseStatement)
        val cl = new ClipSegment(internalSignal,this.outFixed)
        return new SegmentReturn(writer.createCode(cl).code,List(),List(extra),List(internalSignal))
    }
    else if (this.realRound) {
        val extra = new SimpleStatement.Assign(internalSignal,baseStatement)
        val cl = new FixedSelect(internalSignal,this.outFixed)
        return new SegmentReturn(writer.createCode(cl).code,List(),List(extra),List(internalSignal))
    }
    else {
        return writer.createCode(baseStatement)
    }
  }



}

object AdditionSegmentNew {


  def apply(lhs:SimpleSegment, rhs:SimpleSegment, negative:Boolean) =
    new AdditionSegment("",terms(lhs,rhs,negative),FixedType.unsigned(1,0),None)


  /** Truncation Addition Segment */
  class TruncateFixed(name:String,in1:SimpleSegment, in2:SimpleSegment, override val fixed:FixedType, negative:Boolean = false)
    extends AdditionSegmentNew(name,in1,in2,negative) {

    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment):AdditionSegment =
      new TruncateFixed(name,in1,in2,this.fixed,this.negative)
  }

  /** Truncation Addition with a clip segment */
  class TruncateClip(name:String,in1:SimpleSegment, in2:SimpleSegment, override val fixed:FixedType, negative:Boolean = false)
    extends AdditionSegmentNew(name,in1,in2,negative) {

    override def newSegment(in1:SimpleSegment, in2:SimpleSegment):AdditionSegment =
      new TruncateClip(in1,in2,this.fixed,this.negative)


    override def clip:Boolean = true
  }
  
  /** Round Addition  */
  class Round(name:String,in1:SimpleSegment, in2:SimpleSegment, override val fixed:FixedType, negative:Boolean = false)
    extends AdditionSegmentNew(name,in1,in2,negative) {

    override def newSegment(in1:SimpleSegment, in2:SimpleSegment):AdditionSegment =
      new Round(in1,in2,this.fixed,this.negative)

    override def round:Boolean = true
  }
  
    /** Round Addition with Clipping Statement  */
  class RoundClip(name:String,in1:SimpleSegment, in2:SimpleSegment, override val fixed:FixedType, negative:Boolean = false)
    extends AdditionSegmentNew(name,in1,in2,negative) {

    override def newSegment(in1:SimpleSegment, in2:SimpleSegment):AdditionSegment =
      new RoundClip(in1,in2,this.fixed,this.negative)

    override def round:Boolean = true
    override def clip:Boolean = true
  }
  
  
  
}

*/