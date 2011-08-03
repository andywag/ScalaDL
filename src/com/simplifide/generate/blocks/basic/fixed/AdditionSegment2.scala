package com.simplifide.generate.blocks.basic.fixed

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */



import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.signal.{Constant, SignalTrait, OpType, FixedType}
import com.simplifide.generate.generator.{BasicSegments, SimpleSegment, CodeWriter, SegmentReturn}
import com.simplifide.generate.parser.math.Adder
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.parser.{ObjectFactory, ExpressionReturn}
import com.simplifide.generate.blocks.basic.operator.BinaryOperator
import com.simplifide.generate.language.Conversions._

case class AdditionSegment2(override val name:String,
                            val in1:SimpleSegment,
                            val in2:SimpleSegment,
                            override val negative:Boolean,
                            override val fixed:FixedType = FixedType.None,
                            val internal:FixedType       = FixedType.None) extends Adder(name,in1,in2,negative) with SimpleSegment{

  lazy val round:Boolean = false
  lazy val clip:Boolean  = false

  override def numberOfChildren:Int = in1.numberOfChildren
  override def child(index:Int):SimpleSegment = newSegment(name,in1.child(index),in2.child(index),index)

  def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment,negative:Boolean) =
    new AdditionSegment2(output.name,input1,input2,negative,output.fixed,this.internal)

  override def split(output:Expression,index:Int):ExpressionReturn = {

    val out   = (if (index == -1) output else output.copy(index)).asInstanceOf[SimpleSegment]
    val lp    = lhs.split(out,0)
    val rp    = rhs.split(out,1)
    val adder = ObjectFactory.Statement(out,newAdder(output.name,out,
      lp.output.asInstanceOf[SimpleSegment],rp.output.asInstanceOf[SimpleSegment],this.negative))

    new ExpressionReturn(out,lp.states ::: rp.states ::: List(adder)  )
  }


  def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int):AdditionSegment2 =
      new AdditionSegment2(name,in1,in2,negative,fixed,internal)



  /** Calculates the Real Internal Value which is used for the initial calculation. If not specified this assumes that
   *  the width is equal to the total width of the inputs */
  val realInternal:FixedType = {
    internal.getOrElse(this.in1.fixed.union(in2.fixed,fixed))
  }

  val realRound:Boolean = round && (realInternal.fraction > fixed.fraction)
  val realClip:Boolean  = clip  && (realInternal.integer > fixed.integer)

  private val shift = realInternal.fraction-fixed.fraction

  /** Rounding Term if round is required */
  val roundTerm:SimpleSegment =
    new AdditionTerm.AddTerm(Constant(math.pow(2.0,shift-1).toInt,realInternal.width))

  /** Output of the initial round block */
  val internalSignal:SignalTrait = SignalTrait(name + "_i",OpType.Signal,realInternal)





  override def createCode(writer:CodeWriter):SegmentReturn = {

    val baseStatement1 = List(new AdditionTerm.Empty(in1.sliceFixed(realInternal)),
      if (negative) new AdditionTerm.SubTerm(in2.sliceFixed(realInternal)) else new AdditionTerm.AddTerm(in2.sliceFixed(realInternal)))

    val baseStatement  = BasicSegments.List(if (realRound) baseStatement1 ::: List(roundTerm) else baseStatement1)


    if (this.realClip) {
        val extra = new SimpleStatement.Assign(internalSignal,baseStatement)
        val cl = new ClipSegment(internalSignal,this.fixed)
        return new SegmentReturn(writer.createCode(cl).code,List(),List(extra),List(internalSignal))
    }
    else if (this.realRound) {
        val extra = new SimpleStatement.Assign(internalSignal,baseStatement)
        val cl = new FixedSelect(internalSignal,this.fixed)
        return new SegmentReturn(writer.createCode(cl).code,List(),List(extra),List(internalSignal))
    }
    else {
        return writer.createCode(baseStatement)
    }
  }



}

object AdditionSegment2 {





  /** Truncation Addition Segment */
  class Truncate(name:String,in1:SimpleSegment, in2:SimpleSegment,negative:Boolean = false,
                 fixed:FixedType,internal:FixedType)
    extends AdditionSegment2(name,in1,in2,negative,fixed,internal) {



    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int):AdditionSegment2 =
      new Truncate(name,in1,in2,negative,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment,negative:Boolean) =
      new Truncate(output.name,input1,input2,negative,output.fixed,this.internal)

  }

  class TruncateClip(name:String,in1:SimpleSegment, in2:SimpleSegment,negative:Boolean = false,
                 fixed:FixedType,internal:FixedType)
    extends AdditionSegment2(name,in1,in2,negative,fixed,internal) {

    override lazy val clip = true

    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int):AdditionSegment2 =
      new TruncateClip(name,in1,in2,negative,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment,negative:Boolean) =
      new TruncateClip(output.name,input1,input2,negative,output.fixed,this.internal)
  }

  class Round(name:String,in1:SimpleSegment, in2:SimpleSegment,negative:Boolean = false,
                 fixed:FixedType,internal:FixedType)
    extends AdditionSegment2(name,in1,in2,negative,fixed,internal) {
    override lazy val round = true

    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int):AdditionSegment2 =
      new Round(name,in1,in2,negative,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment,negative:Boolean) =
      new Round(output.name,input1,input2,negative,output.fixed,this.internal)

  }

  class RoundClip(name:String,in1:SimpleSegment, in2:SimpleSegment,negative:Boolean = false,
                 fixed:FixedType,internal:FixedType)
    extends AdditionSegment2(name,in1,in2,negative,fixed,internal) {

    override lazy val round = true
    override lazy val clip = true


    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int):AdditionSegment2 =
      new RoundClip(name,in1,in2,negative,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment,negative:Boolean) =
      new RoundClip(output.name,input1,input2,negative,output.fixed,this.internal)
  }
  
  
}
