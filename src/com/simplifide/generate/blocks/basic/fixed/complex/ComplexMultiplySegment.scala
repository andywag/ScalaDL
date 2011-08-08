package com.simplifide.generate.blocks.basic.fixed.complex

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*


import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.signal.{Constant, SignalTrait, OpType, FixedType}
import com.simplifide.generate.generator.{BasicSegments, SimpleSegment, CodeWriter, SegmentReturn}
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.parser.{ObjectFactory, ExpressionReturn}
import com.simplifide.generate.blocks.basic.operator.BinaryOperator
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.math.{Multiplier, Adder}
import com.simplifide.generate.blocks.basic.fixed.AdditionTerm
import com.simplifide.generate.signal.complex.ComplexSignal
import collection.mutable.ListBuffer

case class ComplexMultiplySegment(override val name:String,
                            val in1:ComplexSignal,
                            val in2:ComplexSignal,
                            override val fixed:FixedType = FixedType.None,
                            val internal:FixedType       = FixedType.None) extends Multiplier(in1,in2) with SimpleSegment{

  lazy val round:Boolean = false
  lazy val clip:Boolean  = false

  override def numberOfChildren:Int = in1.numberOfChildren
  override def child(index:Int):SimpleSegment = newSegment(name,in1.child(index),in2.child(index),index)

  def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment) =
    new ComplexMultiplySegment(output.name,input1,input2,output.fixed,this.internal)

  override def split(output:Expression,index:Int):ExpressionReturn = {

    val out   = (if (index == -1) output else output.copy(index)).asInstanceOf[SimpleSegment]
    val lp    = lhs.split(out,0)
    val rp    = rhs.split(out,1)
    val adder = ObjectFactory.Statement(out,newAdder(output.name,out,
      lp.output.asInstanceOf[SimpleSegment],rp.output.asInstanceOf[SimpleSegment]))

    new ExpressionReturn(out,lp.states ::: rp.states ::: List(adder)  )
  }


  def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int):ComplexMultiplySegment =
      new ComplexMultiplySegment(name,in1,in2,fixed,internal)



  /** Calculates the Real Internal Value which is used for the initial calculation. If not specified this assumes that
   *  the width is equal to the total width of the inputs */
  val realInternal:FixedType = {
    internal.getOrElse(this.in1.fixed + this.in2.fixed)
  }

  val realRound:Boolean = round && (realInternal.fraction > fixed.fraction)
  val realClip:Boolean  = clip  && (realInternal.integer > fixed.integer)

  private val shift = realInternal.fraction-fixed.fraction

  /** Rounding Term if round is required */
  val roundTerm:SimpleSegment =
    new AdditionTerm.AddTerm(Constant(math.pow(2.0,shift-1).toInt,realInternal.width))

  val  multiplierFixed = in1.fixed * in2.fixed
  /** Output of the initial round block */
  val internalSignalM:SignalTrait = SignalTrait(name + "_im",OpType.Signal,multiplierFixed)
  //val internalSignalR:SignalTrait = SignalTrait(name + "_ir",OpType.Signal,realInternal)





  override def createCode(writer:CodeWriter):SegmentReturn = {

    val segments = new ListBuffer[SimpleSegment]

    val internalReRe = ComplexSignal(this.name+"_re_re", OpType.WIRE, this.multiplierFixed)
    val internalReIm = ComplexSignal(this.name+"_re_im", OpType.WIRE, this.multiplierFixed)
    val internalImRe = ComplexSignal(this.name+"_im_re", OpType.WIRE, this.multiplierFixed)
    val internalImIm = ComplexSignal(this.name+"_im_im", OpType.WIRE, this.multiplierFixed)

    internalReRe := in1.real * in2.real
    internalReIm := in1.real * in2.imag
    internalImRe := in1.imag + in2.real
    internalImIm := in1.imag + in2.imag

    null

  }



}

object MultiplySegment {





  /** Truncation Addition Segment */
  class Truncate(name:String,in1:SimpleSegment, in2:SimpleSegment,fixed:FixedType,internal:FixedType)
    extends CopmlexMultiplySegment(name,in1,in2,fixed,internal) {



    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int) =
      new Truncate(name,in1,in2,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment) =
      new Truncate(output.name,input1,input2,output.fixed,this.internal)

  }

  class TruncateClip(name:String,in1:SimpleSegment, in2:SimpleSegment,fixed:FixedType,internal:FixedType)
    extends MultiplySegment(name,in1,in2,fixed,internal) {

    override lazy val clip = true

    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int) =
      new TruncateClip(name,in1,in2,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment) =
      new TruncateClip(output.name,input1,input2,output.fixed,this.internal)
  }

  class Round(name:String,in1:SimpleSegment, in2:SimpleSegment,fixed:FixedType,internal:FixedType)
    extends MultiplySegment(name,in1,in2,fixed,internal) {
    override lazy val round = true

    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int) =
      new Round(name,in1,in2,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment) =
      new Round(output.name,input1,input2,output.fixed,this.internal)

  }

  class RoundClip(name:String,in1:SimpleSegment, in2:SimpleSegment,fixed:FixedType,internal:FixedType)
    extends MultiplySegment(name,in1,in2,fixed,internal) {

    override lazy val round = true
    override lazy val clip = true


    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int) =
      new RoundClip(name,in1,in2,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment) =
      new RoundClip(output.name,input1,input2,output.fixed,this.internal)
  }
  
  
}

 */