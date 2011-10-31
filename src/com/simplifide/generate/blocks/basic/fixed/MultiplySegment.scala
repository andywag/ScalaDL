package com.simplifide.generate.blocks.basic.fixed



import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.signal.{Constant, SignalTrait, OpType, FixedType}
import com.simplifide.generate.generator.{BasicSegments, SimpleSegment, CodeWriter, SegmentReturn}
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.blocks.basic.operator.BinaryOperator
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.math.{Multiplier}
import com.simplifide.generate.parser.{SegmentHolder, ObjectFactory, ExpressionReturn}
import com.simplifide.generate.proc.Controls


/**
 * Class which controls the creation of a multiplication segment
 *
 * @constructor
 * @parameter name Name of Segment
 * @parameter in1 First Input
 * @parameter in2 Second Input
 * @parameter fixed Output Fixed Type
 * @parameter internal Internal Fixed Type
 */
case class MultiplySegment(override val name:String,
                            val in1:SimpleSegment,
                            val in2:SimpleSegment,
                            override val fixed:FixedType = FixedType.Simple,
                            val internal:FixedType       = FixedType.Simple) extends Multiplier(in1,in2) with SimpleSegment{

  lazy val round:Boolean = false
  lazy val clip:Boolean  = false

  override def numberOfChildren:Int = in1.numberOfChildren
  override def child(index:Int):SimpleSegment = newSegment(name,in1.child(index),in2.child(index),index)

  def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment) =
    new MultiplySegment(output.name,input1,input2,output.fixed,this.internal)

  override def split(output:Expression,index:Int):ExpressionReturn = {

    val out   = (if (index == -1) output else output.copy(index)).asInstanceOf[SimpleSegment]
    val extra   = if (index == -1) List() else List(out.asInstanceOf[SignalTrait])

    val lp    = lhs.split(out,0)
    val rp    = rhs.split(out,1)

     val newAdderBlock = newAdder(output.name,
      out,
      lp.output.asInstanceOf[SimpleSegment],
      rp.output.asInstanceOf[SimpleSegment])

    val adder = newAdderBlock.createAssign(out,extra)




    new ExpressionReturn(out,lp.states ::: rp.states ::: List(adder)  )
  }


  def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int):MultiplySegment =
      new MultiplySegment(name,in1,in2,fixed,internal)



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




  // TODO Clean up this code. All the cases are actually the same. Multiply might not even require to be combined with round
  override def createCode(writer:CodeWriter):SegmentReturn = {

    val multiplier = new BinaryOperator.Multiply(this.in1,this.in2)
    if (this.fixed == this.multiplierFixed) {
      return writer.createCode(multiplier)
    }
    else if (realRound && realClip) {
      val multiplyStatement = new SimpleStatement.Assign(internalSignalM,multiplier)
      val roundSegment = new RoundSegment.RoundClip(internalSignalM.name,internalSignalM,this.fixed, this.internal)
      return new SegmentReturn("",List(),List(multiplyStatement),List(internalSignalM)) + writer.createCode(roundSegment)
    }
    else if (realClip) {
      val multiplyStatement = new SimpleStatement.Assign(internalSignalM,multiplier)
      val roundSegment = new RoundSegment.RoundClip(internalSignalM.name,internalSignalM,this.fixed, this.internal)
      return new SegmentReturn("",List(),List(multiplyStatement),List(internalSignalM)) + writer.createCode(roundSegment)
    }
    else if (realRound) {
      val multiplyStatement = new SimpleStatement.Assign(internalSignalM,multiplier)
      val roundSegment = new RoundSegment.Round(internalSignalM.name,internalSignalM,this.fixed, this.internal)
      return new SegmentReturn("",List(),List(multiplyStatement),List(internalSignalM)) + writer.createCode(roundSegment)
    }
    else {
      val extra = new SimpleStatement.Assign(internalSignalM,multiplier)
      val cl = new FixedSelect(internalSignalM,this.fixed)
      return new SegmentReturn(writer.createCode(cl).code,List(),List(extra),List(internalSignalM))
    }
    null


  }

  override def controls = in1.controls ::: in2.controls
  override def controlMatch(actual:SimpleSegment,statements:SegmentHolder):Boolean = actual.isInstanceOf[MultiplySegment]
  override  def createControl(actual:SimpleSegment,statements:SegmentHolder,index:Int):List[Controls] = {
    val multiply = actual.asInstanceOf[MultiplySegment]
    this.in1.createControl(multiply.in1,statements,index) ::: this.in2.createControl(multiply.in2,statements,index)
  }



}

object MultiplySegment {





  /** Truncation Addition Segment */
  case class Truncate(override val name:String,
                      override val in1:SimpleSegment,
                      override val in2:SimpleSegment,
                      override val fixed:FixedType,
                      override val internal:FixedType)
    extends MultiplySegment(name,in1,in2,fixed,internal) {



    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int) =
      new Truncate(name,in1,in2,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment) =
      new Truncate(output.name,input1,input2,output.fixed,this.internal)

  }

  case class TruncateClip(override val name:String,
                      override val in1:SimpleSegment,
                      override val in2:SimpleSegment,
                      override val fixed:FixedType,
                      override val internal:FixedType)
    extends MultiplySegment(name,in1,in2,fixed,internal) {

    override lazy val clip = true

    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int) =
      new TruncateClip(name,in1,in2,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment) =
      new TruncateClip(output.name,input1,input2,output.fixed,this.internal)
  }

    case class Round(override val name:String,
                      override val in1:SimpleSegment,
                      override val in2:SimpleSegment,
                      override val fixed:FixedType,
                      override val internal:FixedType)
    extends MultiplySegment(name,in1,in2,fixed,internal) {
    override lazy val round = true

    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int) =
      new Round(name,in1,in2,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment) =
      new Round(output.name,input1,input2,output.fixed,this.internal)

  }

    case class RoundClip(override val name:String,
                      override val in1:SimpleSegment,
                      override val in2:SimpleSegment,
                      override val fixed:FixedType,
                      override val internal:FixedType)
    extends MultiplySegment(name,in1,in2,fixed,internal) {

    override lazy val round = true
    override lazy val clip = true


    override def newSegment(name:String,in1:SimpleSegment, in2:SimpleSegment,index:Int) =
      new RoundClip(name,in1,in2,fixed,internal)

    override def newAdder(name:String,output:SimpleSegment,input1:SimpleSegment,input2:SimpleSegment) =
      new RoundClip(output.name,input1,input2,output.fixed,this.internal)
  }
  
  
}

