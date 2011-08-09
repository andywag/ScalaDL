package com.simplifide.generate.blocks.basic.fixed.complex

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */




import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.signal.{Constant, SignalTrait, OpType, FixedType}
import com.simplifide.generate.parser.model.Expression
import com.simplifide.generate.blocks.basic.operator.BinaryOperator
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.math.{Multiplier, Adder}
import com.simplifide.generate.blocks.basic.fixed.AdditionTerm
import com.simplifide.generate.signal.complex.ComplexSignal
import collection.mutable.ListBuffer
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.parser.{SegmentHolder, ObjectFactory, ExpressionReturn}
import com.simplifide.generate.generator._
import com.simplifide.generate.blocks.basic.fixed.complex.ComplexMultiplySegment.RoundClip
import com.simplifide.generate.language.SignalFactory

case class ComplexMultiplySegment(override val name:String,
                            val clk:ClockControl,
                            val out:ComplexSignal,
                            val in1:ComplexSignal,
                            val in2:ComplexSignal,
                            val internal:FixedType       = FixedType.None) extends Multiplier(in1,in2) with ComplexSegment {



  lazy val round:Boolean = false
  lazy val clip:Boolean  = false

  override val fixed:FixedType = out.fixed


  //override def numberOfChildren:Int = in1.numberOfChildren
  //override def child(index:Int):SimpleSegment = newMultiplier(name,clk,in1.child(index),in2.child(index))

  def newMultiplier(name:String,output:ComplexSignal,input1:ComplexSignal,input2:ComplexSignal) =
    new ComplexMultiplySegment(output.name,clk,output,input1,input2,this.internal)

  override def split(output:Expression,index:Int):ExpressionReturn = {

    val out   = (if (index == -1) output else output.copy(index)).asInstanceOf[SimpleSegment]
    val lp    = lhs.split(out,0)
    val rp    = rhs.split(out,1)
    val adder = ObjectFactory.Statement(out,newMultiplier(output.name,out.asInstanceOf[ComplexSignal],
      lp.output.asInstanceOf[ComplexSignal],rp.output.asInstanceOf[ComplexSignal]))

    new ExpressionReturn(out,lp.states ::: rp.states ::: List(adder)  )
  }






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

  def operate(exp:Expression) = SignalFactory.truncate(exp,out.fixed,internal)

  def createBody = {

    val inReRe = attach(SignalTrait(this.name+"_re_re", OpType.Signal, this.multiplierFixed))
    val inReIm = attach(SignalTrait(this.name+"_re_im", OpType.Signal, this.multiplierFixed))
    val inImRe = attach(SignalTrait(this.name+"_im_re", OpType.Signal, this.multiplierFixed))
    val inImIm = attach(SignalTrait(this.name+"_im_im", OpType.Signal, this.multiplierFixed))

    inReRe :=  operate(in1.real * in2.real) //@@ this.clk
    inReIm :=  operate(in1.real * in2.imag) //@@ this.clk
    inImRe :=  operate(in1.imag * in2.real) //@@ this.clk
    inImIm :=  operate(in1.imag * in2.imag) //@@ this.clk

    out.real := operate(inReRe - inImIm)    //@@ this.clk
    out.imag := operate(inImRe + inReIm)    //@@ this.clk

  }

  /*
  override def split =   {
    this.createBody
    this.statements.toList.flatMap(x => x.split)
  }
  */


  override def createCode(writer:CodeWriter):SegmentReturn = {
    this.statements.toList.map(writer.createCode(_)).reduceLeft(_ + _)
  }



}


object ComplexMultiplySegment {
    class Truncate(name:String,
                   clk:ClockControl,
                   out:ComplexSignal,
                   in1:ComplexSignal,
                   in2:ComplexSignal,
                   fixed:FixedType = FixedType.None,
                   internal:FixedType       = FixedType.None) extends ComplexMultiplySegment(name,clk,out,in1,in2,internal) {

     override def newMultiplier(name:String,output:ComplexSignal,input1:ComplexSignal,input2:ComplexSignal) =
        new Truncate(output.name,clk,output,input1,input2,this.internal)

    }

    class TruncateClip(name:String,clk:ClockControl,
                   out:ComplexSignal,
                   in1:ComplexSignal,
                   in2:ComplexSignal,
                   fixed:FixedType = FixedType.None,
                   internal:FixedType       = FixedType.None) extends ComplexMultiplySegment(name,clk,out,in1,in2,internal) {

      override def newMultiplier(name:String,output:ComplexSignal,input1:ComplexSignal,input2:ComplexSignal) =
        new TruncateClip(output.name,clk,output,input1,input2,this.internal)

      lazy override val clip:Boolean  = true
    }

    class Round(name:String,clk:ClockControl,
                out:ComplexSignal,
                in1:ComplexSignal,
                   in2:ComplexSignal,
                   fixed:FixedType = FixedType.None,
                   internal:FixedType       = FixedType.None) extends ComplexMultiplySegment(name,clk,out,in1,in2,internal) {

      override def newMultiplier(name:String,output:ComplexSignal,input1:ComplexSignal,input2:ComplexSignal) =
        new Round(output.name,clk,output,input1,input2,this.internal)

      lazy override val round:Boolean = true

    }

    class RoundClip(name:String,clk:ClockControl,
                   out:ComplexSignal,
                   in1:ComplexSignal,
                   in2:ComplexSignal,
                   fixed:FixedType = FixedType.None,
                   internal:FixedType       = FixedType.None) extends ComplexMultiplySegment(name,clk,out,in1,in2,internal) {

      override def newMultiplier(name:String,output:ComplexSignal,input1:ComplexSignal,input2:ComplexSignal) =
        new RoundClip(output.name,clk,output,input1,input2,this.internal)

      lazy override val round:Boolean = true
      lazy override val clip:Boolean  = true

    }




}

