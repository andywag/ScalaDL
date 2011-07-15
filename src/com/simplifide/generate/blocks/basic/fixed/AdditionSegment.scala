package com.simplifide.generate.blocks.basic.fixed

 /*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

import com.simplifide.generate.blocks.basic.SimpleStatement
import java.lang.Boolean
import com.simplifide.generate.signal.{Constant, SignalTrait, OpType, FixedType}
import com.simplifide.generate.generator.{BasicSegments, SimpleSegment, CodeWriter, SegmentReturn}

abstract class AdditionSegment(val name:String,
                               val terms:List[SimpleSegment],
                               val outFixed:FixedType,
                               val internal:Option[FixedType]) extends SimpleSegment{

  override val fixed:FixedType = outFixed // Kludge
  override def numberOfChildren:Int = terms(0).numberOfChildren
  override def child(index:Int):SimpleSegment = newSegment(terms.map(x => x.child(index)),index)

  def newSegment(terms:List[SimpleSegment], index:Int):AdditionSegment

  def round:Boolean = false
  def clip:Boolean  = false

  /** Calculates the Real Internal Value which is used for the initial calculation */
  val realInternal:FixedType = {
    if (internal != None) internal.get    // If the Internal Exists Return this
    terms.map(_.fixed).reduceLeft((x:FixedType,y:FixedType) => x.union(y))
  }

  val realRound:Boolean = round && (realInternal.frac > outFixed.frac)
  val realClip:Boolean  = clip  && (realInternal.integer > outFixed.integer)

  private val shift = realInternal.frac-outFixed.frac

  /** Rounding Term if round is required */
  val roundTerm:SimpleSegment =
    new AdditionTerm.AddTerm(Constant.newConstant(math.pow(2.0,shift-1).toInt,realInternal.width))

  /** Output of the initial round block */
  val internalSignal = SignalTrait(name + "_i",OpType.Signal,realInternal)




  override def createCode(writer:CodeWriter):SegmentReturn = {
    val internalRound = if (realRound) List(roundTerm) else List()
    val addTerms:List[SimpleSegment] = terms.map(x => x.sliceFixed(realInternal)) ::: internalRound
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

object AdditionSegment {

  

  
  class TruncateFixed(name:String,terms:List[SimpleSegment],outFixed:FixedType,internal:Option[FixedType]) extends
    AdditionSegment(name,terms,outFixed,internal) {

    def newSegment(terms:List[SimpleSegment],index:Int):AdditionSegment =
      new TruncateFixed(name + "_" + index,terms,outFixed,internal)


  }
  
  class TruncateClip(name:String,terms:List[SimpleSegment],outFixed:FixedType,internal:Option[FixedType]) extends
    AdditionSegment(name,terms,outFixed,internal) {

     override def newSegment(terms:List[SimpleSegment],index:Int):AdditionSegment =
        new TruncateClip(name + "_" + index,terms,outFixed,internal)

    override def clip:Boolean = true


  }
  
  class Round(name:String,
              terms:List[SimpleSegment],
              outFixed:FixedType,
              internal:Option[FixedType]) extends AdditionSegment(name,terms,outFixed,internal) {

      override def newSegment(terms:List[SimpleSegment],index:Int):AdditionSegment =
        new Round(name + "_" + index,terms,outFixed,internal)

      override def round:Boolean = true

  }
  
   /** Most often used statement which contains a round and a clip */
   class RoundClip(name:String,terms:List[SimpleSegment],outFixed:FixedType,internal:Option[FixedType]) extends
        AdditionSegment(name,terms,outFixed,internal) {
      override def newSegment(terms:List[SimpleSegment],index:Int):AdditionSegment =
        new RoundClip(name + "_" + index,terms,outFixed,internal)

      override def clip:Boolean = true
      override def round:Boolean = true


    }
  
  
  
}
