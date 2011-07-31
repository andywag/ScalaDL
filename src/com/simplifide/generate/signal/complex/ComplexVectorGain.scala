package com.simplifide.generate.signal.complex

import com.simplifide.generate.signal.{SignalTrait, OpType, FixedType}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 3/7/11
 * Time: 8:49 AM
 * To change this template use File | Settings | File Templates.
 */

/** Class which contains a complex vector signal as well as a gain signal */

/*
class ComplexVectorGain(val signal:ComplexVectorArray, val gain:SignalTrait) extends BusTrait {

  val len = signal.len

  override val name1 = signal.name1
  override val fixed:FixedType = signal.fixed

  val signals:List[SignalTrait] = List(signal,gain)

  val internalFixed:FixedType = signal.ifixed

  override def newSignal(nam:String,optype:OpType,fix:FixedType):SignalTrait = this

  def allSignals:List[SignalTrait] = List(signal,gain)

  def createSignal(name1:String,fixed:FixedType):ComplexVectorGain = {
    createSignal(name1,fixed,OpType.Signal)
  }

  def createSignal(name1:String,fixed:FixedType,optype:OpType):ComplexVectorGain = {
    val sig  = ComplexVectorArray.newSignal(name1,optype,fixed,signal.len)
    val ugain = SignalTrait(name1 + "_gain",optype,gain.fixed)
    new ComplexVectorGain(sig,ugain)
  }



}
object ComplexVectorGain {

   def newSignal(name1:String,opType:OpType,fixed:FixedType,len:Int):ComplexVectorGain = {
       ComplexVectorGain.newSignal(name1,opType,fixed,len,FixedType.signed(5,0))
   }
   def newSignal(name1:String,opType:OpType,fixed:FixedType,len:Int,gfix:FixedType) = {
     val sig = ComplexVectorArray.newSignal(name1,opType,fixed,len)
     val gain = SignalTrait(name1+"_gain",opType,gfix)
     new ComplexVectorGain(sig,gain)
   }

}
*/