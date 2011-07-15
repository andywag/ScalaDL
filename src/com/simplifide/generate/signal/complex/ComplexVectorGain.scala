package com.simplifide.generate.signal.complex

import com.simplifide.generate.signal.{SignalTrait, BusTrait, OpType, FixedType}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 3/7/11
 * Time: 8:49 AM
 * To change this template use File | Settings | File Templates.
 */

/** Class which contains a complex vector signal as well as a gain signal */
class ComplexVectorGain(val signal:ComplexVectorArray, val gain:SignalTrait) extends BusTrait {

  val len = signal.len

  override val name = signal.name
  override val fixed:FixedType = signal.fixed

  val signals:List[SignalTrait] = List(signal,gain)

  val internalFixed:FixedType = signal.ifixed

  override def newSignal(nam:String,optype:OpType,fix:FixedType):SignalTrait = this

  def getAllSignals:List[SignalTrait] = List(signal,gain)

  def createSignal(name:String,fixed:FixedType):ComplexVectorGain = {
    createSignal(name,fixed,OpType.Signal)
  }

  def createSignal(name:String,fixed:FixedType,optype:OpType):ComplexVectorGain = {
    val sig  = ComplexVectorArray.newSignal(name,optype,fixed,signal.len)
    val ugain = SignalTrait(name + "_gain",optype,gain.fixed)
    new ComplexVectorGain(sig,ugain)
  }



}
object ComplexVectorGain {

   def newSignal(name:String,opType:OpType,fixed:FixedType,len:Int):ComplexVectorGain = {
       ComplexVectorGain.newSignal(name,opType,fixed,len,FixedType.signed(5,0))
   }
   def newSignal(name:String,opType:OpType,fixed:FixedType,len:Int,gfix:FixedType) = {
     val sig = ComplexVectorArray.newSignal(name,opType,fixed,len)
     val gain = SignalTrait(name+"_gain",opType,gfix)
     new ComplexVectorGain(sig,gain)
   }

}