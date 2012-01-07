package com.simplifide.generate.signal

import com.simplifide.generate.generator.SimpleSegment

/**
 * A group of signals
 */
class BusDirect(override val name:String,
                override val opType:OpType) extends SignalTrait {

  override val fixed:FixedType   = FixedType.Simple

  val signals = List[SignalTrait]()


  override def changeTestType:SignalTrait = new BusDirect(this.name,opType.testType)
  override def changeType(typ:OpType):SignalTrait = new BusDirect(this.name,typ)
    /** Changes the type of the signal. Mainly used for Input Output Changes during connections */
  override def reverseType:SignalTrait = new BusDirect(this.name,opType.reverseType)

  def newSignal(nam:String, optype:OpType,fix:FixedType):SignalTrait = new BusDirect.Impl(nam,optype,signals)

  override def connect(inputSignal:SignalTrait):Map[SignalTrait,SignalTrait] = {
    if   (inputSignal.isInstanceOf[BusDirect])
      (this.signals zip inputSignal.asInstanceOf[BusDirect].signals).map(x => (x._1,x._2)).toMap
    else
      Map(signal -> inputSignal)

  }

    //(this.signals zip inputSignal.signals).map(x => x.)



  override def numberOfChildren:Int = signals.length // Number of Signals is the length of the list of signals
  override def children:List[SignalTrait] = signals  // Signals are the total amount of children

  override def createSlice(index:Int,prefix:String=""):SignalTrait = new BusDirect(this.name + "_" + index,this.opType)

  override def child(index:Int):SignalTrait = this.children(index)
  override def slice(index:Int):SignalTrait   = this.children(index)


}

object BusDirect {

  def apply[T <: BusType](name:String, busType:T) = new Bus[T](name,busType)

  class Impl(name:String,
    opType:OpType,
    override val signals:List[SignalTrait]) extends BusDirect(name,opType)


}