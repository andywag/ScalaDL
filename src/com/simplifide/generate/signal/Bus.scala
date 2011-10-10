package com.simplifide.generate.signal

import com.simplifide.generate.generator.SimpleSegment

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 6/9/11
 * Time: 6:48 AM
 * To change this template use File | Settings | File Templates.
 */

class Bus(override val name:String,
          val busType:BusType) extends SignalTrait {


  override val opType            = OpType.Signal
  override val fixed:FixedType   = FixedType.Simple


  override def changeTestType:SignalTrait = new Bus(this.name,busType.changeTestType)
  override def changeType(typ:OpType):SignalTrait = new Bus(this.name,busType.changeType(typ))
    /** Changes the type of the signal. Mainly used for Input Output Changes during connections */
  override def reverseType:SignalTrait = new Bus(this.name,busType.reverseType)

  def newSignal(nam:String, optype:OpType,fix:FixedType):SignalTrait =
    new Bus(nam,this.busType)

  override def numberOfChildren:Int = busType.length

  override def children:List[SignalTrait] = busType.createSignals(this.name)
  override def createSlice(index:Int,prefix:String=""):SignalTrait = new Bus(this.name + "_" + index,this.busType)

  override def child(index:Int):SimpleSegment = this.children(index)
  override def slice(index:Int):SignalTrait   = this.children(index)


}

object Bus {

  def apply(name:String, busType:BusType) = new Bus(name,busType)


}