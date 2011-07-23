package com.simplifide.generate.signal

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

  def newSignal(nam:String, optype:OpType,fix:FixedType):SignalTrait =
    new Bus(nam,this.busType)

  override def numberOfChildren:Int = busType.length

  override def children:List[SignalTrait] = busType.createSignals(this.name)
  override def createSlice(index:Int):SignalTrait = new Bus(this.name + "_" + index,this.busType)
  override def slice(index:Int):SignalTrait  = this


}

object Bus {

  def apply(name:String, busType:BusType) = new Bus(name,busType)


}