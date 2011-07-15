package com.simplifide.generate.signal


/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 6/9/11
 * Time: 6:48 AM
 * To change this template use File | Settings | File Templates.
 */

trait BusTrait extends SignalTrait {

  val signals:List[SignalTrait]
  override val opType = OpType.Signal
  override val fixed:FixedType = FixedType.Simple


  def newBus(name:String,signals:List[SignalTrait]):BusTrait = new BusTrait.Bus(name,signals)

  override def numberOfChildren:Int = signals.length

  override def getChildren:List[SignalTrait] = signals


  override def createSlice(index:Int):SignalTrait =
    newBus(this.name + "_" + index,signals.map(x => x.createSlice(index)))



  override def slice(index:Int):SignalTrait  = signals(index)


}

object BusTrait {

  def newBus(name:String,signals:List[SignalTrait]):Bus = new Bus(name,signals)

  class Bus(override val name:String,override val signals:List[SignalTrait]) extends BusTrait {
    override def newSignal(nam:String,optype:OpType,fix:FixedType):SignalTrait = this

  }

}