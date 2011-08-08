package com.simplifide.generate.signal

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/3/11
 * Time: 9:17 PM
 * To change this template use File | Settings | File Templates.
 */

trait ParameterTrait extends SignalTrait {
  val value:Int
  override val fixed = FixedType.None
  override val opType = OpType.Param

  def newSignal(nam:String,optype:OpType = this.opType,fix:FixedType = this.fixed):SignalTrait =
    ParameterTrait(nam,this.value)

}

object ParameterTrait {
  def apply(name:String, value:Int) = new Implementation(name,value)

  class Implementation(override val name:String, override val value:Int) extends ParameterTrait {

  }
}