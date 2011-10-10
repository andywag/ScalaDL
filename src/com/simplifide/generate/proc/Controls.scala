package com.simplifide.generate.proc

import com.simplifide.generate.signal.SignalTrait

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/19/11
 * Time: 2:25 PM
 * To change this template use File | Settings | File Templates.
 */

trait Controls {
  val signal:SignalTrait
  val index:Int
  val value:Int

  override def toString = signal + "(" + index + ")" + " = " + value

}

object Controls {
  def apply(signal:SignalTrait,index:Int,value:Int) = new Impl(signal,index,value)
  class Impl(override val signal:SignalTrait,override val index:Int,override val value:Int) extends Controls





}