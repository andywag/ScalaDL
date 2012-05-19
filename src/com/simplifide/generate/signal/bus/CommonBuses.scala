package com.simplifide.generate.signal.bus

import com.simplifide.generate.signal.{SignalTrait, BusDirect, OpType}
import com.simplifide.generate.parser.SignalMethods


/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 5/18/12
 * Time: 2:31 PM
 * To change this template use File | Settings | File Templates.
 */

object CommonBuses extends SignalMethods {

  class DataValid(name:String, val busWidth:Int, opType:OpType) extends BusDirect(name,opType) {
    val data = SignalTrait(name + "_" + "dat",this.opType,U(busWidth,0))
    val vld  = SignalTrait(name + "_" + "vld",this.opType)
    override val signals = List(data,vld)
  }
}