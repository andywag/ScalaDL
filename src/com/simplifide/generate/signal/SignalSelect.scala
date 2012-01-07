package com.simplifide.generate.signal

import com.simplifide.generate.signal.FixedType.Signing

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 12/2/11
 * Time: 10:46 AM
 * To change this template use File | Settings | File Templates.
 */

class SignalSelect(val original:SignalTrait, val top:Int, val bottom:Int) extends SignalTrait {

  override val name:String = original.name + "[" + (if (top == bottom) top else (top + ":" + bottom)) + "]"
  /** Type of Signal */
  override val opType:OpType = original.opType
  /** Fixed type of signal */
  override val fixed:FixedType = FixedType(Signing.UnSigned,top-bottom + 1,0)

  override def getOpType:OpType = this.opType

  def newSignal(nam:String,optype:OpType = this.opType, fix:FixedType = this.fixed):SignalTrait = this

}