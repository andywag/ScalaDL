package com.simplifide.generate.test

import com.simplifide.generate.parser.ModuleParser
import com.simplifide.generate.generator.SimpleSegment
import collection.mutable.ListBuffer
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.blocks.test.Initial
import com.simplifide.generate.parser.model.Expression

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/22/11
 * Time: 11:38 AM
 * To change this template use File | Settings | File Templates.
 */

trait TestParser  {

  val initials = new ListBuffer[SimpleSegment]()

  def init(signal:SignalTrait, value:Long, delay:Int) = initials.append(new Initial.Delay(signal,value,delay))
  def init(signal:SignalTrait, value:Long) = initials.append(new Initial.Assignment(signal,value))
  def init(signal:SignalTrait, value:SimpleSegment) = initials.append(new Initial.AssignSegment(signal,value))
  def init(initial:SimpleSegment) = initials.append(initial)

}