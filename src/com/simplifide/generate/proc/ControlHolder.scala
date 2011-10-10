package com.simplifide.generate.proc

import com.simplifide.generate.parser.SegmentHolder
import com.simplifide.generate.generator.SimpleSegment

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 9/21/11
 * Time: 12:42 PM
 * To change this template use File | Settings | File Templates.
 */

trait ControlHolder {

  /** Controls which are included in this block */
  def controls:List[Controls] = List()
  /** Determines whether this control signal matches the current control */
  def controlMatch(actual:SimpleSegment,statements:SegmentHolder):Boolean = false
  /** Creates a list of controls based on this object -- Tree Walking */
  def createControl(actual:SimpleSegment,statements:SegmentHolder,index:Int):List[Controls] = List()

}