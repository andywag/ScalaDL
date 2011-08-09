package com.simplifide.generate.parser

import collection.mutable.ListBuffer
import model.{Signal, Expression}
import com.simplifide.generate.signal.SignalTrait

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/8/11
 * Time: 6:42 PM
 * To change this template use File | Settings | File Templates.
 */

trait SegmentHolder {

  val statements = new ListBuffer[Expression]()
  val signals    = new ListBuffer[Signal]()

  /** Attaches and assign statement */
  def assign(statement:Expression) = statements.append(statement)
  /** Attach siganl to the module */
  def attach(signal:SignalTrait):SignalTrait = {
    signals.append(signal)
    signal
  }


}