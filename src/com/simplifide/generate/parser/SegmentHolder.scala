package com.simplifide.generate.parser

import collection.mutable.ListBuffer
import model.{Model, SignalType, Signal, Expression}
import com.simplifide.generate.signal.{RegisterTrait, SignalTrait}
import com.simplifide.generate.generator.{SimpleSegment, CodeWriter}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/8/11
 * Time: 6:42 PM
 * To change this template use File | Settings | File Templates.
 */

trait SegmentHolder extends SignalHolder{

  val statements = new ListBuffer[Expression]()

  /** List of all of the statements associated with this segment of code */
  def allStatements = autoFlops ::: statements.toList

  /** Attaches and assign statement */
  def assign(statement:Expression) = statements.append(statement)

  /** Create flops which are automatically create from registers */
  def autoFlops:List[SimpleSegment] = {
    val registers = this.signals.filter(x => x.isInstanceOf[RegisterTrait[_]]).map(x => x.asInstanceOf[RegisterTrait[_]])
    if (registers.length > 0) List(registers.map(x => x.createFlop).reduceLeft(_ + _)) else List()
  }


}