package com.simplifide.generate.parser

import collection.mutable.ListBuffer
import model.{Model, SignalType, Signal, Expression}
import com.simplifide.generate.generator.{SimpleSegment, CodeWriter}
import com.simplifide.generate.blocks.basic.misc.Comment
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.signal.{OpType, RegisterTrait, SignalTrait}
import com.simplifide.generate.blocks.basic.SimpleStatement

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/8/11
 * Time: 6:42 PM
 * To change this template use File | Settings | File Templates.
 */

trait SegmentHolder extends SignalHolder{

  val statements = new ListBuffer[Expression]()

  def getStatement(signal:SignalTrait):Option[SimpleStatement] = {
    statements.find(x => signal.generalEquals(x.asInstanceOf[SimpleStatement].output)) match {
      case Some(x) => if (x.isInstanceOf[SimpleStatement]) Some(x.asInstanceOf[SimpleStatement]) else None
      case _ => None
    }
  }

  /** List of all of the statements associated with this segment of code */
  def allStatements = autoFlops ::: statements.toList

  /** Attaches and assign statement */
  def assign(statement:Expression) =
    statements.append(statement.asInstanceOf[SimpleSegment])

  def /- (value:String) = comment(value)
  /** Adds a comment to the code */
  def comment(value:String) = statements.append(new Comment.SingleLine(value))



  /** Create flops which are automatically create from registers */
  def autoFlops:List[SimpleSegment] = {
    val registers = this.signals.filter(x => x.isInstanceOf[RegisterTrait[_]]).map(x => x.asInstanceOf[RegisterTrait[_]])
    if (registers.length > 0) List(registers.map(x => x.createFlop).reduceLeft(_ + _)) else List()
  }


}