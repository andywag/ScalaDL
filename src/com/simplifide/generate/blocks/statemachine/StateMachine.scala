package com.simplifide.generate.blocks.statemachine

import com.simplifide.generate.parser.model.Model
import com.simplifide.generate.signal.{ParameterTrait, SignalTrait}
import com.simplifide.generate.blocks.basic.flop.{SimpleFlopList, ClockControl}
import util.matching.Regex.Groups
import com.simplifide.generate.parser.block.state.{State, StateModel}
import com.simplifide.generate.blocks.basic.SimpleStatement
import com.simplifide.generate.generator.{BasicSegments, SegmentReturn, CodeWriter, SimpleSegment}
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.blocks.basic.condition.{ConditionStatementFunctional, ConditionStatement2, NewCaseStatement}
import com.simplifide.generate.blocks.basic.state.AlwaysProcess
import com.simplifide.generate.blocks.basic.state.AlwaysProcess.AlwaysStar

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/3/11
 * Time: 8:24 PM
 * To change this template use File | Settings | File Templates.
 */

class StateMachine(val model:StateModel, val clk:ClockControl, current:SignalTrait, val next:SignalTrait) extends SimpleSegment{

  val states = model.groups.keys.toList.sortBy(_.index)
  val params:List[SignalTrait] = states.map(x => ParameterTrait(x.name,x.index)).toList



  def fsmStatememt:SimpleSegment = {
    def caseItem(state:State,transitions:List[State.Transition]) = {
      val tran = transitions.map(x => (OptionExpression2OptionSegment(x.expr),List(new SimpleStatement.Reg(this.next,BasicSegments.Ident(x.destination.name)))))
      val condition = ConditionStatementFunctional(tran)
      NewCaseStatement.Item(BasicSegments.Ident(state.name),condition)
    }

    val states = model.groups.map(x => (x._1,x._2)).toList.sortBy(x => x._1.index)
    val states2 = states.map(x => caseItem(x._1,x._2))
    val cas  = new NewCaseStatement(this.current,states2)
    new AlwaysStar(None,cas,List())
  }

  def actionStatement:SimpleSegment = {
    val expressions = states.map(x => NewCaseStatement.Item(BasicSegments.Ident(x.name),BasicSegments.ListSurround(x.expressions)))
    val cas    = new NewCaseStatement(this.next,expressions)
    new AlwaysStar(None,cas,List())
  }

  override def createCode(writer:CodeWriter):SegmentReturn = {
    // Create the State Diagram
    val flop = SimpleFlopList.simple(clk,current,next) // State Flop
    SegmentReturn.combineFinalReturns(writer,List(flop,this.fsmStatememt),this.params)
  }

}