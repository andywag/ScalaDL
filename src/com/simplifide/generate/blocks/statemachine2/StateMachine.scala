package com.simplifide.generate.blocks.statemachine2

import com.simplifide.generate.signal.{ParameterTrait, SignalTrait}
import com.simplifide.generate.blocks.basic.condition.{NewCaseStatement, ConditionStatement}
import com.simplifide.generate.blocks.basic.state.Always
import com.simplifide.generate.generator._
import com.simplifide.generate.blocks.basic.flop.{SimpleFlopList, ClockControl}


/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/3/11
 * Time: 8:24 PM
 * To change this template use File | Settings | File Templates.
 */

class StateMachine(val states:List[State],
  val current:SignalTrait)(implicit val clk:ClockControl) extends ComplexSegment{


  def createBody {
    // Create the transition related to a state
    def createState(state:State) = {
      def createIf(transition:Transition) = $ifo(transition.condition) $then  (this.current ::= transition.destination.parameter)
      // Creates the case condition
      $cases(state.parameter) $then ( // Creates the state block
        state.transitions.map(x => createIf(x)).reduceLeft(_+_).create // Create the full if statement clause
      )
    } 
    
    this.signal(states.map(_.parameter)) // Append the parameters to this code segment
    // Create a Case ParserStatement containing the transitions
    /- ("State Machine FunctionBody")
    $always_clk(clk) (
      this.current $match (
         states.map(createState(_))
      )
    )
    /*/- ("State Machine Test")
    $always_clk(clk) (
      this.current ::= this.next
    )
    */
    
  }
  /*
  val states = model.groups.keys.toList.sortBy(_.index)
  val params:List[SignalTrait] = states.map(x => ParameterTrait.Decimal(x.name,x.index.toString)).toList



  def fsmStatememt:SimpleSegment = {
    def caseItem(state:State,transitions:List[State.Transition]) = {
      val tran = transitions.map(x => (OptionExpression2OptionSegment(x.expr),List(new SimpleStatement.Reg(this.next,BasicSegments.Identifier(x.destination.name)))))
      val condition = ConditionStatement(tran)
      NewCaseStatement.Item(BasicSegments.Identifier(state.name),condition)
    }

    val states = model.groups.map(x => (x._1,x._2)).toList.sortBy(x => x._1.index)
    val states2 = states.map(x => caseItem(x._1,x._2))
    val cas  = new NewCaseStatement(this.current,states2)
    new Always.Star(None,cas,List())
  }

  def actionStatement:SimpleSegment = {
    val expressions = states.map(x => NewCaseStatement.Item(BasicSegments.Identifier(x.name),BasicSegments.BeginEnd(x.expressions)))
    val cas    = new NewCaseStatement(this.next,expressions)
    new Always.Star(None,cas,List())
  }

  override def createCode(implicit writer:CodeWriter):SegmentReturn = {
    // Create the State Diagram
    val flop = SimpleFlopList.simple(clk,current,next) // State Flop
    SegmentReturn.combine(writer,List(flop,this.fsmStatememt),this.params)
  }
  */
}