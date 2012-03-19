package com.simplifide.generate.blocks.statemachine2.parser

import collection.mutable.ListBuffer
import com.simplifide.generate.blocks.statemachine2.{Transition, State}
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.parser.SegmentHolder


/**
 * Builder Class to create a statemachine description
 */
trait StateMachineParser {


  val states      = new ListBuffer[State]()
  val transitions = new ListBuffer[Transition]()

  /** Create a state and append it to the list of states */
  def state(name:String, index:Int)  = {
    val state = State(name,index)
    states.append(state)
    state
  }

  /** Calculate the final states by combining the transitions as well as the outputs */
  def finalStates:List[State] = {
    val transitionMap = transitions.toList.groupBy(_.source)
    states.toList.map(x =>
      transitionMap.get(x) match {
        case Some(y) => x.copy(transitions = y)
        case None    => x
      }
    )
  }
  
  def transition(transitionBuilder:TransitionBuilder*) =
    transitions.appendAll(transitionBuilder.flatMap(_.totalTransitions))

  /** Implicit conversion to function for defining transitions */
  implicit def state2TransitionBuilder(state:State) = TransitionBuilder(state)
  
}