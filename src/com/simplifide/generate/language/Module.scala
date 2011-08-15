package com.simplifide.generate.language

import com.simplifide.generate.project.ModuleProvider
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.parser.condition.Condition
import com.simplifide.generate.parser.model.{Expression, Signal, Model, SignalType}
import com.simplifide.generate.parser.{ModuleParser, ObjectFactory, SignalParser}
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.block.state.StateModel
import com.simplifide.generate.blocks.statemachine.StateMachine
import com.simplifide.generate.hier.HierarchyModule

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/15/11
 * Time: 9:11 PM
 * To change this template use File | Settings | File Templates.
 */

class Module(val name:String) extends ModuleParser  {






  /** Create a state machine based on a state model */
  def state_machine(model:StateModel,clk:ClockControl,state:SignalTrait, next:SignalTrait) = {
    this.assign(new StateMachine(model,clk,state,next))
  }


  def createModule:ModuleProvider = {
      this.transform
      ModuleProvider(name,
        this.signals.toList.map(x => x.asInstanceOf[SignalTrait]),
        this.statements.toList.map(x => x.asInstanceOf[SimpleSegment]))
  }

}