package com.simplifide.generate.language

import com.simplifide.generate.project.ModuleProvider
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.parser.condition.Condition
import com.simplifide.generate.parser.model.{Expression, Signal, Model, SignalType}
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.block.state.StateModel
import com.simplifide.generate.hier.{Instance, HierarchyModule}
import com.simplifide.generate.blocks.statemachine.{StateDescriptionHtmlTable, StateTransitionHtmlTable, StateDotFile, StateMachine}
import com.simplifide.generate.parser.{RegisterMapHolder, ModuleParser, ObjectFactory, SignalParser}
import com.simplifide.generate.blocks.proc.RegisterMapHtml
import com.simplifide.generate.html.Description

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/15/11
 * Time: 9:11 PM
 * To change this template use File | Settings | File Templates.
 */

class Module(val name:String) extends ModuleParser with DescriptionHolder  {


  def processor_interface(reg:RegisterMapHolder) = {
    this.assign(reg.createReadMux)
    this.assign(reg.createWriteMux)
    this.extraFiles.append(new RegisterMapHtml(name + ".html",reg.createRegisterMap))

  }

  /** Create a state machine based on a state model */
  def state_machine(model:StateModel,clk:ClockControl,state:SignalTrait, next:SignalTrait,description:String = "") = {
    this.assign(new StateMachine(model,clk,state,next))
    this.extraFiles.append(new StateDotFile(name + ".dot",model))
    this.extraFiles.append(new StateTransitionHtmlTable(name + "_tran.html",model))
    this.extraFiles.append(new StateDescriptionHtmlTable(name + "_state.html",model))

  }


  def createModule[T <: Module]:ModuleProvider[T] = {
    this.transform
    val mod = ModuleProvider[T](name,
        this.asInstanceOf[T],
        this.signals.toList.map(x => x.asInstanceOf[SignalTrait]),
        this.statements.toList.map(x => x.asInstanceOf[SimpleSegment]),
        List(),
        this.instances.toList,
        this.extraFiles.toList)
    mod.description = this.description
    mod
  }

}