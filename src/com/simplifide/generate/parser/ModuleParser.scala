package com.simplifide.generate.parser

import block.state.StateModel
import collection.mutable.ListBuffer
import com.simplifide.generate.blocks.proc.RegisterMapHtml
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.blocks.statemachine.{StateDescriptionHtmlTable, StateTransitionHtmlTable, StateDotFile, StateMachine}
import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.project.{ModuleProvider, EntityInstance}
import com.simplifide.generate.project.ModuleProvider._
import com.simplifide.generate.language.{DescriptionHolder, ExtraFile}
import items.{ExpressionGroupParser, SingleConditionParser, SingleCaseParser}

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 7/21/11
 * Time: 11:42 AM
 * To change this template use File | Settings | File Templates.
 */

// TODO Replace the extra file outputs for the processor interface and state machine

trait ModuleParser extends ConditionParser with SignalParser with DescriptionHolder with SingleConditionParser with
  SingleCaseParser with ExpressionGroupParser {

  val name:String
  val extraFiles = new ListBuffer[ExtraFile]()
  /** Splits the statements into groups */
  def transform = {
    val newStatements = this.allStatements.flatMap(x => if (x != null) x.split else List())
    this.statements.clear
    this.statements.appendAll(newStatements)
  }

    /** Adds a processor interface to this module */
  def processor_interface(reg:RegisterMapHolder) = {
    this.assign(reg.createReadMux)
    this.assign(reg.createWriteMux)
    //this.extraFiles.append(new RegisterMapHtml(name + ".html",reg.createRegisterMap))
  }

  /** Create a state machine based on a state model */
  def state_machine(model:StateModel,clk:ClockControl,state:SignalTrait, next:SignalTrait,description:String = "") = {
    this.assign(new StateMachine(model,clk,state,next))
    //this.extraFiles.append(new StateDotFile(name + ".dot",model))
    //this.extraFiles.append(new StateTransitionHtmlTable(name + "_tran.html",model))
    //this.extraFiles.append(new StateDescriptionHtmlTable(name + "_state.html",model))
  }

    /** Create the statements for this module */
  protected def createStatements:List[SimpleSegment] = this.statements.toList.map(x => x.asInstanceOf[SimpleSegment])
  /** Create the instances for this module */
  protected def createInstances:List[EntityInstance[_]] = List()

  /** Create a module provider from this module */
  def createModule:ModuleProvider = {
    this.transform
    val mod = ModuleProvider(name,
        null,
        this.signals.toList.map(x => x.asInstanceOf[SignalTrait]),
        this.createStatements,
        this.createInstances,
        this.extraFiles.toList)
    mod.description = this.description
    mod
  }

}