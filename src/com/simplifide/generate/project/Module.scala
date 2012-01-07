package com.simplifide.generate.project

import com.simplifide.generate.generator.SimpleSegment
import com.simplifide.generate.parser.condition.Condition
import com.simplifide.generate.parser.model.{Expression, Signal, Model, SignalType}
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.block.state.StateModel
import com.simplifide.generate.blocks.statemachine.{StateDescriptionHtmlTable, StateTransitionHtmlTable, StateDotFile, StateMachine}
import com.simplifide.generate.parser.{RegisterMapHolder, ModuleParser, ObjectFactory}
import com.simplifide.generate.blocks.proc.RegisterMapHtml
import com.simplifide.generate.html.Description
import com.simplifide.generate.language.DescriptionHolder


// TODO remove the processor_interface and state_machine
/**
 * Class which defines a module which contains signals and statements
 */
class Module(override val name:String)(implicit clk:ClockControl) extends ModuleParser   {



  /** Create the statements for this module */
  //protected def createStatements:List[SimpleSegment] = this.statements.toList.map(x => x.asInstanceOf[SimpleSegment])
  /** Create the instances for this module */
  //protected def createInstances:List[EntityInstance] = List()

  /** Create a module provider from this module */
  /*def createModule:ModuleProvider = {
    this.transform
    val mod = ModuleProvider(name,
        this,
        this.signals.toList.map(x => x.asInstanceOf[SignalTrait]),
        this.createStatements,
        this.createInstances,
        this.extraFiles.toList)
    mod.description = this.description
    mod
  } */

}

object Module {

}