package com.simplifide.scala2.test.language

import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.parser.block.state.StateModel._
import com.simplifide.generate.parser.block.state.{State, StateModel}
import com.simplifide.generate.parser.RegisterMapHolder
import com.simplifide.generate.blocks.proc.{ProcessorBus, Address, RegisterMap}
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.TestConstants
import com.simplifide.generate.project.{Entity, Project, Module}

/**
 * This test case is for a processor simple processor interface. The advantage of this code is that all of the information
 * is stored in the same place so that both the verilog code, documentation, and software interfaces can be automatically
 * generated.
 *
 * The design files can be found at       XXXX/proc/design/XXXX
 * The test files can be found at         XXXX/proc/test/XXXX
 * The html documentation can be found at XXXX/proc/doc/XXXX
 *
 */

class ProcessorInterfaceTest {

}

// TODO Convert the Processor Interface to the entity to allow outputs to be added to the entity
object ProcessorInterfaceTest {

  /** Top Level Project which contains the processor interface */
  object Proj extends Project {
    // Sets the location where the projects outputs will be stored
    val location:String = TestConstants.locationPrefix + "outputs" + TestConstants.separator + "proc"
    // Create the clock for the design
    implicit val clk = ClockControl("clk","reset")
    // Set the Top Level Module for the design
    override val root = new Ent()
  }

  class Ent()(implicit clk:ClockControl) extends Entity.Root("processor_interface","processor_interface") {
    // Creates a bus which contains the signal required for the processor interface
    val processorBus = ProcessorBus(clk,
      SignalTrait("wrAddress",INPUT,U(3,0)),
      SignalTrait("wrValid",INPUT,U(1,0)),
      SignalTrait("wrData",INPUT,U(32,0)),
      SignalTrait("rdAddress",INPUT,U(3,0)),
      SignalTrait("rdValid",INPUT,U(1,0)),
      SignalTrait("rdData",OUTPUT,U(32,0)))
    // Set the signals for this entity
    override val signals = clk.allSignals(INPUT) ::: processorBus.signals
    // Create the module associated with this entity
    override def createModule = new Mod(this).createModule

  }

  class Mod(entity:Ent)(implicit clk:ClockControl) extends Module("processor_interface") with RegisterMapHolder {
      // Import all of the internal values from the entity
      import entity._

      val processorBus = entity.processorBus

      sRegister(0,0 ,"a", 32) -- "Test Register"
      sRegister(1,0 ,"b", 32) -- <p>Html Documentation <i>Test</i></p>
      sRegister(2,0 ,"c", 16) -- "Test Register"
      sRegister(2,16,"d", 16) -- "Test Register"

      //entity.appendSignals(this.createRegisterSignals)
      this.processor_interface(this)

  }





  def main(args:Array[String]) = {
     Proj.createProject2
  }
}