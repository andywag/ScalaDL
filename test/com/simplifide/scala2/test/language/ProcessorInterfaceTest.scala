package com.simplifide.scala2.test.language

import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.parser.block.state.StateModel._
import com.simplifide.generate.parser.block.state.{State, StateModel}
import com.simplifide.generate.parser.RegisterMapHolder
import com.simplifide.generate.blocks.proc.{ProcessorBus, Address, RegisterMap}
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.hier2.Entity
import com.simplifide.generate.TestConstants
import com.simplifide.generate.project2.{Project, Module}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/12/11
 * Time: 8:44 AM
 * To change this template use File | Settings | File Templates.
 */

class ProcessorInterfaceTest {

}

object ProcessorInterfaceTest {

  class Ent()(implicit clk:ClockControl) extends Entity.Root("processor_interface","processor_interface") {

    val processorBus = ProcessorBus(clk,
      SignalTrait("wrAddress",INPUT,U(3,0)),
      SignalTrait("wrValid",INPUT,U(1,0)),
      SignalTrait("wrData",INPUT,U(32,0)),
      SignalTrait("rdAddress",INPUT,U(3,0)),
      SignalTrait("rdValid",INPUT,U(1,0)),
      SignalTrait("rdData",OUTPUT,U(32,0)))

    override val signals = processorBus.signals
    override def createModule = new Mod(this).createModule

  }

  class Mod(entity:Ent)(implicit clk:ClockControl) extends Module("processor_interface") with RegisterMapHolder {

      //import entity._

      val processorBus = entity.processorBus

      this.appendSignals(this.createRegisterSignals)

      sRegister(0,0 ,"a", 32) -- "Test Register"
      sRegister(1,0 ,"b", 32) -- <p>Html Documentation <i>Test</i></p>
      sRegister(2,0 ,"c", 16) -- "Test Register"
      sRegister(2,16,"d", 16) -- "Test Register"

      this.processor_interface(this)

  }

  object Proj extends Project {
    val location:String = TestConstants.locationPrefix + "language\\proc_output"

    implicit val clk = ClockControl("clk","reset")

    override val root = new Ent()
  }



  def main(args:Array[String]) = {
     Proj.createProject2
  }
}