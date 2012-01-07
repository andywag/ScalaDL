package com.simplifide.scala2.test.basic

import com.simplifide.generate.TestConstants
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.project.{Entity, Project}
import com.simplifide.generate.signal.SignalTrait
import com.simplifide.generate.blocks.proc.ProcessorBus
import com.simplifide.generate.blocks.proc2.parser.RegisterParser
import com.simplifide.generate.language.Conversions._

/**
 * Created by IntelliJ IDEA.
 * User: awagner
 * Date: 12/28/11
 * Time: 1:24 PM
 * To change this template use File | Settings | File Templates.
 */

class ProcessorInterfaceTest extends Project {

  val location:String = TestConstants.locationPrefix + "outputs" + TestConstants.separator + "condition"
  // Create the Clock
  implicit val clk = ClockControl("clk","reset")
  // Main Module for the Design
  override val root = new ProcessorInterfaceTest.ProcessorEntity()

}

object ProcessorInterfaceTest {

  class ProcessorEntity()(implicit clk:ClockControl) extends Entity.Root("processor","processor") with RegisterParser {

    val processorBus = ProcessorBus(clk,
      SignalTrait("wrAddress",INPUT,U(3,0)),
      SignalTrait("wrValid",INPUT,U(1,0)),
      SignalTrait("wrData",INPUT,U(32,0)),
      SignalTrait("rdAddress",INPUT,U(3,0)),
      SignalTrait("rdValid",INPUT,U(1,0)),
      SignalTrait("rdData",REGOUT,U(32,0)))

    registerGroup(0) (
      read("gamma",15) at (2,0) comment ("Read Register for ...")
      readWrite("alpha",10) at (0,0)
      readWrite("delta",10) at (0,10)
      readWrite("beta",10)  at (1,0)
    )

    /*
    registerGroup(12) (
      //address("asdfas")
    )
    */

    assign(readDecoder.split)
    assign(writeDecoder.split)

  }

  def main(args:Array[String]) = {
    new ProcessorInterfaceTest().createProject2
  }

}