package com.simplifide.scala2.test.language

import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.block.state.{StateModel, State}
import com.simplifide.generate.blocks.basic.flop.ClockControl._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.project2.{Project, Module}
import java.lang.annotation.Documented
import com.simplifide.generate.hier2.Entity
import com.simplifide.scala2.test.language.SignalProcessingTest.TestCase
import com.simplifide.generate.TestConstants
import com.simplifide.generate.signal.{Constant, SignalTrait}
import com.simplifide.generate.signalproc.Filter
import com.simplifide.generate.signal.Constant._
import com.simplifide.generate.test.Test._
import com.simplifide.generate.test.{Test, Isim, TestModule}




/**
 * This test case is a simple state machine which shows the simplified syntax which can be used for a state machine
 * construction. For the case of the state machine additional documentation is created in the dot directory which
 * contains a .dot file which can be used for a diagram as well as some html descriptions.
 *
 * The design files can be found at       XXXX/state/design/XXXX
 * The test files can be found at         XXXX/state/test/XXXX
 * The html documentation can be found at XXXX/state/doc/XXXX
 *
 */

class CounterTest {

}

object CounterTest {

  /** Project which contains a simple state machine example */
  object CounterProject extends Project {
    // Set the Base Location for the Project
    val location:String = TestConstants.locationPrefix + "outputs" + TestConstants.separator + "counter"
    // Create the Clock
    implicit val clk = ClockControl("clk","reset")
    // Main Module for the Design
    override val root = new CounterEntity()
        // Defines the Tests for this project
    override val tests    = List(Test(new TestCase(root)))
    // Selects the simulator for this module - ISIM for this case
    override val testType = Some(new Isim(this))
  }

  //** Entity which contains the state
  class CounterEntity()(implicit clk:ClockControl) extends Entity.Root("counter","counter") {
    // Adding the Input and Output Signals to the module
    override val signals = clk.allSignals(INPUT)
    // Module Creation
    override def createModule = new CounterModule(this).createModule
  }

  class CounterModule(entity:CounterEntity)(implicit clk:ClockControl) extends Module("counter") {
    import entity._
    val count = signal("counter",REG,U(8,0))

    //count := count + 1
    count := ((count == C(32)) ? 0 :: count + 1) @@ clk
    //count := (count + 1.0) @@ clk

  }

    /** Test Case for the State Machine */
  class TestCase(val entity:CounterEntity)(implicit clk:ClockControl) extends TestModule("state_test",entity) {

  }


    def main(args:Array[String]) = {
      CounterTest.CounterProject.createProject2
    }
}