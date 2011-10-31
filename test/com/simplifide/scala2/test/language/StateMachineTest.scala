package com.simplifide.scala2.test.language

import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.block.state.{StateModel, State}
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.TestConstants
import com.simplifide.generate.test.{Test, Isim, TestModule}
import com.simplifide.generate.project.{Entity, Project, Module}


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

class StateMachineTest {

}

object StateMachineTest {

  /** Project which contains a simple state machine example */
  class StateMachineProject extends Project {
    // Set the Base Location for the Project
    val location:String = TestConstants.locationPrefix + "outputs" + TestConstants.separator + "state"
    // Create the Clock
    implicit val clk = ClockControl("clk","reset")
    // Main Module for the Design
    override val root = new StateMachineEntity()
        // Defines the Tests for this project
    override val tests    = List(Test(new TestCase(root)))
    // Selects the simulator for this module - ISIM for this case
    override val testType = Some(new Isim(this))
  }

  //** Entity which contains the state
  class StateMachineEntity()(implicit clk:ClockControl) extends Entity.Root("state_machine","state_machine") {
    // Creation of the Input and Output Signals for the test
    val condition = signal("condition",INPUT,U(3,0))
    val result    = signal("state",REGOUT,U(3,0))
    // Adding the Input and Output Signals to the module
    override val signals = clk.allSignals(INPUT) ::: List(condition,result)
    // Module Creation
    override def createModule = new StateMachine(this).createModule
  }

  class StateMachine(entity:StateMachineEntity)(implicit clk:ClockControl) extends Module("state_machine") {
    import entity._

    // Description of the module
    description = Some(<p>This is a basic <i>state machine</i> used for testing.</p> )
    // Internal Signal Declarations
    val next        = signal("next" ,REG, unsigned(3,0))
    // State Definitions
    val stateA = State("a",0) -- <p><i>State</i> A</p>
    val stateB = State("b",1) -- "State B"
    val stateC = State("c",2) -- "State C"
    val stateD = State("d",3) -- "State D"
    val stateE = State("e",4) -- "State E"
    // State Machine Model Definition
    // The first clause with the arrow specifies the transition (stateA -> stateB)
    // The Second clause followed by the ## Specifies the transition condition  ## (condition == 2)
    val gr = StateModel((stateA -> stateB) ## (condition == C(3,2))             -- "StateA to StateB Comment",
                        (stateB -> stateC) ## (condition == C(3,2))             -- "StateB to StateC Comment",
                        (stateC -> stateD) ## (condition == C(3,1))             -- "StateC to StateD Comment"  ,
                        (stateD -> stateE) ## (condition == C(3,0))             -- "StateD to StateE Comment",
                        (stateE -> stateA) ## (condition == C(3,4))             -- "StateE to StateA Comment",
                        (stateE -> stateC) ## (condition == C(3,1))             -- "StateE to StateC Comment")
    // The state machine is added to the module
    state_machine(gr,clk,result,next, "Basic State Machine")
  }

    /** Test Case for the State Machine */
  class TestCase(val entity:StateMachineEntity)(implicit clk:ClockControl) extends TestModule("state_test",entity) {
    // Run the condition which controls the state machine like a counter
    // The assigment is handled by the --> key. The input is a function which input x mod 8
    entity.condition --> (x => x % 8,1000)
    this.createTest
  }


    def main(args:Array[String]) = {
      new StateMachineProject().createProject2
    }
}