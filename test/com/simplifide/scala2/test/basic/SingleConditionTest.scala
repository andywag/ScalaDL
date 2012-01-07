package com.simplifide.scala2.test.basic

import com.simplifide.generate.TestConstants
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.project.{ Entity, Project}


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

class SingleConditionTest {

}

object SingleConditionTest {

  /** Project which contains a simple state machine example */
  class StateMachineProject extends Project {
    // Set the Base Location for the Project
    val location:String = TestConstants.locationPrefix + "outputs" + TestConstants.separator + "condition"
    // Create the Clock
    implicit val clk = ClockControl("clk","reset")
    // Main Module for the Design
    override val root = new StateMachineEntity()
    // Defines the Tests for this project
    //override val tests    = List(Test(new TestCase(root)))
    // Selects the simulator for this module - ISIM for this case
    //override val testType = Some(new Isim(this))
  }

  //** Entity which contains the state
  class StateMachineEntity()(implicit clk:ClockControl) extends Entity.Root("state_machine","state_machine") {
    // Creation of the Input and Output Signals for the test
    val condition = signal("condition",INPUT,U(3,0))
    val result    = signal("state",REGOUT,U(3,0))
    // Adding the Input and Output Signals to the module
    override val entitySignals = clk.allSignals(INPUT) ::: List(condition,result)
    // Module Creation
    val alpha    = array("ttt")(5)

    val counter  = signal("counter",REG,U(5,0))
    
    val wrEnable = signal("wrEnable",REG)
    val temp     = signal("temp")

    val tt = G(wrEnable,temp)

    wrEnable := wrEnable ? 0 :: 1

    counter := $iff (counter == 5) $then 0 $else (counter + 3) $at (clk)
    
    


    tt := tt


    tt := (
      $iff (temp) $then (
        $iff (temp) $then (
          $iff (temp) $then (
            tt
          )
        )
        $else  (tt)
      )
      $else_if (temp) $then {tt}
    )


    $always_body(
      $iff (temp) $then (
        wrEnable ::= wrEnable,
        $iff (temp) $then (wrEnable ::= wrEnable)
      )
    )



    wrEnable := wrEnable
    tt := tt



    tt := wrEnable $match (
      $cases(C(1,0))     $then  (
        $iff (wrEnable) $then (
          tt
        )
        $else (tt)
      )
      $cases(C(1,1))     $then  (tt)
    ) $at (clk)



    wrEnable := C(1,0)

    $always_body(
      $iff(wrEnable) $then (
        //wrEnable ::= C(1,0)
        $iff (wrEnable) $then  (
          wrEnable ::= C(1,0)
        )
      )
    )


    $always_body(
      wrEnable $match (
        $cases(C(1,0)) $then (
          wrEnable ::= C(1,0),
          wrEnable ::= C(1,0)
        )  
        $cases(C(1,1)) $then (
          wrEnable ::= C(1,1)
        )
      )
    )



    //tt(wrEnable) := tt(temp)

    //G(wrEnable,temp)   := H(wrEnable -> temp)
    
    //G(wrEnable,temp) := G(temp,wrEnable)


  }



  def main(args:Array[String]) = {
    new StateMachineProject().createProject2
  }
}