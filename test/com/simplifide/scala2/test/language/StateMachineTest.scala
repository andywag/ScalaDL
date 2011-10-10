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
import com.simplifide.generate.signal.SignalTrait

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/3/11
 * Time: 7:54 PM
 * To change this template use File | Settings | File Templates.
 */





class StateMachineTest {

}

object StateMachineTest {

  class StateMachineProject extends Project {
    val location:String = TestConstants.locationPrefix + "language\\sm_output"
    implicit val clk = ClockControl("clk","reset")


    override val root = new StateMachineEntity()

  }


  class StateMachineEntity()(implicit clk:ClockControl) extends Entity.Root("state_machine","state_machine") {
    val condition = SignalTrait("condition",INPUT,U(3,0))
    val result    = SignalTrait("state",OUTPUT,U(3,0))
    override val signals = clk.allSignals(INPUT) ::: List(condition,result)

    override def createModule = new StateMachine(this).createModule
  }


class StateMachine(entity:StateMachineEntity)(implicit clk:ClockControl) extends Module("state_machine") {
  import entity._



  val state       = signal("state",REG, unsigned(2,0))
  val next        = signal("next" ,REG, unsigned(2,0))

  description = Some(<p>This is a basic <i>state machine</i> used for testing.</p> )
  // State Machine Test

  val stateA = State("a",0) -- <p><i>State</i> A</p>
  val stateB = State("b",1) -- "State B"
  val stateC = State("c",2) -- "State C"
  val stateD = State("d",3) -- "State D"
  val stateE = State("e",4) -- "State E"

  val gr = StateModel((stateA -> stateB) ## (condition == 2)             -- "StateA to StateB Comment",
                      (stateB -> stateC) ## (condition == 4)             -- "StateB to StateC Comment",
                      (stateC -> stateD) ## (condition == 0)             -- "StateC to StateD Comment"  ,
                      (stateD -> stateE) ## (condition == 1)             -- "StateD to StateE Comment",
                      (stateE -> stateA) ## (condition == 3)             -- "StateE to StateA Comment",
                      (stateE -> stateC) ## (condition == 2)             -- "StateE to StateC Comment")

  state_machine(gr,clk,state,next, "Basic State Machine")


}


    def main(args:Array[String]) = {
      new StateMachineProject().createProject2
    }
}