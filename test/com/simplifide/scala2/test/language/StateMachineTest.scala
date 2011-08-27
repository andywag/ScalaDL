package com.simplifide.scala2.test.language

import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.block.state.{StateModel, State}
import com.simplifide.generate.blocks.basic.flop.ClockControl._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.{Project, Module}
import java.lang.annotation.Documented

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/3/11
 * Time: 7:54 PM
 * To change this template use File | Settings | File Templates.
 */



class StateMachineProject extends Project {
  //val location:String = "C:\\home\\Generator\\test\\com\\simplifide\\scala2\\test\\language\\sm_output"
  //val location:String = "C:\\in_temp"
  val location:String = "/home/andy/simplifide_base/Generator/test/com/simplifide/scala2/test/language/sm_output"

  override val modules  = List(new StateMachine().createModule)   // List of modules contained in this project

}

class StateMachine extends Module("state_machine") {

  implicit val clk         = ClockControl("clk","reset")

  val signal1     = array("alpha1",WIRE,signed(8,6))(3)
  val signal2     = array("alpha2",WIRE,signed(8,6))(3)

  val condition1  = signal("cond1")
  val condition2  = signal("cond2")
  val condition3  = signal("cond3")
  val condition4  = signal("cond4")
  val condition5  = signal("cond5")


  val state       = signal("state",REG, unsigned(2,0))
  val next        = signal("next" ,REG, unsigned(2,0))

  description = Some(<p>This is a basic <i>state machine</i> used for testing.</p> )
  // State Machine Test

  val stateA = State("a",0,List(signal1(0) ::= signal2(0))) -- <p><i>State</i> A</p>
  val stateB = State("b",1,List(signal1(1) ::= signal2(1))) -- "State B"
  val stateC = State("c",2,List(signal1(2) ::= signal2(2))) -- "State C"
  val stateD = State("d",3,List(signal1(2) ::= signal2(2))) -- "State D"
  val stateE = State("e",4,List(signal1(2) ::= signal2(2))) -- "State E"

  val gr = StateModel((stateA -> stateB) ## (signal1(0) == signal2(0)) -- "StateA to StateB Comment",
                      (stateB -> stateC) ## condition1                 -- "StateB to StateC Comment",
                      (stateC -> stateD) ## condition2                 -- "StateC to StateD Comment"  ,
                      (stateD -> stateE) ## condition3                 -- "StateD to StateE Comment",
                      (stateE -> stateA) ## condition4                 -- "StateE to StateA Comment",
                      (stateE -> stateC) ## condition5                 -- "StateE to StateC Comment")

  state_machine(gr,clk,state,next, "Basic State Machine")


}

class StateMachineTest {

}

object StateMachineTest {
    def main(args:Array[String]) = {
      new StateMachineProject().createProject
    }
}