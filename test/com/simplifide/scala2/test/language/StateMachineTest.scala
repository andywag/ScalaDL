package com.simplifide.scala2.test.language

import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.language.Module
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.block.state.{StateModel, State}
import com.simplifide.generate.blocks.basic.flop.ClockControl._
import com.simplifide.generate.blocks.basic.flop.ClockControl

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 8/3/11
 * Time: 7:54 PM
 * To change this template use File | Settings | File Templates.
 */

class StateMachineTest {

}

object StateMachineTest extends Module("state_machine") {

  val clk         = ClockControl("clk","reset")

  val signal1     = array("alpha1",WIRE,signed(8,6))(3)
  val signal2     = array("alpha2",WIRE,signed(8,6))(3)

  val condition1  = signal("condition1")
  val condition2  = signal("condition2")

  val state       = signal("state",REG, unsigned(2,0))
  val next        = signal("next" ,REG, unsigned(2,0))
  // State Machine Test
  val stateA = new State("a",0,List(signal1(0) ::= signal2(0)))
  val stateB = new State("b",1,List(signal1(1) ::= signal2(1)))
  val stateC = new State("c",2,List(signal1(2) ::= signal2(2)))

  val gr = StateModel((stateA -> stateB) ## (condition1 == condition2),
                      (stateB -> stateC) ## condition2,
                      (stateC -> stateA) ## condition2)

  state_machine(gr,clk,state,next)


}

object TestMain {
    def main(args:Array[String]) = {
     val mod = StateMachineTest.createModule
     System.out.println(mod.createCode(CodeWriter.Verilog))
  }
}