package com.simplifide.scala2.test.language

import com.simplifide.generate.language.Module
import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.parser.block.state.StateModel._
import com.simplifide.generate.parser.block.state.{State, StateModel}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/12/11
 * Time: 8:44 AM
 * To change this template use File | Settings | File Templates.
 */

class ModuleTest {

}

object ModuleTest {


  object BusTest extends BusType {
    val alpha = SignalTrait("alpha")
    val beta  = SignalTrait("beta")

    override val signals = List(alpha,beta)

  }

  object TestCase extends Module("alpha") {

     val clk         = ClockControl("clk","reset")
     val n = clk
     val clk_signal  = signal(clk.getBus(OpType.Input))

     val bus_a       = bus("aaa",BusTest)
     val bus_b       = bus("bbb",BusTest)

     val signal1     = array("alpha1",INPUT,signed(8,6))(5)
     val signal2     = array("alpha2",INPUT,signed(8,6))(5)

     val condition1  = signal("condition1")
     val condition2  = signal("condition2")


     // Flop with Single Condition
     signal1   := signal1 @@ clk
     // Flop with multiple conditions
     signal1(0)   := (signal1(0) -> signal1(1)) @@ clk
      // Question Mark Statement
     signal1      := signal1 ? signal1 :: signal1
     signal1      := signal2 & signal2
     // Condition Statement

      $always_star(
        $if (condition1) (
          signal2 ::= signal1
        ),
        $else_if (condition2) (
          signal2 ::= signal1
        )
      )

      $always(condition1,condition2)(
        $if (condition1) (
          signal2 ::= signal1
        ),
        $else_if (condition2) (
          signal2 ::= signal1
        )
      )

      // Case Statement
      $always_star(
        $case(condition1) (
          condition1 -> (signal1 ::= signal2),
          condition2 -> (signal1 ::= signal2)
        )
      )

      /*
      // Basic Math Statements
      y(n) := x(n)    + a0*y(n-2) + a1*y(n-3)
      z(n) := b0*y(n) - b1*y(n-1) + b2*y(n-2)
      // State Machine Test
      val stateA = new State("a",0,List(x ::= y, z ::= y))
      val stateB = new State("b",1,List(x ::= y, z ::= y))
      val stateC = new State("c",2,List(x ::= y, z ::= y))

      val gr = StateMachine((stateA -> stateB) ## condition1,
                            (stateA -> stateC) ## condition2,
                            (stateB -> stateC) ## condition2)
      */
  }

  def main(args:Array[String]) = {
     val mod = TestCase.createModule
     System.out.println(mod.createCode(CodeWriter.Verilog))
  }
}