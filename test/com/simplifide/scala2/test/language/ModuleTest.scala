package com.simplifide.scala2.test.language

import com.simplifide.generate.language.Module
import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl

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

     /*
     // Flop with Single Condition
     signal1(0)   := signal1(1) @@ clk
     // Flop with multiple conditions
     signal1(0)   := (signal1(0) -> signal1(1)) @@ clk
      // Question Mark Statement
     signal1      := signal1 ? signal1 :: signal1
     signal1      := signal2 & signal2
     // Condition Statement
     */
    /*
      $if (condition1) (
        signal2 ::= signal1
      )
      $else_if (condition2) (
        signal2 ::= signal1
      )
     */
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
      /*$always_star(
        $case(condition1) (
          condition1 -> signal1 ::= signal2,
          condition2 -> signal1 ::= signal2
        )
      )*/
      // Basic Math Statements
      //signal1(n) <= .5*signal2(n-1) + .075*signal1(n-2)

  }

  def main(args:Array[String]) = {
     val mod = TestCase.createModule
     System.out.println(mod.createCode(CodeWriter.Verilog))
  }
}