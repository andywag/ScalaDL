package com.simplifide.scala2.test.language

import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.parser.block.state.StateModel._
import com.simplifide.generate.parser.block.state.{State, StateModel}
import com.simplifide.generate.project2.{Project, Module}
import com.simplifide.generate.hier2.Entity
import com.simplifide.generate.TestConstants
import com.simplifide.generate.language.Conversions._

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

  class Ent()(implicit clk:ClockControl) extends Entity.Root("test","test") {
    override val signals = List()
    override val createModule = new Mod().createModule
  }

  class Mod()(implicit val clk:ClockControl) extends Module("alpha") {

     val n = clk
     val clk_signal  = appendSignal(clk.getBus(OpType.Input))

     val bus_a       = bus("aaa",BusTest)
     val bus_b       = bus("bbb",BusTest)

     val signal1     = array("alpha1",REG,signed(8,6))(5)
     val signal2     = array("alpha2",REG,signed(8,6))(5)

     val condition1  = signal("condition1")
     val condition2  = signal("condition2")

     /*
     // Flop with Single Condition
     signal1   := (signal1 + 10) @@ clk
     // Flop with multiple conditions
     signal1(0)   := (signal1(0) -> signal1(1)) @@ clk
      // Question Mark Statement
     signal1      := signal1 ? signal1 :: signal1
     signal1      := signal2 & signal2
     // Condition Statement


      $always_star(
        $if (condition1 == 10) (
          signal2 ::= signal1
        ),
        $else_if (condition2) (
          signal2 ::= signal1
        )
      )
      */
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
          condition1 -> (signal1(0) ::= signal2(0)),
          condition2 -> (signal1 ::= signal2)
        )
      )
  }

  object Proj extends Project {
    val location:String = TestConstants.locationPrefix + "language\\module_output"
    implicit val clk         = ClockControl("clk","reset")

    override val root = new Ent()

  }



  def main(args:Array[String]) = {
     Proj.createProject2
  }
}