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

class SignalProcessingTest {

}

object SignalProcessingTest {


  object BusTest extends BusType {
    val alpha = SignalTrait("alpha")
    val beta  = SignalTrait("beta")

    override val signals = List(alpha,beta)

  }

  object TestCase extends Module("alpha") {

     val clk         = ClockControl("clk","reset")
     val n = clk
     val clk_signal  = signal(clk.getBus(OpType.Input))


     val constant1      = constant(.5,S(8,6))

     val signal_in1     = signal("signal_in1",INPUT,S(8,6))
     val signal_in2     = signal("signal_in2",INPUT,S(8,6))
     val signal_out     = signal("signal_out",OUTPUT,S(14,11))


     //signal_out := RC(signal_in1 + signal_in2)
     signal_out := RC(signal_in1 + signal_in2) + RC(signal_in1 + signal_in2)
     //signal_out := RC(signal_in1)

  }

  def main(args:Array[String]) = {

     val mod = TestCase.createModule

     System.out.println(mod.createCode(CodeWriter.Verilog))
  }
}