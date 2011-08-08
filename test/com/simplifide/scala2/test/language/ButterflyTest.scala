package com.simplifide.scala2.test.language

import com.simplifide.generate.language.Module
import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.model.{SignalType, Expression}

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/12/11
 * Time: 8:44 AM
 * To change this template use File | Settings | File Templates.
 */

class ButterflyTest {

}

object ButterflyTest {


  object TestCase extends Module("alpha") {

     val clk         = ClockControl("clk","reset")
     val n = clk
     val clk_signal  = signal(clk.getBus(OpType.Input))

     val len = 5

     val sig1          = complex("sig1",WIRE,S(8,6))
     val sig2          = complex("sig2",WIRE,S(8,6))

     val sig_out       = complex("sig_out",WIRE,S(8,6))

     //sig_out := sig1 * sig2

      val a             = signal("num",INPUT,S(8,6))
      val b             = signal("den",INPUT,S(8,5))

      b := RC(a + b) @@ clk

      val iW = S(12,8)



  }



  def main(args:Array[String]) = {

     val mod = TestCase.createModule
     System.out.println(mod.createCode(CodeWriter.Verilog))



  }
}