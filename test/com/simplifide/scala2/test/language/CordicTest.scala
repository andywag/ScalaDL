package com.simplifide.scala2.test.language

import com.simplifide.generate.language.Module
import com.simplifide.generate.generator.CodeWriter
import com.simplifide.generate.signal._
import com.simplifide.generate.blocks.basic.flop.ClockControl
import com.simplifide.generate.language.Conversions._
import com.simplifide.generate.parser.model.{SignalType, Expression}
import com.simplifide.generate.blocks.basic.misc.Comment

/**
 * Created by IntelliJ IDEA.
 * User: andy
 * Date: 7/12/11
 * Time: 8:44 AM
 * To change this template use File | Settings | File Templates.
 */

class CordicTest {

}

object CordicTest {


  object TestCase extends Module("alpha") {

     implicit val n = assignClock(ClockControl("clk","reset"))


     val stages = 8

     val signal              = complex("signal_in",INPUT,S(8,6))
     val angle               = signal ("angle_in",INPUT,S(8,8))

     val signal_out          = complex("signal_out",OUTPUT,S(8,6))

     val signal_internal     = array("signal_internal",WIRE,S(8,6))(stages)
     val angle_internal      = array("angle_internal",WIRE,S(8,6))(stages)

  }





  def main(args:Array[String]) = {

     val mod = TestCase.createModule
     System.out.println(mod.createCode(CodeWriter.Verilog))



  }
}